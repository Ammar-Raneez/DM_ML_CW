library(tidyverse)
library(maltese)
library(neuralnet)
library(dummy)

data("r_enwiki", package = "maltese")
head(r_enwiki)

ggplot(r_enwiki, aes(x = date, y = pageviews)) + 
  geom_line() + 
  theme_minimal() + 
  labs(x = "Date", y = "Pageviews",
       title = "Pageviews"
       )

split_point <- "2017-02-01"
table(ifelse(r_enwiki$date < split_point, "training set", "testing set"))

normalization_constants <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = r_enwiki$pageviews[r_enwiki$date < split_point])
)
r_enwiki$normalized <- (r_enwiki$pageviews - normalization_constants$mean)/normalization_constants$std.dev

mlts <- mlts_transform(r_enwiki, date, normalized, p = 21, extras = TRUE, extrasAsFactors = TRUE, granularity = "day")
str(mlts)


mlts_categories <- categories(mlts[, c("mlts_extras_weekday", "mlts_extras_month", "mlts_extras_monthday", "mlts_extras_week"), drop = FALSE])
mlts_dummied <- cbind(mlts, dummy(
  mlts[, c("mlts_extras_weekday", "mlts_extras_month", "mlts_extras_monthday", "mlts_extras_week"),
       drop = FALSE],
  object = mlts_categories, int = TRUE
))
str(mlts_dummied, list.len = 30)


training_idx <- which(mlts_dummied$dt < split_point)
testing_idx <- which(mlts_dummied$dt >= split_point)


# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features <- grep("(mlts_lag_[0-9]+)|(mlts_extras_((weekday)|(month)|(monthday)|(week))_.*)", names(mlts_dummied), value = TRUE)
nn_formula <- as.formula(paste("y ~", paste(nn_features, collapse = " + ")))

# Train:
set.seed(0)
nn_model <- neuralnet(
  nn_formula, mlts_dummied[training_idx, c("y", nn_features)],
  linear.output = TRUE, hidden = c(9, 7, 5), algorithm = "sag"
)


new_data <- rbind(
  tail(r_enwiki, 22),
  data.frame(
    date = seq(
      as.Date("2017-05-17"),
      as.Date("2017-05-17") + 90,
      "day"
    ),
    pageviews = NA,
    normalized = NA
  )
); rownames(new_data) <- NULL

for (d in 23:nrow(new_data)) {
  new_mlts <- mlts_transform(
    new_data[(d - 22):d, ],
    date, normalized, p = 21,
    extras = TRUE, extrasAsFactors = TRUE,
    granularity = "day")
  new_mlts <- cbind(
    new_mlts[-1, ], # don't need to forecast known outcome
    dummy(
      new_mlts[, c("mlts_extras_weekday", "mlts_extras_month", "mlts_extras_monthday", "mlts_extras_week"),
               drop = FALSE],
      object = mlts_categories, int = TRUE
    )[-1, ]
  )
  new_data$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model, new_mlts[, nn_features])$net.result
  )
}

# Forecast on normalized scale:
nn_forecast <- as.numeric(neuralnet::compute(
  nn_model, new_mlts[, nn_features])$net.result
)

new_data$pageviews <- (new_data$normalized * normalization_constants$std.dev) + normalization_constants$mean

ggplot(dplyr::filter(r_enwiki, date >= "2016-10-01"),
       aes(x = date, y = pageviews)) +
  geom_line() +
  geom_line(aes(y = pageviews), color = "red",
            data = dplyr::filter(new_data, date >= "2017-05-16")) +
  theme_minimal() +
  labs(x = "Date", y = "Pageviews",
       title = "90 days forecast Pageviews"
  )
