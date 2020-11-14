
library(dplyr)
library(plotly)

# logifySlider javascript function
JS.logify <-
  "
// function to logify a sliderInput
function logifySlider (sliderId, sci = false) {
    // regular number style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return (Math.pow(10, num)); }
    })
}"

# call logifySlider for each relevant sliderInput
JS.onload <-
  "
// execute upon document loading
$(document).ready(function() {
  // wait a few ms to allow other scripts to execute
  setTimeout(function() {
    // include call for each slider
    logifySlider('n', sci = true)
  }, 5)})
"
# returns a dataset for a normal distribution of a specified class label
create_distribution_data <- function(mean, sd, class) {
  
  x <- seq(from = mean - 4*sd,
           to = mean + 4*sd,
           by = 0.05) 
  
  dist <- as.data.frame(x) %>% 
    mutate(class = class,
           mean = mean,
           sd = sd) %>% 
    mutate(y = dnorm(x, mean = mean, sd = sd))
  
  return(dist)
  
}

create_theoretical_dist_data <- function(pos_mean, neg_mean, pos_sd, neg_sd) {
  positive_exact <- create_distribution_data(pos_mean, pos_sd, "positive")
  negative_exact <- create_distribution_data(neg_mean, neg_sd, "negative")
  return(list(
    positive_exact,
    negative_exact
  ))
}

func_threshold_range <- function(pos_mean, neg_mean, pos_sd, neg_sd) {
  min = min(pos_mean - 4*pos_sd, neg_mean - 4*neg_sd)
  max = max(pos_mean + 4*pos_sd, neg_mean + 4*neg_sd)
  return(c(min, max))
}

# Generates all the empirical data for all the other functions
create_empirical_data <- function(n, p, pos_mean, neg_mean, pos_sd, neg_sd) {
  
  outcome <- rbinom(n = (10^n), size = 1, prob = p)
  
  n_pos <- nrow(data.frame(outcome) %>% filter(outcome == 1))
  n_neg <- nrow(data.frame(outcome) %>% filter(outcome == 0))
  
  pos_val <- rnorm(n_pos, mean = pos_mean, sd = pos_sd)
  neg_val <- rnorm(n_neg, mean = neg_mean, sd = neg_sd)
  
  return(list(
    pos_val,
    neg_val,  
    outcome
  ))
  
}

# data <- create_empirical_data(2, 0.5, 10, 8, 2, 2)

create_empirical_ROC_data <- function(empirical_data) {
  
  pos_val <- empirical_data[[1]]
  neg_val <- empirical_data[[2]]
  outcome <- empirical_data[[3]]
  
  x <- bind_rows(as.data.frame(pos_val) %>% rename(x = pos_val), as.data.frame(neg_val) %>% rename(x = neg_val))
  
  data <- data.frame(outcome) %>% 
    arrange(desc(outcome)) %>% 
    bind_cols(x)
  
  # fit a logistic regression to the empirical data
  model <- glm(data = data, outcome ~ x, family = "binomial")
  predictions <- predict(model, newdata = data, type = "response")
  scores <- predict(model, newdata = data, type = "link")
  
  empirical_roc <- bind_cols(data, as.data.frame(predictions), as.data.frame(scores)) %>% 
    select(outcome, predictions, scores) %>% 
    group_by(predictions, scores) %>% 
    summarise(
      negatives = sum(ifelse(outcome == 0, 1, 0)),
      positives = sum(ifelse(outcome == 1, 1, 0)),
      total = n()
    ) %>% 
    arrange(desc(predictions), desc(scores)) %>%
    ungroup() %>% 
    mutate(
      total_positives = sum(positives),
      total_negatives = sum(negatives),
      true_positives = cumsum(positives),
      true_negatives = total_negatives - cumsum(negatives)
    ) %>%
    mutate(
      TPR = true_positives/total_positives,
      TNR = true_negatives/total_negatives,
      FPR = 1 - TNR,
      FNR = 1 - TPR,
      FPR_lag = lag(FPR),
      TPR_lag = lag(TPR),
      F1 = 2*TPR/(2*TPR+FPR+FNR),
      area = (FPR - FPR_lag)*(TPR+TPR_lag)/2
    )
  
  return(empirical_roc)
  
}

# create_empirical_ROC_data(data)

create_theoretical_ROC_data <- function(pos_mean, neg_mean, pos_sd, neg_sd) {
  
  positive.min <- pos_mean - 4*pos_sd
  positive.max <- pos_mean + 4*pos_sd
  negative.min <- neg_mean - 4*neg_sd
  negative.max <- neg_mean + 4*neg_sd
  
  x <- seq(from = min(negative.min, positive.min),
           to = max(negative.max, positive.max),
           by = 0.05)
  
  ROC_data <- as.data.frame(x) %>% 
    arrange(desc(x)) %>% 
    mutate(TPR = pnorm(x, pos_mean, pos_sd, lower.tail = FALSE),
           FNR = 1 - TPR,
           TNR = pnorm(x, neg_mean, neg_sd),
           FPR = 1 - TNR,
           FPR_lag = lag(FPR),
           TPR_lag = lag(TPR),
           F1 = 2*TPR/(2*TPR+FPR+FNR),
           area = (FPR - FPR_lag)*(TPR+TPR_lag)/2)
  
  return(ROC_data)

}

# create_theoretical_ROC_data(10, 8, 2, 2)


plot_ROC <- function(theoretical_ROC_data, empirical_data, threshold) {
  
  emp <- create_empirical_ROC_data(empirical_data)
  
  ROC_FPR_threshold <- theoretical_ROC_data %>% 
    filter(x == threshold) %>% 
    select(FPR) %>% 
    as.numeric()
  
  opt_threshold <- optimal_threshold(theoretical_ROC_data)

  # ROC plot
  plot_ly(data = theoretical_ROC_data, x = ~FPR, y = ~TPR, type = "scatter", mode = "lines") %>% 
    add_trace(x = emp$FPR, y = emp$TPR, type = "scatter", mode = "lines") %>% 
    add_segments(x = ROC_FPR_threshold, xend = ROC_FPR_threshold, y = 0, yend = 1, line = list(color = "black"), name = "threshold") %>% 
    add_trace(x = opt_threshold$FPR, y = opt_threshold$TPR, type = "scatter", mode = "markers", marker = list(size = 10, color = "black"))
  
}

plot_distributions <- function(theoretical_data, empirical_data, threshold) {
  
  positive_exact <- theoretical_data[[1]]
  negative_exact <- theoretical_data[[2]]
  
  pos_val <- empirical_data[[1]]
  neg_val <- empirical_data[[2]]
  
  plot_ly() %>%
    add_histogram(x = pos_val, histnorm = "probability density", marker = list(color = "green"), opacity = 0.3, name = "positive (sampled)") %>%
    add_histogram(x = neg_val, histnorm = "probability density", marker = list(color = "orange"), opacity = 0.3, name = "negative (sampled)") %>%
    add_trace(x = positive_exact$x, y = positive_exact$y, type = "scatter", mode = "lines", line = list(color = 'green'), name = "positive") %>%
    add_trace(x = negative_exact$x, y = negative_exact$y, type = "scatter", mode = "lines",  line = list(color = 'orange'), name = "negative") %>% 
    add_segments(x = threshold, xend = threshold, y = 0, yend = 1.2*max(positive_exact$y, negative_exact$y), line = list(color = "black"), name = "threshold") %>% 
    layout(barmode = "overlay",
           yaxis = list(showgrid = FALSE))
  
}

# for the following we only calculate the theoretical versions

calc_auc <- function(theoretical_ROC_data) {
  auc <- sum(theoretical_ROC_data$area, na.rm = TRUE)
  return(auc)
}

calc_gini <- function(theoretical_ROC_data) {
  auc <- sum(theoretical_ROC_data$area, na.rm = TRUE)
  return(2*auc - 1)
}

# Optimal threshold is where F1 is closest to 1
optimal_threshold <- function(theoretical_ROC_data) {
  optimal_threshold <- theoretical_ROC_data %>% 
    arrange(desc(F1)) %>% 
    head(1)
  return(optimal_threshold)
}

accuracy <- function(pos_mean, neg_mean, pos_sd, neg_sd, threshold) {
  TPR <- pnorm(threshold, pos_mean, pos_sd, lower.tail = FALSE)
  TNR <- pnorm(threshold, neg_mean, neg_sd)
  accuracy <- (TPR + TNR)/(2)
  return(accuracy)
}

# accuracy(3, 2, 1, 1, 1.3)

result_table <- function(auc, gini, accuracy) {
  Parameter <- c("AUC", "Gini", "Accuracy")
  Value <- c(round(auc,2), round(gini,2), round(accuracy,2))
  table <- data.frame(
    Parameter,
    Value
  )
  return(table)
}

