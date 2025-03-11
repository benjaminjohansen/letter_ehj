require(ISLR2)
require(gt)
require(tibble)
require(dplyr)

# variables ---------------------------------------------------------------
# Set to 20 as median = 19.17 and mean = 24.72
follow_up_time <- 20

# Baseline scenario
# dataset
data_baseline <- ISLR2::Publication
# Number of participants
n_total <- base::nrow(data_baseline)
# Number of independent variables
n_variables <- base::ncol(data) - 2

# Helper functions --------------------------------------------------------
# Function to calculate number of events per variable
events_per_variable <- function(events_vector, n_variables) {
  n_positive <- base::sum(events_vector)
  return(n_positive / n_variables)
}

# Function to calculate censoring rate
cens_rate <- function(events_vector, n_participants) {
  n_events <- base::sum(events_vector)
  n_censored <- n_participants - n_events
  cens_rate <- n_censored / n_participants
  return(cens_rate)
}

# Incidence rate
inc_rate <- function(events_vector, time_vector) {
  person_time <- base::sum(time_vector)
  n_events <- base::sum(events_vector)
  return(n_events / person_time)
}

# Data preprocess for scenarios -------------------------------------------

# Update data to reflect the censoring at follow up time
# This is the true baseline for the study period!
data <- data_baseline |>
  dplyr::mutate(
    status_cens = ifelse((time <= follow_up_time & status == 0) | time > follow_up_time, 0, 1)
  )

# Scenario A, cut off time
a <- data

# Scenarion B, Remove censored rows during cut off
b <- data |>
  # Remove all rows that are censored
  dplyr::filter(
    !(time < follow_up_time & status == 0)
  )

# Scenario C, ignore cenosring completely
c <- data_baseline |>
  dplyr::mutate(
    status_cens = status
  )

# Scenario d, ignore censoring after follow up time
d <- c |>
  dplyr::filter(
    !(time < follow_up_time & status == 0)
  )


# calculate differences ---------------------------------------------------
# Incidence rates
inc_a <- inc_rate(events_vector = a$status_cens, time_vector = a$time)
inc_b <- inc_rate(events_vector = b$status_cens, time_vector = b$time)
inc_c <- inc_rate(events_vector = c$status_cens, time_vector = c$time)
inc_d <- inc_rate(events_vector = d$status_cens, time_vector = d$time)

# Censoring rate
cens_a <- cens_rate(events_vector = a$status_cens, n_participants = base::length(a$status_cens))
cens_b <- cens_rate(events_vector = b$status_cens, n_participants = base::length(b$status_cens))
cens_c <- cens_rate(events_vector = c$status_cens, n_participants = base::length(c$status_cens))
cens_d <- cens_rate(events_vector = d$status_cens, n_participants = base::length(d$status_cens))

# Calculate number of events for each scenario
EPV_a <- events_per_variable(a$status_cens, n_variables)
EPV_b <- events_per_variable(b$status_cens, n_variables)
EPV_c <- events_per_variable(c$status_cens, n_variables)
EPV_d <- events_per_variable(d$status_cens, n_variables)

events_baseline <- sum(data$status)
events_a <- sum(a$status_cens)
events_b <- sum(b$status_cens)
events_c <- sum(c$status_cens)

# Proportion of events to no events
prob_a <- sum(a$status_cens) / length(a$status_cens)
prob_b <- sum(b$status_cens) / length(b$status_cens)
prob_c <- sum(c$status_cens) / length(c$status_cens)
prob_d <- sum(d$status_cens) / length(d$status_cens)

output <- tibble::tibble(
  scenario = c("A", "B", "C", "D"),
  explanation = c("Account for censoring", "A and remove rows lost to follow up", "Ignore censoring", "C + remove rows lost to follow up"),
  incidence = c(inc_a, inc_b, inc_c, inc_d),
  censoring = c(cens_a, cens_b, cens_c, cens_d),
  EPV = c(EPV_a, EPV_b, EPV_c, EPV_d),
  events_no_events = c(prob_a, prob_b, prob_c, prob_d),
  cens = c("Account for censoring", "Account for censoring", "Ignore censoring", "Ignore censoring")
)

gt_tbl <- gt::gt(
  output[1:3, ],
  rowname_col = "scenario",
  groupname_col = "cens") |>
  gt::tab_header(
    title = "Different scenarios for converting survival data into binary data",
    subtitle = "Based on a follow up time at 20 months"
  ) |>
  cols_label(
    explanation = "Scenario",
    incidence = "Incidence rate (per person month)",
    censoring = "Censoring rate (proportion of censored events)",
    EPV = "Events per variable",
    events_no_events = "Ratio of Events/No events (censored)"
  ) |>
  gt::fmt_number(
    decimals = 3,
drop_trailing_zeros = TRUE
  ) |>
  gt::tab_source_note(
    source_note = md("Source: Gordon, Taddei-Peters, Mascette, Antman, Kaufmann, and Lauer. Publication of trials funded by the National Heart, Lung, and Blood Institute. New England Journal of Medicine, 369(20):1926-1934, 2013")
  ) |>
  gt::tab_source_note(
    source_note = md("Reference: James, G., Witten, D., Hastie, T., and Tibshirani, R. (2021) An Introduction to Statistical Learning with applications in R, Second Edition, https://www.statlearning.com, Springer-Verlag, New York")
  )


gt_tbl

gt::gtsave(gt_tbl, filename = "results_table.png", path = base::paste(base::getwd(), "output", sep = "/"))
