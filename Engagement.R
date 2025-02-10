#' GOAL
# Just the code used to analyze and visualize the Engagement section of the report, and conforming to
# "getting started" Template format and lintr
# SET UP

rm(list = ls())
set.seed(12)

#lint_this() # nolint

# LOAD PACKAGES
library(cowplot)
library(scales)
library(plotly)
library(gridExtra)
library(tidyverse)
library(devtools)
library(oefenwebDatabase)
library(oefenwebTools)
library(lme4)
library(lmerTest)
library(lubridate)
library(DBI)
library(ggalluvial)
library(survival)

# DATABASE CONNECTIONS
# close any existing connections
invisible(oefenwebDatabase::close_connections())
# Establish new connection with oefenweb database
con <- oefenwebDatabase::connect()


#### LOAD DATA ####
ed_logs <- list()
domains <- c(1:5, 7, 9, 10, 11, 59)

# Loop through each E_D table
for (i in 1:10) {
  # Write SQL query to get data, and convert to seconds
  query <- paste0("SELECT *
                   FROM extended_deadline_logs_", domains[i])
  # Get the data and store it in the list
  ed_logs[[i]] <- suppressWarnings(DBI::dbGetQuery(con, query))
}

# Turn the list into a single df
ed_logs <- bind_rows(ed_logs)

# Getting deadlines for items in 59
query <- paste0("SELECT id AS item_id,
                 maximum_response_in_seconds AS deadline
                 FROM extended_deadline_items
                 WHERE domain_id = 59")
# Get the data and store it in the list
deadlines_59 <- suppressWarnings(DBI::dbGetQuery(con, query))


#### DATA PROCESSING ####
# Create necessary objects
change_date <- as.Date("2024-10-25")
start_date <- as.Date("2023-09-01")

# Define a consistent theme and colors for all plots
custom_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 10)
  )

custom_colors <- c("Before" = "#00BFC4", "After" = "#F8766D") # Adjust colors as needed

# Convert created to date and into POSIXct format
ed_logs <- ed_logs %>%
  rename(date = created)
ed_logs$date <-
  as.POSIXct(ed_logs$date, format = "%Y-%m-%d %H:%M:%S")

# Getting item deadlines for domain 59
items_5 <- deadlines_59 %>%
  filter(deadline == 5) %>%
  pull(item_id)

items_10 <- deadlines_59 %>%
  filter(deadline == 10) %>%
  pull(item_id)

items_15 <- deadlines_59 %>%
  filter(deadline == 15) %>%
  pull(item_id)

items_20 <- deadlines_59 %>%
  filter(deadline == 20) %>%
  pull(item_id)

# Categorical columns
cats <- colnames(ed_logs[, c(8:11, 14:15)])

# Adjustments to data
ed_logs <- ed_logs %>%
  rename(response_in_seconds = response_in_milliseconds) %>%
  mutate(
    deadline = case_when(
      domain_id %in% c(1:4, 7, 10) ~ 20,
      domain_id == 9 ~ 30,
      domain_id == 11 ~ 60,
      domain_id == 5 ~ 8,
      domain_id == 59 & item_id %in% items_5 ~ 5,
      domain_id == 59 & item_id %in% items_10 ~ 10,
      domain_id == 59 & item_id %in% items_15 ~ 15,
      domain_id == 59 & item_id %in% items_20 ~ 20
    ),
    response_in_seconds = response_in_seconds / 1000,
    across(all_of(cats), as.factor)
  )
# Adding time_weeks column and organizing by user_id then date
ed_logs <- ed_logs %>%
  group_by(user_id) %>%
  arrange(user_id, date) %>%
  mutate(time_weeks = ceiling(as.numeric(
    difftime(date, start_date, units = "weeks")
  ))) %>%
  mutate(
    time_weeks = ifelse(time_weeks < 1, 1, time_weeks),
    late_response = ifelse(response_in_seconds > deadline, 1, 0)
  ) %>% # Ensure any negative or 0 values are set to 1
  ungroup()


### EGAGEMENT SPECIFIC DATA PROCESSING ###

# Add a new variable indicating whether the response was late
# late_response: 1 indicates late response, 0 indicates on-time response
ed_logs <- ed_logs %>%
  mutate(late_response = ifelse(response_in_seconds > deadline, 1, 0))

# Show the earliest and latest date in the dataset
earliest_date <- min(ed_logs$date, na.rm = TRUE)
latest_date <- max(ed_logs$date, na.rm = TRUE)
print(paste("Earliest date:", earliest_date))
print(paste("Latest date:", latest_date))

# Split the data into before and after October 25, 2024
split_date <- as.Date("2024-10-25")
# Filter records after the split date
ed_logs_after <- ed_logs %>%
  filter(date > split_date)

## Data Preparation
data <- ed_logs %>%
  mutate(grade = as.numeric(as.character(grade))) %>% # Convert grade from factor to numeric
  filter(grade >= 3 & grade <= 8) # Keep only grades between 3 and 8

# Define the range
start_date <- as.Date("2024-09-05")
end_date <- as.Date("2024-12-14")

# Filter the data
data <- subset(data, date >= start_date & date <= end_date)

# Add the 'help' column
data$help <- ifelse(data$answer == "¿", 1, 0)

# Add the 'no_answer' column
data$no_answer <- ifelse(data$answer == "…", 1, 0)

# Add the 'learning_goal' column
data$learning_goal <- ifelse(data$session == "learning_goal", 1, 0)

# Function to calculate the mode
calculate_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

# Add a column for mode of difficulty based on user_id
data <- data %>%
  group_by(user_id) %>%
  mutate(modeDifficulty = calculate_mode(difficulty)) %>%
  ungroup()

# Define the reference date
reference_date <- as.Date("2024-10-25")

# Add the 'new_UI' column
data$new_UI <- factor(
  ifelse(data$date > reference_date, "After", "Before"), levels = c("Before", "After")
)


# Map domain IDs to domain names
domain_names <- c(
  "1" = "Addition",
  "2" = "Subtraction",
  "3" = "Multiplication",
  "4" = "Division",
  "5" = "Mix",
  "7" = "Counting",
  "9" = "Clock",
  "10" = "Series",
  "11" = "Numerals",
  "59" = "Tables"
)

# Add names to domain_id
data$domain_name <- domain_names[as.character(data$domain_id)]

#### DATA ANALYSIS ####
# Check the structure and unique values in domain_id
str(ed_logs)
unique(ed_logs$domain_id)

# group by new UI and calculate mean and sd of no answer
group_summary_no_answer <- data %>%
  group_by(new_UI) %>%
  summarise(
    mean_no_answer = mean(no_answer, na.rm = TRUE),
    sd_no_answer = sd(no_answer, na.rm = TRUE),
    n = n()
  )
# Prepare the data
test_data_no_answer <- data %>%
  group_by(new_UI, user_id) %>% # Assuming individual-level responses
  summarise(
    no_answer_mean = mean(no_answer, na.rm = TRUE) # Mean no_answer per student
  ) %>%
  ungroup()

# Perform an independent t-test
t_test_result_no_answer <- t.test(
  no_answer_mean ~ new_UI, # Compare "Before" vs "After"
  data = test_data_no_answer,
  var.equal = FALSE # Use Welch's t-test (default), which is robust to unequal variances
)

# Output the results
cat("T-Test Results:\n")
print(t_test_result_no_answer)

# Define a function to perform t-tests within subgroups
sg_t_test_no_answer <- function(data, subgroup_var) {
  data %>%
    group_by(!!sym(subgroup_var)) %>% # Group by the subgroup variable
    summarise(
      t_test_result_no_answer = list(t.test(no_answer ~ new_UI, data = cur_data(), var.equal = FALSE)),
      .groups = "drop"
    ) %>%
    mutate(
      subgroup = !!sym(subgroup_var),
      t_statistic = map_dbl(t_test_result_no_answer, ~ .x$statistic),
      p_value = map_dbl(t_test_result_no_answer, ~ .x$p.value),
      mean_diff = map_dbl(t_test_result_no_answer, ~ diff(.x$estimate)),
      conf_low = map_dbl(t_test_result_no_answer, ~ .x$conf.int[1]),
      conf_high = map_dbl(t_test_result_no_answer, ~ .x$conf.int[2])
    ) %>%
    select(subgroup, t_statistic, p_value, mean_diff, conf_low, conf_high)
}

# Apply the function for each subgroup variable
(results_by_domain_no_answer <- sg_t_test_no_answer(data, "domain_id"))
(results_by_grade_no_answer <- sg_t_test_no_answer(data, "grade"))
(results_difficulty_no_answer <- sg_t_test_no_answer(data, "modeDifficulty"))
(results_by_learning_no_answer <- sg_t_test_no_answer(data, "learning_goal"))

# Summarize the data: Calculate the mean and standard deviation of "help" usage before and after the new UI
help_summary <- data %>%
  group_by(new_UI) %>%
  summarise(
    mean_help = mean(help, na.rm = TRUE), # Average "help" usage
    sd_help = sd(help, na.rm = TRUE), # Standard deviation of "help" usage
    n = n() # Sample size
  )

# Perform an independent t-test to test for differences in "help" usage
t_test_help <- t.test(
  help ~ new_UI, # Test if "help" differs between "Before" and "After"
  data = data,
  var.equal = FALSE # Use Welch's t-test by default
)

# Output results
cat("Summary of Help Usage by New UI:\n")
print(help_summary)
cat("\nT-Test Results:\n")
print(t_test_help)


# Define a function to perform t-tests for subgroups
perform_subgroup_t_test_help <- function(data, subgroup_var) {
  data %>%
    group_by(!!sym(subgroup_var)) %>% # Group by the subgroup variable
    summarise(
      t_test_result_help = list(t.test(help ~ new_UI, data = cur_data(), var.equal = FALSE)), # Perform t-test
      .groups = "drop"
    ) %>%
    mutate(
      subgroup = !!sym(subgroup_var),
      t_statistic = map_dbl(t_test_result_help, ~ .x$statistic),
      p_value = map_dbl(t_test_result_help, ~ .x$p.value),
      mean_diff = map_dbl(t_test_result_help, ~ diff(.x$estimate)),
      conf_low = map_dbl(t_test_result_help, ~ .x$conf.int[1]),
      conf_high = map_dbl(t_test_result_help, ~ .x$conf.int[2])
    ) %>%
    select(subgroup, t_statistic, p_value, mean_diff, conf_low, conf_high)
}

# Apply the function for each subgroup variable
(results_by_domain_help <- perform_subgroup_t_test_help(data, "domain_id"))
(results_by_grade <- perform_subgroup_t_test_help(data, "grade"))
(results_by_difficulty <- perform_subgroup_t_test_help(data, "modeDifficulty"))
(results_by_learning_goal <- perform_subgroup_t_test_help(data, "learning_goal"))


# Compute percentage of correct answers per domain
correct_answers_domain <- data %>%
  group_by(new_UI, domain_id) %>%
  summarise(
    total_responses = n(),
    correct_responses = sum(correct_answered == "1"),
    correct_percentage = mean(correct_answered == "1") * 100,
    .groups = "drop"
  ) %>%
  mutate(
    UI_Status = new_UI, # Consistent with "Before" and "After"
    domain_name = domain_names[as.character(domain_id)]
  )

# Compute percentage of correct answers per grade
correct_answers_grade <- data %>%
  group_by(new_UI, grade) %>%
  summarise(
    total_responses = n(),
    correct_responses = sum(correct_answered == "1"),
    correct_percentage = mean(correct_answered == "1") * 100,
    .groups = "drop"
  ) %>%
  mutate(UI_Status = new_UI)

# Compute percentage of correct answers per difficulty
correct_answers_difficulty <- data %>%
  group_by(new_UI, modeDifficulty) %>%
  summarise(
    total_responses = n(),
    correct_responses = sum(correct_answered == "1"),
    correct_percentage = mean(correct_answered == "1") * 100,
    .groups = "drop"
  ) %>%
  mutate(
    UI_Status = new_UI,
    Difficulty = factor(
      case_when(
        modeDifficulty == "0" ~ "Easy",
        modeDifficulty == "1" ~ "Medium",
        modeDifficulty == "2" ~ "Hard"
      ),
      levels = c("Easy", "Medium", "Hard") # Set the correct order
    )
  )

# Compute percentage of correct answers for learning goal
correct_answers_learning <- data %>%
  group_by(new_UI, learning_goal) %>%
  summarise(
    total_responses = n(),
    correct_responses = sum(correct_answered == "1"),
    correct_percentage = mean(correct_answered == "1") * 100,
    .groups = "drop"
  ) %>%
  mutate(
    UI_Status = new_UI,
    LearningGoal = factor(ifelse(learning_goal == 1, "Yes", "No"), levels = c("No", "Yes"))
  )


# Convert `correct_answered` to numeric
data$correct_answered <- as.numeric(as.character(data$correct_answered))

# Overall t-test for correct answers (Before vs After)
overall_t_test_correct <- t.test(correct_answered ~ new_UI, data = data, var.equal = FALSE)

# Summarize overall mean and SD by new_UI
overall_summary_correct <- data %>%
  group_by(new_UI) %>%
  summarise(
    mean_correct = mean(correct_answered, na.rm = TRUE) * 100, # Mean in percentage
    sd_correct = sd(correct_answered, na.rm = TRUE) * 100, # Standard deviation in percentage
    n = n() # Sample size
  )

# Print overall results
cat("Overall Summary:\n")
print(overall_summary_correct)
cat("\nOverall T-Test Results:\n")
print(overall_t_test_correct)

# Define a function to test for significance by subgroup
perform_test_correct <- function(data, group_var) {
  data %>%
    group_by(!!sym(group_var)) %>% # Group by the specified variable
    summarise(
      t_test_result_correct = list(t.test(correct_answered ~ new_UI, data = cur_data(), var.equal = FALSE)),
      .groups = "drop"
    ) %>%
    mutate(
      subgroup = !!sym(group_var),
      t_statistic = map_dbl(t_test_result, ~ .x$statistic),
      p_value = map_dbl(t_test_result_correct, ~ .x$p.value),
      mean_diff = map_dbl(t_test_result_correct, ~ diff(.x$estimate) * 100), # Mean difference in percentage
      conf_low = map_dbl(t_test_result_correct, ~ .x$conf.int[1] * 100),
      conf_high = map_dbl(t_test_result_correct, ~ .x$conf.int[2] * 100)
    ) %>%
    select(subgroup, t_statistic, p_value, mean_diff, conf_low, conf_high)
}

# Test significance for each subgroup
(results_by_domain_correct <- perform_test_correct(data, "domain_id"))
(results_by_grade_correct <- perform_test_correct(data, "grade"))
(results_by_difficulty_correct <- perform_test_correct(data, "modeDifficulty"))
(results_by_learning_correct <- perform_test_correct(data, "learning_goal"))

# Total items answered
items_answered <- data %>%
  group_by(new_UI) %>%
  summarise(
    total_items = n(),
    .groups = "drop"
  )
(t_test_items <- t.test(item_id ~ new_UI, data = data))

# Average response time and significance test
response_time <- data %>%
  group_by(new_UI) %>%
  summarise(
    avg_response_time = mean(response_in_seconds, na.rm = TRUE),
    sd_response_time = sd(response_in_seconds, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
(t_test_response_time <- t.test(response_in_seconds ~ new_UI, data = data))

# Average daily unique users and significance test
daily_unique_users <- data %>%
  group_by(new_UI) %>%
  summarise(
    avg_daily_users = mean(n_distinct(user_id), na.rm = TRUE),
    sd_daily_users = sd(n_distinct(user_id), na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
(t_test_daily_users <- t.test(
  daily_users ~ new_UI,
  data = data %>%
    group_by(new_UI, date) %>%
    summarise(daily_users = n_distinct(user_id), .groups = "drop")
))

# Average daily sessions per user and significance test
daily_sessions_per_user <- data %>%
  group_by(new_UI, date) %>%
  summarise(
    total_sessions = n(),
    unique_users = n_distinct(user_id),
    avg_sessions_per_user = total_sessions / unique_users,
    .groups = "drop"
  ) %>%
  group_by(new_UI) %>%
  summarise(
    avg_sessions_per_user = mean(avg_sessions_per_user, na.rm = TRUE),
    sd_sessions_per_user = sd(avg_sessions_per_user, na.rm = TRUE),
    n_days = n(), # Number of days
    .groups = "drop"
  )
(t_test_sessions_per_user <- t.test(
  avg_sessions_per_user ~ new_UI,
  data = data %>%
    group_by(new_UI, date) %>%
    summarise(
      total_sessions = n(),
      unique_users = n_distinct(user_id),
      avg_sessions_per_user = total_sessions / unique_users,
      .groups = "drop"
    )
))

# Combine results
descriptives <- items_answered %>%
  left_join(response_time, by = "new_UI") %>%
  left_join(daily_unique_users, by = "new_UI") %>%
  left_join(daily_sessions_per_user, by = "new_UI")

# Print the final descriptive statistics
print(descriptives)

# Ensure `date` is in POSIXct format
data$date <- as.POSIXct(data$date)

# Add `hour` column to the data
data <- data %>%
  mutate(hour = hour(date)) # Extract hour from the date column

# Filter data for "After" the new UI and exclude rows where no_answer = 1 and late_response = 1
filtered_data <- data %>%
  filter(new_UI == "After" & !(late_response == 1 & no_answer == 1))

# Late Responses by Hour of the Day
late_response_by_hour <- filtered_data %>%
  group_by(hour) %>%
  summarize(
    total_responses = n(),
    late_responses = sum(late_response),
    late_response_rate = round(mean(late_response) * 100, 2),
    .groups = "drop"
  )

# Late Responses by Weekday
late_response_by_weekday <- filtered_data %>%
  group_by(weekday = wday(date, label = TRUE, abbr = FALSE)) %>%
  summarize(
    total_responses = n(),
    late_responses = sum(late_response),
    late_response_rate = round(mean(late_response) * 100, 2),
    .groups = "drop"
  )

# Calculate the reference (minimum) date
global_min <- min(filtered_data$date)

# Group by the computed week number
late_response_by_week <- filtered_data %>%
  group_by(week_since_ui = as.integer(difftime(date, global_min, units = "weeks")) + 1) %>%
  summarize(
    total_responses    = n(),
    late_responses     = sum(late_response),
    late_response_rate = round(mean(late_response) * 100, 2),
    .groups            = "drop"
  )

# ANOVA for Late Response Rate by Hour
anova_hour <- aov(late_response ~ as.factor(hour), data = filtered_data)
cat("ANOVA for Late Response Rate by Hour:\n")
summary(anova_hour)

# ANOVA for Late Response Rate by Weekday
anova_weekday <- aov(late_response ~ as.factor(wday(date)), data = filtered_data)
cat("\nANOVA for Late Response Rate by Weekday:\n")
summary(anova_weekday)

# Create the week_since_ui variable
filtered_data <- filtered_data %>%
  mutate(week_since_ui = floor(as.numeric(difftime(date, min(date), units = "weeks"))))


# ANOVA for Late Response Rate by Week Since New UI
anova_week <- aov(late_response ~ as.factor(week_since_ui), data = filtered_data)
cat("\nANOVA for Late Response Rate by Week Since New UI:\n")
summary(anova_week)

# Post-hoc Analysis for Late Response Rate by Hour
tukey_hour <- TukeyHSD(anova_hour)
cat("Post-hoc Analysis for Late Response Rate by Hour:\n")
print(tukey_hour)

# Post-hoc Analysis for Late Response Rate by Weekday
tukey_weekday <- TukeyHSD(anova_weekday)
cat("\nPost-hoc Analysis for Late Response Rate by Weekday:\n")
print(tukey_weekday)

# Post-hoc Analysis for Late Response Rate by Week Since New UI
tukey_week <- TukeyHSD(anova_week)
cat("\nPost-hoc Analysis for Late Response Rate by Week Since New UI:\n")
print(tukey_week)

# Calculate total responses and late responses by hour
responses_by_hour <- filtered_data %>%
  group_by(hour) %>%
  summarize(
    total_responses = n(),
    late_responses = sum(late_response),
    late_response_rate = round(mean(late_response) * 100, 2),
    .groups = "drop"
  )

# Calculate total responses and late responses by weekday
responses_by_weekday <- filtered_data %>%
  mutate(weekday = wday(date, label = TRUE)) %>%
  group_by(weekday) %>%
  summarize(
    total_responses = n(),
    late_responses = sum(late_response),
    late_response_rate = round(mean(late_response) * 100, 2),
    .groups = "drop"
  )


# Filter data for domain = 59
data_domain_59 <- data %>% filter(domain_id == 59)

# Calculate the percentage of late_responses that are not no_responses per deadline compared to total responses
result_domain_59 <- data_domain_59 %>%
  group_by(deadline) %>%
  summarise(
    total_responses = n(), # Total number of responses
    total_late_responses = sum(late_response == TRUE), # Total late responses
    non_no_responses = sum(late_response == TRUE & no_answer == FALSE), # Late but not no responses
    percentage_non_no_responses = (non_no_responses / total_responses) * 100 # Percentage compared to total responses
  )


# Calculate the percentage of no_answer responses before new_UI (1 vs 0) per deadline
result_no_answer_domain_59 <- data_domain_59 %>%
  group_by(deadline, new_UI) %>%
  summarise(
    total_responses = n(), # Total number of responses
    total_no_answer = sum(no_answer == TRUE), # Total no_answer responses
    percentage_no_answer = (total_no_answer / total_responses) * 100 # Percentage of no_answer
  )



# Calculate the percentage of correct responses before and after the new UI (1 vs 0) per deadline
correct_answered_domain_59 <- data_domain_59 %>%
  group_by(deadline, new_UI) %>%
  summarise(
    total_responses = n(), # Total number of responses
    total_correct = sum(correct_answered == 1), # Total correct responses
    percentage_correct = (total_correct / total_responses) * 100 # Percentage of correct responses
  )


# Calculate the total number of unique items (item_id) played before and after the new UI (1 vs 0) per domain
result_items_played <- data %>%
  group_by(domain_id, new_UI) %>%
  summarise(
    total_items_played = n() # Count distinct item_id (unique items played)
  )

# Add the domain names to the result
result_items_played$domain_name <- domain_names[as.character(result_items_played$domain_id)]

# Percent of late responses that went past extended
sum(ed_logs_after$answer == "…") / sum(ed_logs_after$late_response == 1) * 100 # 23.92%



#### DATA VISUALIZATION ####
### NO ANSWER ###

# Plot no answer before and after - domain
plot_no_answer_domain <- data %>%
  group_by(new_UI, domain_name) %>%
  summarise(
    no_answer_percentage = mean(no_answer) * 100,
    sd = sd(no_answer) * 100,
    .groups = "drop"
  ) %>%
  ggplot(aes(
    y = reorder(domain_name, no_answer_percentage),
    x = no_answer_percentage,
    fill = as.factor(new_UI)
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = custom_colors) + # Apply consistent colors
  labs(title = "   By Domain",
       y = "Domain",
       x = "  ",
       fill = "Update on Oct 25th") +
  custom_theme

# Plot no answer before and after - grade
plot_no_answer_grade <- data %>%
  group_by(new_UI, grade) %>%
  summarise(
    no_answer_percentage = mean(no_answer) * 100,
    sd = sd(no_answer) * 100,
    .groups = "drop"
  ) %>%
  ggplot(aes(
    x = factor(grade),
    y = no_answer_percentage,
    fill = as.factor(new_UI)
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = custom_colors) + # Apply consistent colors
  labs(title = "   By Grade",
       x = "Grade",
       y = "  ",
       fill = "Update on Oct 25th") +
  custom_theme

# Plot no answer before and after - difficulty
plot_no_answer_difficulty <- data %>%
  group_by(new_UI, modeDifficulty) %>%
  summarise(
    no_answer_percentage = mean(no_answer) * 100,
    sd = sd(no_answer) * 100,
    .groups = "drop"
  ) %>%
  mutate(modeDifficultyLabel = factor(
    case_when(
      modeDifficulty == "0" ~ "Easy",
      modeDifficulty == "1" ~ "Medium",
      modeDifficulty == "2" ~ "Hard"
    ),
    levels = c("Easy", "Medium", "Hard") # Set the desired order
  )) %>%
  ggplot(aes(
    x = modeDifficultyLabel,
    y = no_answer_percentage,
    fill = as.factor(new_UI)
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = custom_colors) + # Apply consistent colors
  labs(title = "   By Mode Difficulty",
       x = "Mode Difficulty",
       y = "  ",
       fill = "Update on Oct 25th") +
  custom_theme

# Plot no answer before and after - learning goal
plot_no_answer_learning_goal <- data %>%
  group_by(new_UI, learning_goal) %>%
  summarise(
    no_answer_percentage = mean(no_answer) * 100,
    sd = sd(no_answer) * 100,
    .groups = "drop"
  ) %>%
  ggplot(aes(
    x = factor(learning_goal, labels = c("No", "Yes")),
    y = no_answer_percentage,
    fill = as.factor(new_UI)
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = custom_colors) + # Apply consistent colors
  labs(title = "      For Learning Goal Sessions",
       x = "Learning Goal Session",
       y = "  ",
       fill = "Update on Oct 25th") +
  custom_theme


# Combine all plots into a 2x2 grid
grid.arrange(
  plot_no_answer_domain,
  plot_no_answer_grade,
  plot_no_answer_difficulty,
  plot_no_answer_learning_goal,
  ncol = 2,
  top = textGrob("Percentage of No Answer",
                 gp = gpar(
                   fontsize = 16, fontface = "bold"
                 ))
)

#### HELP ###

# Plot help before and after - domain
plot_help_domain <- data %>%
  group_by(new_UI, domain_name) %>%
  summarise(
    help_percentage = mean(help) * 100,
    sd = sd(help) * 100,
    .groups = "drop"
  ) %>%
  ggplot(aes(
    y = reorder(domain_name, help_percentage),
    x = help_percentage,
    fill = as.factor(new_UI)
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = custom_colors) + # Apply consistent colors
  labs(title = "  By Domain",
       y = "Domain",
       x = " ",
       fill = "Update on Oct 25th") +
  custom_theme

# Plot help before and after - grade
plot_help_grade <- data %>%
  group_by(new_UI, grade) %>%
  summarise(
    help_percentage = mean(help) * 100,
    sd = sd(help) * 100,
    .groups = "drop"
  ) %>%
  ggplot(aes(
    x = factor(grade),
    y = help_percentage,
    fill = as.factor(new_UI)
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = custom_colors) + # Apply consistent colors
  labs(title = "  By Grade",
       x = "Grade",
       y = " ",
       fill = "Update on Oct 25th") +
  custom_theme

# Plot help before and after - difficulty
plot_help_difficulty <- data %>%
  group_by(new_UI, modeDifficulty) %>%
  summarise(
    help_percentage = mean(help) * 100,
    sd = sd(help) * 100,
    .groups = "drop"
  ) %>%
  mutate(modeDifficultyLabel = factor(
    case_when(
      modeDifficulty == "0" ~ "Easy",
      modeDifficulty == "1" ~ "Medium",
      modeDifficulty == "2" ~ "Hard"
    ),
    levels = c("Easy", "Medium", "Hard") # Set the desired order
  )) %>%
  ggplot(aes(
    x = modeDifficultyLabel,
    y = help_percentage,
    fill = as.factor(new_UI)
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = custom_colors) + # Apply consistent colors
  labs(title = "  By Mode Difficulty",
       x = "Mode Difficulty",
       y = " ",
       fill = "Update on Oct 25th") +
  custom_theme

# Plot help before and after - learning goal
plot_help_learning_goal <- data %>%
  group_by(new_UI, learning_goal) %>%
  summarise(
    help_percentage = mean(help) * 100,
    sd = sd(help) * 100,
    .groups = "drop"
  ) %>%
  ggplot(aes(
    x = factor(learning_goal, labels = c("No", "Yes")),
    y = help_percentage,
    fill = as.factor(new_UI)
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = custom_colors) + # Apply consistent colors
  labs(title = "     For Learning Goal Sessions",
       x = "Learning Goal Session",
       y = " ",
       fill = "Update on Oct 25th") +
  custom_theme


# Combine all plots into a 2x2 grid
grid.arrange(
  plot_help_domain,
  plot_help_grade,
  plot_help_difficulty,
  plot_help_learning_goal,
  ncol = 2,
  top = textGrob("Percentage of Help",
                 gp = gpar(
                   fontsize = 16, fontface = "bold"
                 ))
)


### CORRECT ANSWERS ###

# Percentage correct before and after - domain
plot_correct_domain <-
  ggplot(correct_answers_domain,
         aes(x = correct_percentage, y = domain_name, fill = UI_Status)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "    By Domain",
       x = "   ",
       y = "Domain",
       fill = "Update on Oct 25th") +
  custom_theme

# Percentage correct before and after - grade
plot_correct_grade <-
  ggplot(correct_answers_grade,
         aes(x = factor(grade), y = correct_percentage, fill = UI_Status)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "    By Grade",
       x = "Grade",
       y = "   ",
       fill = "Update on Oct 25th") +
  custom_theme

# Percentage correct before and after - difficulty
plot_correct_difficulty <- ggplot(
  correct_answers_difficulty,
  aes(x = Difficulty,
      y = correct_percentage,
      fill = UI_Status)
) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "    By Difficulty",
       x = "Difficulty",
       y = "   ",
       fill = "Update on Oct 25th") +
  custom_theme

# Percentage correct before and after - learning goal
plot_correct_learning_goal <- ggplot(
  correct_answers_learning,
  aes(x = LearningGoal,
      y = correct_percentage,
      fill = UI_Status)
) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "    By Learning Goal",
       x = "Learning Goal",
       y = "   ",
       fill = "Update on Oct 25th") +
  custom_theme

# Combine all plots into a 2x2 grid
grid.arrange(
  plot_correct_domain,
  plot_correct_grade,
  plot_correct_difficulty,
  plot_correct_learning_goal,
  ncol = 2,
  top = textGrob(
    "Percentage of Correct Answers",
    gp = gpar(fontsize = 16, fontface = "bold")
  )
)


### TEMPORAL PATTERN ###

# Late Response Rate by Hour of Day
plot_hour <-
  ggplot(late_response_by_hour, aes(x = hour, y = late_response_rate)) +
  geom_line(color = "#57b0b5", size = 1.2) +
  geom_point(color = "#57b0b5", size = 3) +
  labs(title = "Late Response Rate by Hour of Day",
       x = "Hour of Day",
       y = "Late Response Rate (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Late Responses by Weekday
plot_weekday <-
  ggplot(late_response_by_weekday,
         aes(x = weekday, y = late_response_rate)) +
  geom_bar(stat = "identity", fill = "#fe6c66") +
  labs(title = "Late Responses by Weekday",
       x = "Weekday",
       y = "Late Response Rate (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Late Responses by Week
plot_week <-
  ggplot(late_response_by_week,
         aes(x = week_since_ui, y = late_response_rate)) +
  geom_line(color = "#57b0b5", size = 1) +
  geom_point(color = "#57b0b5", size = 2) +
  labs(title = "Late Responses by Week",
       x = "Week Since New UI",
       y = "Late Response Rate (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Arrange the three plots into a single row grid
grid.arrange(
  plot_hour,
  plot_weekday,
  plot_week,
  ncol = 3,
  top = textGrob("Temporal Pattern",
                 gp = gpar(
                   fontsize = 16, fontface = "bold"
                 ))
)


### DOMAIN 59 ###

# Domain 59 - late
late_59 <-
  ggplot(result, aes(x = as.factor(deadline), y = percentage_non_no_responses)) +
  geom_bar(stat = "identity", fill = "#fe6c66") +
  labs(title = "Late Responses per Deadline",
       x = "Deadline",
       y = "Percentage (%)") +
  custom_theme

# Domain 59 - no answer
no_answer_59  <- ggplot(result_no_answer,
                        aes(
                          x = as.factor(deadline),
                          y = percentage_no_answer,
                          fill = as.factor(new_UI)
                        )) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#00BFC4", "#fe6c66")) + # Custom colors for new_UI 0 and 1
  labs(title = "No Answer per Deadline",
       x = "Deadline",
       y = "Percentage (%)",
       fill = "Update on Oc 25th ") +
  custom_theme # You can replace with your custom theme if needed


# Domain 59 - correct
correct_59 <- ggplot(
  correct_answered_domain_59,
  aes(
    x = as.factor(deadline),
    y = percentage_correct,
    fill = as.factor(new_UI)
  )
) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#00BFC4", "#fe6c66")) + # Custom colors for new_UI 0 and 1
  labs(title = "Correct Responses per Deadline",
       x = "Deadline",
       y = "Percentage (%)",
       fill = "Update on Oct 25th") +
  custom_theme # You can replace with your custom theme if needed



# Combine all plots into a grid
grid.arrange(
  no_answer_59,
  late_59,
  correct_59,
  ncol = 3,
  top = textGrob(
    "Percentage of Correct Answers",
    gp = gpar(fontsize = 16, fontface = "bold")
  )
)


### ITEMS PLAYED ###

# Plot the result using ggplot
items_played_plot <- ggplot(result_items_played,
                            aes(
                              x = domain_name,
                              y = total_items_played,
                              fill = as.factor(new_UI)
                            )) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) + # Custom colors for new_UI 0 and 1
  labs(title = "Total Items Played per Domain",
       x = " ",
       y = "TotalItems Played",
       fill = "Update on Oct 25th") +
  custom_theme # You can replace with your custom theme if needed

# Display the plot
print(items_played_plot)
