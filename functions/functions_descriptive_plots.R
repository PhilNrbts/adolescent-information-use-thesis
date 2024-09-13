## Color class graphs

### Functions color class graphs

## Prepare data to get the insights

calculate_relative_color_class_changes <- function(data){
  data <- data %>%
    group_by(player, unique_rounds) %>%
    mutate(previous_color_class = lag(color_class),
           color_class_change = color_class - previous_color_class,
           color_class_change_category = case_when(
             color_class_change > 0 ~ "increase",
             color_class_change == 0 ~ "same",
             color_class_change < 0 ~ "decrease",
             TRUE ~ NA_character_ # For NA values in color_class_change
           )
    ) %>%
    ungroup()
  return(data)
}

create_first_copy_value <- function(data){
  
}
# functions to get likelihoods

likelihoods_func <- function(data, social_info_use_val, social_value_cond = NULL) {
  if (social_info_use_val == "ignore") {
    data_filtered <- data %>%
      filter(social_info_use == "ignore", known_cell == FALSE, phases != "gem")
  } else {
    data_filtered <- data %>%
      filter(social_info_use == "copy", social_value > social_value_cond, known_cell == FALSE, phases != "gem")
  }
  
  data_filtered %>%
    group_by(previous_color_class) %>%
    summarise(
      total = n(),
      likelihood_increase = sum(color_class_change_category == "increase") / total,
      likelihood_same = sum(color_class_change_category == "same") / total,
      likelihood_decrease = sum(color_class_change_category == "decrease") / total
    ) %>%
    na.omit()
}

plot_likelihoods <- function(likelihoods, plot_title) {
  # Pivot the data to a longer format
  likelihoods_long <- likelihoods %>%
    pivot_longer(cols = c(likelihood_increase, likelihood_same, likelihood_decrease),
                 names_to = "Change",
                 values_to = "Likelihood")
  
  # Convert Change to a factor and specify level order
  likelihoods_long$Change <- factor(likelihoods_long$Change, 
                                    levels = c("likelihood_decrease", "likelihood_same", "likelihood_increase"))
  
  # Create the plot
  ggplot(likelihoods_long, aes(x = as.factor(previous_color_class), y = Likelihood, fill = Change)) +
    geom_bar(stat = "identity", color = "black") +
    labs(x = "Previous Color Class", y = "Likelihood", fill = "Color Class Change",
         title = plot_title) +
    scale_fill_brewer(palette = "Spectral") +
    guides(fill = guide_legend(reverse = FALSE)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

likelihoods_count_func <- function(data, social_info_use_val, social_value_cond = NULL) {
  
  if (social_info_use_val == "ignore") {
    data_filtered <- data %>%
      filter(social_info_use == "ignore", known_cell == FALSE, phases != "gem")
  } else {
    data_filtered <- data %>%
      filter(social_info_use == "copy", social_value > social_value_cond, known_cell == FALSE, phases != "gem")
  }
  
  totals_pre <- data_filtered %>%
    group_by(previous_color_class) %>%
    summarise(
      total_pre = n()
    ) %>%
    na.omit()
  
  totals_post <- data_filtered %>%
    group_by(color_class) %>%
    summarise(
      total_post = n()
    ) %>%
    na.omit()
  
  merged_totals <- merge(totals_pre, totals_post, by.x = "previous_color_class", by.y = "color_class", all = TRUE)
  
  return(merged_totals)
}

plot_total_likelihoods <- function(data, plot_title) {
  data_long <- data %>%
    pivot_longer(c(total_pre, total_post), names_to = "Time", values_to = "Total")
  
  ggplot(data_long, aes(x = factor(previous_color_class), y = Total, fill = factor(Time, levels = c("total_pre", "total_post")))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Color Class", y = "Total", fill = "Time", title = plot_title) +
    theme_minimal()
}

wilcox_test_groups <- function(data) {
  
  # Filter data for different social values
  group_0 <- data %>%
    filter(social_value == 0, first_copy == TRUE, color_class != 4.5)
  group_1 <- data %>%
    filter(social_value == 1, first_copy == TRUE, color_class != 4.5)
  group_2 <- data %>%
    filter(social_value == 2, first_copy == TRUE, color_class != 4.5)
  group_3 <- data %>%
    filter(social_value == 3, first_copy == TRUE, color_class != 4.5)
  group_4 <- data %>%
    filter(social_value == 4, first_copy == TRUE, color_class != 4.5)
  group_5 <- data %>%
    filter(social_value == 5, first_copy == TRUE, color_class != 4.5)
  
  # Filter data for no_copy group
  group_no_copy <- data %>%
    filter(social_info_use == "ignore", known_cell == FALSE, phases != "gem")
  
  # Define groups
  groups <- list(
    "0" = group_0$color_class,
    "1" = group_1$color_class,
    "2" = group_2$color_class,
    "3" = group_3$color_class,
    "4" = group_4$color_class,
    "5"  = group_5$color_class,
    "no_copy" = group_no_copy$color_class
  )
  
  # Pairwise comparisons
  comparisons <- list(
    c("0", "no_copy"),
    c("1", "no_copy"),
    c("0", "1"),
    c("1", "2"),
    c("2", "3"),
    c("3", "4"),
    c("3", "5")
  )
  
  # Run tests and gather results
  results <- lapply(comparisons, function(pair) {
    test_result <- wilcox.test(groups[[pair[1]]], groups[[pair[2]]])
    return(c("group_1" = pair[1], "group_2" = pair[2], "W" = test_result$statistic, "p" = test_result$p.value))
  })
  
  # Combine results into a data frame
  results_df <- do.call(rbind, results)
  results_df <- as.data.frame(results_df)
  # Round p-values
  results_df$p <- as.numeric(results_df$p)  # ensure that p-values are numeric
  results_df$p <- round(results_df$p, 3)
  return(results_df)
}

create_pie_plot <- function(summary_color_class_likelihood) {
  # Order the data frame by SocialValue
  summary_color_class_likelihood <- summary_color_class_likelihood %>%
    arrange(SocialValue)
  
  # Create a new row for 'rest'
  rest_row <- data.frame(SocialValue = "rest", 
                         Total = sum(summary_color_class_likelihood$Total[9:nrow(summary_color_class_likelihood)]))
  
  # Combine the first 8 rows with the 'rest' row
  summary_color_class_likelihood_reduced <- rbind(summary_color_class_likelihood[1:8, ], rest_row)
  
  # Calculate proportions
  proportions <- summary_color_class_likelihood_reduced$Total/sum(summary_color_class_likelihood$Total)
  
  # Create labels that include both SocialValue and the percentages
  labels <- paste(summary_color_class_likelihood_reduced$SocialValue, " : ", round(proportions*100, 1), "%", sep="")
  
  # Create a pie chart
  pie(proportions, 
      main = "Proportion of Color Class Likelihood", 
      labels = labels,  # use the new labels
      col = rainbow(length(proportions)))   # use different colors for each slice
}

create_repeat_plot <- function(df, plot_type) {
  if (plot_type == "Class") {
    p <- ggplot(df, aes(x = repeat_length, y = mean_color_class)) +
      geom_point() +
      geom_errorbar(aes(ymin = mean_color_class - std_color_class/sqrt(count), 
                        ymax = mean_color_class + std_color_class/sqrt(count)), 
                    width = 0.2) +
      labs(x = "Repeat Length", 
           y = "Mean Color Class", 
           title = "Mean Color Class by Repeat Length with Standard Errors") +
      scale_x_continuous(breaks = seq(min(df$repeat_length), 
                                      max(df$repeat_length),
                                      by = 1)) +
      theme_minimal()
  } else if (plot_type == "Points") {
    p <- ggplot(df, aes(x = repeat_length, y = mean_points)) +
      geom_point() +
      geom_errorbar(aes(ymin = mean_points - std_points/sqrt(count), 
                        ymax = mean_points + std_points/sqrt(count)), 
                    width = 0.2) +
      labs(x = "Repeat Length", 
           y = "Mean Points", 
           title = "Mean Points by Repeat Length with Standard Errors") +
      scale_x_continuous(breaks = seq(min(df$repeat_length), 
                                      max(df$repeat_length),
                                      by = 1)) +
      theme_minimal()
  } else if (plot_type == "Trial") {
    p <- ggplot(df, aes(x = repeat_length, y = mean_trial)) +
      geom_point() +
      geom_errorbar(aes(ymin = mean_trial - std_trial/sqrt(count), 
                        ymax = mean_trial + std_trial/sqrt(count)), 
                    width = 0.2) +
      labs(x = "Repeat Length", 
           y = "Mean Trial", 
           title = "Mean Trial by Repeat Length with Standard Errors") +
      scale_x_continuous(breaks = seq(min(df$repeat_length), 
                                      max(df$repeat_length),
                                      by = 1)) +
      theme_minimal()
  } else if (plot_type == "Total") {
    p <- ggplot(df, aes(x = repeat_length, y = count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "Repeat Length", 
           y = "Total", 
           title = "Bar Chart of Repeat Length and Total") +
      scale_x_continuous(breaks = seq(min(df$repeat_length), 
                                      max(df$repeat_length),
                                      by = 1)) +
      theme_minimal()
  } else {
    stop("Invalid plot type. Choose from 'Class', 'Points', 'Trial', or 'Total'.")
  }
  
  return(p)
}

create_pie_plot_repeat_length <- function(avg_measures_repeated) {
  # Order the data frame by repeat_length
  avg_measures_repeated <- avg_measures_repeated %>%
    arrange(repeat_length)
  
  # Create a new row for 'rest'
  rest_row <- data.frame(repeat_length = "rest", 
                         mean_color_class = NA,
                         std_color_class = NA,
                         mean_points = NA,
                         std_points = NA,
                         mean_trial = NA,
                         std_trial = NA,
                         count = sum(avg_measures_repeated$count[9:nrow(avg_measures_repeated)])
  )
  
  # Combine the first 8 rows with the 'rest' row
  avg_measures_repeated_reduced <- rbind(avg_measures_repeated[1:8, ], rest_row)
  
  # Calculate proportions
  proportions <- avg_measures_repeated_reduced$count/sum(avg_measures_repeated$count)
  
  # Create labels that include both repeat_length and the percentages
  labels <- paste(avg_measures_repeated_reduced$repeat_length, " : ", round(proportions*100, 1), "%", sep="")
  
  # Create a pie chart
  pie(proportions, 
      main = "Proportion of Total Count by Repeat Length", 
      labels = labels,  # use the new labels
      col = rainbow(length(proportions)))   # use different colors for each slice
}


### Plots exploration and exploitation moves

data <- calculate_relative_color_class_changes(data) 

## Calculate likelihoods

likelihoods_nonsocial <- likelihoods_func(data, "ignore")
likelihoods_social <- likelihoods_func(data, "copy", 0)
likelihoods_count_nonsocial <- likelihoods_count_func(data, "ignore")
likelihoods_count_social <- likelihoods_count_func(data, "copy", 0)
## Tables and Plots Likelihoods

print(likelihoods_count_nonsocial)
plot_total_likelihoods(likelihoods_count_nonsocial,"Total Field Change by Color Class for Non-Social")
print(likelihoods_nonsocial)
plot_likelihoods(likelihoods_nonsocial, "Likelihood of Color Class Change for Non-Social")

print(likelihoods_count_social)
plot_total_likelihoods(likelihoods_count_social,"Total Field Change by Color Class for Social")
print(likelihoods_social)
plot_likelihoods(likelihoods_social, "Likelihood of Color Class Change for Social")

# Identify first copying of a social_info_use per cell for each unique round
data <- data %>%
  arrange(unique_rounds, player, trial) %>%
  group_by(unique_rounds, player, cell) %>%
  mutate(first_copy = ifelse(row_number() == 1 & social_info_use == "copy", TRUE, FALSE)) %>%
  ungroup()



# Now we calculate the likelihoods of each color class for each unique social value
color_class_likelihoods <- data %>%
  filter(first_copy == TRUE) %>%
  group_by(social_value) %>%
  count(color_class) %>%
  group_by(social_value) %>%
  mutate(total = sum(n),
         likelihood = n / total) %>%
  ungroup()

average_result_social_value <- data %>%
  filter(first_copy == TRUE, color_class != 4.5) %>%
  group_by(social_value) %>%
  summarise(
    mean_color_class = mean(color_class, na.rm = TRUE),
    std_color_class = sd(color_class, na.rm = TRUE),
    count = n()
  )

no_copy_row <- data %>%
  filter(social_info_use == "ignore", known_cell == FALSE, phases != "gem") %>%
  summarise(
    social_value = -1,  # Using -1 as placeholder
    mean_color_class = mean(color_class, na.rm = TRUE),
    std_color_class = sd(color_class, na.rm = TRUE),
    count = n()
  )

average_result_social_value <- average_result_social_value %>%
  bind_rows(no_copy_row)

ggplot(average_result_social_value, aes(x = social_value, y = mean_color_class)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_color_class - std_color_class/sqrt(count), 
                    ymax = mean_color_class + std_color_class/sqrt(count)), 
                width = 0.2) +
  labs(x = "Social Value", 
       y = "Mean Color Class", 
       title = "Mean Color Class by Social Value with Standard Errors") +
  scale_x_continuous(breaks = c(-1, sort(unique(average_result_social_value$social_value[average_result_social_value$social_value != -1]))), 
                     labels = c("no_copy", sort(unique(average_result_social_value$social_value[average_result_social_value$social_value != -1])))) +
  theme_minimal()

# Whitney comparisons
wilcox_test_groups(data)

# Print the data
color_class_likelihoods

# Renaming and reordering columns
color_class_likelihoods <- color_class_likelihoods %>%
  rename(
    SocialValue = social_value,
    ColorClass = color_class,
    Count = n,
    Total = total,
    Likelihood = likelihood
  ) %>%
  select(SocialValue, ColorClass, Likelihood, Count, Total)

# Calculate proportions
summary_color_class_likelihood <- color_class_likelihoods %>% 
  select(SocialValue, Total) %>%
  distinct()

color_class_likelihoods %>%
  select(SocialValue, Total) %>% 
  distinct() %>%
  ggplot(aes(x = SocialValue, y = Total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Social Value", y = "Total", title = "Bar Chart of Social Value and Total") +
  theme_minimal()

# Create pie plot
create_pie_plot(summary_color_class_likelihood)

# Plotting
ggplot(color_class_likelihoods, aes(x = as.factor(SocialValue), y = Likelihood, fill = as.factor(ColorClass))) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_brewer(palette = "Spectral") +
  labs(
    title = "Likelihood of Color Classes if Copying per Social Value",
    x = "Social Value",
    y = "Likelihood",
    fill = "Color Class"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Add initial color class for each cell in each unique round
# Calculate the color class change and classify it as 'increase', 'decrease', or 'same'

data <- data %>%
  group_by(unique_rounds, cell) %>%
  mutate(initial_cell_color_class = first(color_class),
         initial_cell_points = first(points),
         initial_cell_trial = first(trial)) %>%
  ungroup()
data <- data %>%
  mutate(repeated_choice_class_diff = color_class - initial_cell_color_class,
         repeated_choice_trial_diff = trial - initial_cell_trial,
         repeated_choice_points_diff = points - initial_cell_points,
         repeated_choice_class_diff_category = case_when(
           repeated_choice_class_diff > 0 ~ "increase",
           repeated_choice_class_diff == 0 ~ "same",
           repeated_choice_class_diff < 0 ~ "decrease"))

data <- data %>%
  group_by(unique_rounds, cell) %>%
  mutate(repeat_length = ifelse(phases == "gem", NaN, max(repeated_choice_trial_diff, na.rm = TRUE))) %>%
  ungroup()

repeated_summary <- data %>%
  select(unique_rounds, player, group, round,
         initial_cell_color_class,initial_cell_points,initial_cell_trial,repeat_length) %>%
  distinct()


print(repeated_summary %>% filter(repeat_length != 0) %>% count(repeat_length))

avg_measures_repeated <- repeated_summary %>%
  filter(repeat_length != 0) %>%
  group_by(repeat_length) %>%
  summarise(
    mean_color_class = mean(initial_cell_color_class, na.rm = TRUE),
    std_color_class = sd(initial_cell_color_class, na.rm = TRUE),
    mean_points = mean(initial_cell_points, na.rm = TRUE),
    std_points = sd(initial_cell_points, na.rm = TRUE),
    mean_trial = mean(initial_cell_trial, na.rm = TRUE),
    std_trial = sd(initial_cell_trial, na.rm = TRUE),
    count = n()
  )

#Plots

create_repeat_plot(avg_measures_repeated,"Total")
create_pie_plot_repeat_length(avg_measures_repeated)
create_repeat_plot(avg_measures_repeated,"Class")
create_repeat_plot(avg_measures_repeated,"Points")
create_repeat_plot(avg_measures_repeated,"Trial")

# Filter data for known_cell = TRUE and phases not 'gem'
data_filtered <- data %>%
  filter(known_cell == TRUE, phases != "gem", repeated_choice_trial_diff > 0)
# Calculate the total for each change category within each initial color class
drift_likelihoods <- data_filtered %>%
  group_by(initial_cell_color_class, repeated_choice_class_diff_category) %>%
  summarise(
    total = n()
  ) %>%
  ungroup()
print(nrow(data_filtered))
repeated_choice_count <- data %>%
  filter(known_cell == TRUE, phases != "gem") %>%
  count(repeated_choice_trial_diff) %>% 
  mutate(n_diff = n - lead(n, default = 0))
# Calculate the total for each initial color class
total_per_initial_cell_color_class <- data_filtered %>%
  group_by(initial_cell_color_class) %>%
  summarise(
    total_initial = n()
  )

# Join the two data frames
drift_likelihoods <- left_join(drift_likelihoods, total_per_initial_cell_color_class, by = "initial_cell_color_class")

# Calculate the likelihood
drift_likelihoods <- drift_likelihoods %>%
  mutate(likelihood = total / total_initial)

# View the updated data
print(drift_likelihoods)

#Histogram and Density Curve for repeated choices
std_points_diff <- sd(data_filtered$repeated_choice_points_diff, na.rm = TRUE)

ggplot(data_filtered, aes(x = repeated_choice_points_diff)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(alpha = .2, fill="#FF6666") +
  geom_vline(aes(xintercept = -1.96*std_points_diff), color = "red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept = 1.96*std_points_diff), color = "red", linetype="dashed", size=1) +
  geom_text(aes(x = min(repeated_choice_points_diff), y = 0.02, label = paste("Std Dev: ", round(std_points_diff, 3))), hjust = 0) +
  geom_text(aes(x = min(repeated_choice_points_diff), y = 0.01, label = paste("Margin of Error: ", round(1.96*std_points_diff, 3))), hjust = 0) +
  labs(x = "Repeated Choice Points Difference", y = "Density", 
       title = "Histogram with Density Curve and 95% Confidence Interval") +
  theme_minimal()


# Convert color_class_change_category to a factor and specify level order
drift_likelihoods$repeated_choice_class_diff_category <- factor(drift_likelihoods$repeated_choice_class_diff_category, levels = c("decrease", "same", "increase"))

# Create the plot
ggplot(drift_likelihoods, aes(x = as.factor(initial_cell_color_class), y = likelihood, fill = repeated_choice_class_diff_category)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Initial Color Class", y = "Likelihood", fill = "Color Class Change",
       title = "Likelihood of Color Class Change per Initial Color Class for Known Cells (Non-gem Phases)") +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))