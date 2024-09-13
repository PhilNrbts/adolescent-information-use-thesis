# Helper function to calculate common statistics for a given variable
calc_stats <- function(data, variable) {
  std_value <- sd({{variable}}, na.rm = TRUE)
  count_value <- sum(!is.na({{variable}}))
  data %>%
    summarise(
      mean = mean({{variable}}, na.rm = TRUE),
      median = median({{variable}}, na.rm = TRUE),
      std = std_value,
      se = std_value / sqrt(count_value)
    )
}

calculate_statistics <- function(data) {
  # Filter data to only those who found gems
  filtered_data <- data[!is.na(data$phase_gem_found), ]
  overall_stats <- filtered_data %>%
    summarise(
      mean_length_exploitation = calc_stats(data, length_exploitation)$mean,
      median_length_exploitation = calc_stats(data, length_exploitation)$median,
      std_length_exploitation = calc_stats(data, length_exploitation)$std,
      se_length_exploitation = calc_stats(data, length_exploitation)$se,
      mean_length_exploration = calc_stats(data, length_exploration)$mean,
      median_length_exploration = calc_stats(data, length_exploration)$median,
      std_length_exploration = calc_stats(data, length_exploration)$std,
      se_length_exploration = calc_stats(data, length_exploration)$se,
      mean_threshold_accumulation = calc_stats(data, threshold_accumulation)$mean,
      median_threshold_accumulation = calc_stats(data, threshold_accumulation)$median,
      std_threshold_accumulation = calc_stats(data, threshold_accumulation)$std,
      se_threshold_accumulation = calc_stats(data, threshold_accumulation)$se,
      mean_threshold_exploitation = calc_stats(data, threshold_exploitation)$mean,
      median_threshold_exploitation = calc_stats(data, threshold_exploitation)$median,
      std_threshold_exploitation = calc_stats(data, threshold_exploitation)$std,
      se_threshold_exploitation = calc_stats(data, threshold_exploitation)$se
    )
  
  
  group_stats <- filtered_data %>%
    group_by(group) %>%
    summarise(
      mean_length_exploitation = calc_stats(data, length_exploitation)$mean,
      median_length_exploitation = calc_stats(data, length_exploitation)$median,
      std_length_exploitation = calc_stats(data, length_exploitation)$std,
      se_length_exploitation = calc_stats(data, length_exploitation)$se,
      mean_length_exploration = calc_stats(data, length_exploration)$mean,
      median_length_exploration = calc_stats(data, length_exploration)$median,
      std_length_exploration = calc_stats(data, length_exploration)$std,
      se_length_exploration = calc_stats(data, length_exploration)$se,
      mean_threshold_accumulation = calc_stats(data, threshold_accumulation)$mean,
      median_threshold_accumulation = calc_stats(data, threshold_accumulation)$median,
      std_threshold_accumulation = calc_stats(data, threshold_accumulation)$std,
      se_threshold_accumulation = calc_stats(data, threshold_accumulation)$se,
      mean_threshold_exploitation = calc_stats(data, threshold_exploitation)$mean,
      median_threshold_exploitation = calc_stats(data, threshold_exploitation)$median,
      std_threshold_exploitation = calc_stats(data, threshold_exploitation)$std,
      se_threshold_exploitation = calc_stats(data, threshold_exploitation)$se
    )
  
  combined_stats <- bind_rows(overall_stats, group_stats)
  return(combined_stats)
}

# Helper function to compute common statistics
compute_summary_stats <- function(data, phase_name) {
  data %>%
    summarise(
      Phase = phase_name,
      Count = n(),
      Mean_round_gem_found = mean(round_gem_found, na.rm = TRUE),
      Median_round_gem_found = median(round_gem_found, na.rm = TRUE),
      Mean_length_exploration = mean(length_exploration, na.rm = TRUE),
      Median_length_exploration = median(length_exploration, na.rm = TRUE),
      Std_length_exploration = sd(length_exploration, na.rm = TRUE),
      SE_length_exploration = Std_length_exploration / sqrt(Count)
    )
}

summary_table_gem_found <- function(summary_data,combined_stats) {
  
  # Pre-compute for exploration
  exploration_temp <- summary_data %>%
    filter(!is.na(phase_gem_found) & phase_gem_found == "exploration") %>%
    mutate(exploration_exceeds_gem = ifelse(length_exploration > round_gem_found, 1, 0))
  
  # Calculate the exceed values for exploration
  exceeds_gem_no_value_exploration <- sum(exploration_temp$exploration_exceeds_gem == 0, na.rm = TRUE)
  exceeds_gem_yes_value_exploration <- sum(exploration_temp$exploration_exceeds_gem == 1, na.rm = TRUE)
  
  # Summarize the exploration data
  exploration_data <- compute_summary_stats(exploration_temp, "Exploration")
  exploration_data$exceeds_gem_no <- exceeds_gem_no_value_exploration
  exploration_data$exceeds_gem_yes <- exceeds_gem_yes_value_exploration
  
  # Pre-compute for accumulation
  accumulation_temp <- summary_data %>%
    filter(!is.na(phase_gem_found) & phase_gem_found == "accumulation") %>%
    mutate(accumulation_exceeds_gem = ifelse((length_exploration + length_accumulation) > round_gem_found, 1, 0))
  
  # Calculate the exceed values for accumulation
  exceeds_gem_no_value_accumulation <- sum(accumulation_temp$accumulation_exceeds_gem == 0, na.rm = TRUE)
  exceeds_gem_yes_value_accumulation <- sum(accumulation_temp$accumulation_exceeds_gem == 1, na.rm = TRUE)
  
  # Summarize the accumulation data
  accumulation_data <- compute_summary_stats(accumulation_temp, "Accumulation")
  accumulation_data$exceeds_gem_no <- exceeds_gem_no_value_accumulation
  accumulation_data$exceeds_gem_yes <- exceeds_gem_yes_value_accumulation
  
  # Additional computations for accumulation data
  accumulation_data <- accumulation_data %>%
    mutate(
      Mean_length_accumulation = mean(accumulation_temp$length_accumulation, na.rm = TRUE),
      Median_length_accumulation = median(accumulation_temp$length_accumulation, na.rm = TRUE),
      Std_length_accumulation = sd(accumulation_temp$length_accumulation, na.rm = TRUE),
      SE_length_accumulation = Std_length_accumulation / sqrt(Count)
    )
  
  # Other computations remain the same
  gem_found_count <- table(summary_data$gem_found)
  overall_mean <- mean(summary_data$round_gem_found, na.rm = TRUE)
  overall_median <- median(summary_data$round_gem_found, na.rm = TRUE)
  overall_exceeds_yes <- exploration_data$exceeds_gem_yes + accumulation_data$exceeds_gem_yes
  overall_exceeds_no <- exploration_data$exceeds_gem_no + accumulation_data$exceeds_gem_no
  
  # Extract values from combined_stats
  overall_stats <- combined_stats %>%
    filter(is.na(group))
  
  exploration_stats <- combined_stats %>%
    filter(group == "Exploration")
  
  accumulation_stats <- combined_stats %>%
    filter(group == "Accumulation")
  
  # Construct the summary table using values from combined_stats
  summary_table <- data.frame(
    Phase = c("Overall", "Exploration", "Accumulation"),
    Count = c(gem_found_count[[2]], exploration_data$Count, accumulation_data$Count),
    Mean_round_gem_found = c(overall_mean, exploration_data$Mean_round_gem_found, accumulation_data$Mean_round_gem_found),
    Median_round_gem_found = c(overall_median, exploration_data$Median_round_gem_found, accumulation_data$Median_round_gem_found),
    Exceeds_Gem_Yes = c(overall_exceeds_yes, exploration_data$exceeds_gem_yes, accumulation_data$exceeds_gem_yes),
    Exceeds_Gem_No = c(overall_exceeds_no, exploration_data$exceeds_gem_no, accumulation_data$exceeds_gem_no),
    Mean_length_exploration = c(overall_stats$mean_length_exploration, exploration_stats$mean_length_exploration, accumulation_stats$mean_length_exploration),
    Median_length_exploration = c(overall_stats$median_length_exploration, exploration_stats$median_length_exploration, accumulation_stats$median_length_exploration),
    Std_length_exploration = c(overall_stats$std_length_exploration, exploration_stats$std_length_exploration, accumulation_stats$std_length_exploration),
    SE_length_exploration = c(overall_stats$se_length_exploration, exploration_stats$se_length_exploration, accumulation_stats$se_length_exploration),
    Mean_length_accumulation = c(NA, NA, accumulation_data$Mean_length_accumulation),
    Median_length_accumulation = c(NA, NA, accumulation_data$Median_length_accumulation),
    Std_length_accumulation = c(NA, NA, accumulation_data$Std_length_accumulation),
    SE_length_accumulation = c(NA, NA, accumulation_data$SE_length_accumulation),
    Mean_threshold_accumulation = c(overall_stats$mean_threshold_accumulation, exploration_stats$mean_threshold_accumulation, accumulation_stats$mean_threshold_accumulation),
    Median_threshold_accumulation = c(overall_stats$median_threshold_accumulation, exploration_stats$median_threshold_accumulation, accumulation_stats$median_threshold_accumulation),
    Std_threshold_accumulation = c(overall_stats$std_threshold_accumulation, exploration_stats$std_threshold_accumulation, accumulation_stats$std_threshold_accumulation),
    SE_threshold_accumulation = c(overall_stats$se_threshold_accumulation, exploration_stats$se_threshold_accumulation, accumulation_stats$se_threshold_accumulation)
  )
  
  
  return(summary_table)
}


createSummaryTable <- function(exploration_data, accumulation_data) {
  
  gem_found_count <- table(summary_data$gem_found)
  overall_mean <- mean(summary_data$round_gem_found, na.rm = TRUE)
  overall_median <- median(summary_data$round_gem_found, na.rm = TRUE)
  
  overall_exceeds_yes <- exploration_data$overall_exceeds_yes + accumulation_data$overall_exceeds_yes
  overall_exceeds_no <- exploration_data$overall_exceeds_no + accumulation_data$overall_exceeds_no
  
  summary_table <- data.frame(
    Phase = c("Overall", "Exploration", "Accumulation"),
    Count = c(gem_found_count[[2]], exploration_data$Count, accumulation_data$Count),
    Mean_round_gem_found = c(overall_mean, exploration_data$Mean_round_gem_found, accumulation_data$Mean_round_gem_found),
    Median_round_gem_found = c(overall_median, exploration_data$Median_round_gem_found, accumulation_data$Median_round_gem_found),
    Exceeds_Gem_Yes = c(overall_exceeds_yes, exploration_data$overall_exceeds_yes, accumulation_data$overall_exceeds_yes),
    Exceeds_Gem_No = c(overall_exceeds_no, exploration_data$overall_exceeds_no, accumulation_data$overall_exceeds_no),
    Mean_length_exploration = c(NA, exploration_data$Mean_length_exploration, accumulation_data$Mean_length_exploration),
    Median_length_exploration = c(NA, exploration_data$Median_length_exploration, accumulation_data$Median_length_exploration),
    Std_length_exploration = c(NA, exploration_data$Std_length_exploration, accumulation_data$Std_length_exploration),
    SE_length_exploration = c(NA, exploration_data$SE_length_exploration, accumulation_data$SE_length_exploration),
    Mean_length_accumulation = c(NA, NA, accumulation_data$Mean_length_accumulation),
    Median_length_accumulation = c(NA, NA, accumulation_data$Median_length_accumulation),
    Std_length_accumulation = c(NA, NA, accumulation_data$Std_length_accumulation),
    SE_length_accumulation = c(NA, NA, accumulation_data$SE_length_accumulation),
    Mean_threshold_accumulation = c(NA, NA, accumulation_data$Mean_threshold_accumulation),
    Median_threshold_accumulation = c(NA, NA, accumulation_data$Median_threshold_accumulation),
    Std_threshold_accumulation = c(NA, NA, accumulation_data$Std_threshold_accumulation),
    SE_threshold_accumulation = c(NA, NA, accumulation_data$SE_threshold_accumulation)
  )
  
  return(summary_table)
}

create_gem_found_exploration <- function(summary_data) {
  
  filtered_data <- summary_data %>%
    filter(!is.na(phase_gem_found), phase_gem_found == "exploration") %>%
    mutate(
      explore_exceeds_gem = ifelse(length_exploration > round_gem_found, 1, 0),
      range_to_gem = pmin(length_exploration, round_gem_found),
      range_beyond_gem = ifelse(length_exploration - round_gem_found > 0, length_exploration - round_gem_found, NA)
    )
  
  round_gem_found_table <- table(filtered_data$round_gem_found)
  length_exploration_table <- table(filtered_data$length_exploration)
  
  list(
    round_gem_found_table = round_gem_found_table,
    round_gem_found_mean = mean(filtered_data$round_gem_found, na.rm = TRUE),
    round_gem_found_median = median(filtered_data$round_gem_found, na.rm = TRUE),
    length_exploration_table = length_exploration_table,
    length_exploration_mean = mean(filtered_data$length_exploration, na.rm = TRUE),
    length_exploration_median = median(filtered_data$length_exploration, na.rm = TRUE),
    filtered_data = filtered_data
  )
}

create_gem_found_accumulation <- function(summary_data) {
  
  filtered_data <- summary_data %>%
    filter(!is.na(phase_gem_found), phase_gem_found == "accumulation") %>%
    mutate(
      explore_and_accumulation_exceeds_gem = ifelse(length_exploration + length_accumulation > round_gem_found, 1, 0)
    )
  
  list(
    round_gem_found_table = table(filtered_data$round_gem_found),
    length_exploration_table = table(filtered_data$length_exploration),
    length_accumulation_table = table(filtered_data$length_accumulation),
    length_exploration_accumulation_table = table(filtered_data$length_exploration + filtered_data$length_accumulation),
    threshold_accumulation_table = table(filtered_data$threshold_accumulation),
    length_exploration_mean = mean(filtered_data$length_exploration, na.rm = TRUE),
    length_exploration_median = median(filtered_data$length_exploration, na.rm = TRUE),
    length_accumulation_mean = mean(filtered_data$length_accumulation, na.rm = TRUE),
    length_accumulation_median = median(filtered_data$length_accumulation, na.rm = TRUE),
    threshold_accumulation_mean = mean(filtered_data$threshold_accumulation, na.rm = TRUE),
    threshold_accumulation_median = median(filtered_data$threshold_accumulation, na.rm = TRUE),
    length_exploration_accumulation_mean = mean(filtered_data$length_exploration + filtered_data$length_accumulation, na.rm = TRUE),
    length_exploration_accumulation_median = median(filtered_data$length_exploration + filtered_data$length_accumulation, na.rm = TRUE),
    filtered_data = filtered_data,
    explore_and_accumulation_exceeds_gem_table = table(filtered_data$explore_and_accumulation_exceeds_gem)
  )
}

create_loc_and_remain_cell <- function(data) {
  
  data <- data %>%
    group_by(player, unique_rounds) %>%
    mutate(
      gem_cell = cell[match(round_gem_found, trial)],
      remain_gem = case_when(
        trial > round_gem_found & cell == gem_cell ~ 1,
        trial > round_gem_found & cell != gem_cell ~ 0,
        TRUE ~ NA
      )
    ) %>%
    ungroup() %>%
    group_by(unique_rounds) %>%
    filter(gem_found == 1, trial > round_gem_found) %>%
    mutate(
      same_cell_sum = sum(remain_gem, na.rm = TRUE)
    ) %>%
    ungroup()
  
  list(
    data = data,
    remain_gem_table = table(data$remain_gem)
  )
}

count_sub_classes <- function(df) {
  sub_class_counts <- lapply(df, function(column) table(column))
  return(sub_class_counts)
}

process_no_gem_data <- function(summary_data, trim_data) {
  
  # Use group_by and summarise to create the summary for player
  player_summary <- summary_data %>%
    group_by(player) %>%
    summarise(
      group = unique(group),
      mean_length_exploitation = mean(length_exploitation, na.rm = TRUE),
      mean_length_exploration = mean(length_exploration, na.rm = TRUE),
      mean_length_accumulation = mean(length_accumulation, na.rm = TRUE),
      mean_threshold_accumulation = mean(threshold_accumulation, na.rm = TRUE),
      mean_threshold_exploitation = mean(threshold_exploitation, na.rm = TRUE),
      mean_copy_social_value = mean(n_copy_social_value),
      mean_tot_points = mean(tot_points)
    )
  
  no_gem_summary_data <- summary_data %>%
    filter(gem_found == 0 | gem_present == 0) %>%
    summarise(
      Count = n(),
      Mean_length_exploration = mean(length_exploration, na.rm = TRUE),
      Median_length_exploration = median(length_exploration, na.rm = TRUE),
      Std_length_exploration = sd(length_exploration, na.rm = TRUE),
      SE_length_exploration = sd(length_exploration, na.rm = TRUE) / sqrt(n()),
      Mean_threshold_accumulation = mean(threshold_accumulation, na.rm = TRUE),
      Median_threshold_accumulation = median(threshold_accumulation, na.rm = TRUE),
      Std_threshold_accumulation = sd(threshold_accumulation, na.rm = TRUE),
      SE_threshold_accumulation = sd(threshold_accumulation, na.rm = TRUE) / sqrt(n()),
      Mean_length_accumulation = mean(length_accumulation, na.rm = TRUE),
      Median_length_accumulation = median(length_accumulation, na.rm = TRUE),
      Std_length_accumulation = sd(length_accumulation, na.rm = TRUE),
      SE_length_accumulation = sd(length_accumulation, na.rm = TRUE) / sqrt(n()),
      Mean_threshold_exploitation = mean(threshold_exploitation, na.rm = TRUE),
      Median_threshold_exploitation = median(threshold_exploitation, na.rm = TRUE),
      Std_threshold_exploitation = sd(threshold_exploitation, na.rm = TRUE),
      SE_threshold_exploitation = sd(threshold_exploitation, na.rm = TRUE) / sqrt(n()),
      Mean_length_exploitation = mean(length_exploitation, na.rm = TRUE),
      Median_length_exploitation = median(length_exploitation, na.rm = TRUE),
      Std_length_exploitation = sd(length_exploitation, na.rm = TRUE),
      SE_length_exploitation = sd(length_exploitation, na.rm = TRUE) / sqrt(n())
    )
  
  no_gem_data <- summary_data %>%
    filter(gem_found == 0 | gem_present == 0)
  
  no_gem_data <- no_gem_data %>%
    mutate(class = case_when(
      (length_exploration > 1 & length_accumulation > 0 & length_exploitation > 0) ~ "All_stages",
      (length_exploration > 1 & length_accumulation > 0 & length_exploitation == 0) ~ "NoExploit",
      (length_exploration == 1 & length_accumulation > 0 & length_exploitation > 0) ~ "NoExploration",
      (length_exploration > 1 & length_accumulation == 0 & length_exploitation == 0) ~ "JustExploration",
      (length_exploration > 1 & length_accumulation == 0 & length_exploitation > 0) ~ "NoAccumulation",
      TRUE ~ "Other"
    ))
  
  no_gem_class_summary_data <- no_gem_data %>%
    group_by(class) %>%
    summarise(
      Count = n(),
      Mean_points= mean(tot_points, na.rm = TRUE),
      Median_points = median(tot_points, na.rm = TRUE),
      Std_points = sd(tot_points, na.rm = TRUE),
      SE_points = sd(tot_points, na.rm = TRUE) / sqrt(n()),
      Mean_length_exploration = mean(length_exploration, na.rm = TRUE),
      Median_length_exploration = median(length_exploration, na.rm = TRUE),
      Std_length_exploration = sd(length_exploration, na.rm = TRUE),
      SE_length_exploration = sd(length_exploration, na.rm = TRUE) / sqrt(n()),
      Mean_threshold_accumulation = mean(threshold_accumulation, na.rm = TRUE),
      Median_threshold_accumulation = median(threshold_accumulation, na.rm = TRUE),
      Std_threshold_accumulation = sd(threshold_accumulation, na.rm = TRUE),
      SE_threshold_accumulation = sd(threshold_accumulation, na.rm = TRUE) / sqrt(n()),
      Mean_length_accumulation = mean(length_accumulation, na.rm = TRUE),
      Median_length_accumulation = median(length_accumulation, na.rm = TRUE),
      Std_length_accumulation = sd(length_accumulation, na.rm = TRUE),
      SE_length_accumulation = sd(length_accumulation, na.rm = TRUE) / sqrt(n()),
      Mean_threshold_exploitation = mean(threshold_exploitation, na.rm = TRUE),
      Median_threshold_exploitation = median(threshold_exploitation, na.rm = TRUE),
      Std_threshold_exploitation = sd(threshold_exploitation, na.rm = TRUE),
      SE_threshold_exploitation = sd(threshold_exploitation, na.rm = TRUE) / sqrt(n()),
      Mean_length_exploitation = mean(length_exploitation, na.rm = TRUE),
      Median_length_exploitation = median(length_exploitation, na.rm = TRUE),
      Std_length_exploitation = sd(length_exploitation, na.rm = TRUE),
      SE_length_exploitation = sd(length_exploitation, na.rm = TRUE) / sqrt(n())
    )
  
  no_gem_class_summary_data <- no_gem_class_summary_data %>%
    mutate(
      Mean_length_accumulation = if_else(class == "NoAccumulation", NA_real_, Mean_length_accumulation),
      Median_length_accumulation = if_else(class == "NoAccumulation", NA_real_, Median_length_accumulation),
      Std_length_accumulation = if_else(class == "NoAccumulation", NA_real_, Std_length_accumulation),
      SE_length_accumulation = if_else(class == "NoAccumulation", NA_real_, SE_length_accumulation),
      Mean_threshold_accumulation = if_else(class == "NoAccumulation", NA_real_, Mean_threshold_accumulation),
      Median_threshold_accumulation = if_else(class == "NoAccumulation", NA_real_, Median_threshold_accumulation),
      Std_threshold_accumulation = if_else(class == "NoAccumulation", NA_real_, Std_threshold_accumulation),
      SE_threshold_accumulation = if_else(class == "NoAccumulation", NA_real_, SE_threshold_accumulation),
      
      Mean_length_exploration = if_else(class == "NoExploration", NA_real_, Mean_length_exploration),
      Median_length_exploration = if_else(class == "NoExploration", NA_real_, Median_length_exploration),
      Std_length_exploration = if_else(class == "NoExploration", NA_real_, Std_length_exploration),
      SE_length_exploration = if_else(class == "NoExploration", NA_real_, SE_length_exploration),
      
      Mean_length_exploitation = if_else(class == "NoExploit", NA_real_, Mean_length_exploitation),
      Median_length_exploitation = if_else(class == "NoExploit", NA_real_, Median_length_exploitation),
      Std_length_exploitation = if_else(class == "NoExploit", NA_real_, Std_length_exploitation),
      SE_length_exploitation = if_else(class == "NoExploit", NA_real_, SE_length_exploitation),
      Mean_threshold_exploitation = if_else(class == "NoExploit", NA_real_, Mean_threshold_exploitation),
      Median_threshold_exploitation = if_else(class == "NoExploit", NA_real_, Median_threshold_exploitation),
      Std_threshold_exploitation = if_else(class == "NoExploit", NA_real_, Std_threshold_exploitation),
      SE_threshold_exploitation = if_else(class == "NoExploit", NA_real_, SE_threshold_exploitation)
    )
  
  # Assume the dataframe is df
  # Filter out JustExploration and Other from class column
  no_gem_class_summary_data <- no_gem_class_summary_data %>% 
    filter(!class %in% c("JustExploration", "Other"))
  
  # Adding 1 to mean_length_accumulation of class NoExploration
  no_gem_class_summary_data <- no_gem_class_summary_data %>% 
    mutate(Mean_length_accumulation = ifelse(class == "NoExploration", Mean_length_accumulation + 1, Mean_length_accumulation))
  
  # Convert from wide to long format
  no_gem_class_summary_data_long <- no_gem_class_summary_data %>% 
    pivot_longer(cols = c("Mean_length_exploration", "Mean_length_accumulation", "Mean_length_exploitation"),
                 names_to = "LengthType", 
                 values_to = "Value")
  
  # Replace NA values with 0
  no_gem_class_summary_data_long[is.na(no_gem_class_summary_data_long)] <- 0
  
  # Set levels for LengthType
  no_gem_class_summary_data_long$LengthType <- factor(no_gem_class_summary_data_long$LengthType, 
                                                      levels = c("Mean_length_exploitation", "Mean_length_accumulation", "Mean_length_exploration"))
  
  # Return the processed data
  return(list(player_summary = player_summary, 
              no_gem_summary_data = no_gem_summary_data, 
              no_gem_data = no_gem_data, 
              no_gem_class_summary_data = no_gem_class_summary_data, 
              no_gem_class_summary_data_long = no_gem_class_summary_data_long))
}
# Use group_by and summarise to create the summary
adjust_nan_lengths <- function(data) {
  data %>%
    mutate(
      length_exploration = ifelse(is.nan(length_exploration), 1, length_exploration),
      length_accumulation = ifelse(is.nan(length_accumulation), 0, length_accumulation),
      length_exploitation = ifelse(is.nan(length_exploitation), 0, length_exploitation)
    )
}
