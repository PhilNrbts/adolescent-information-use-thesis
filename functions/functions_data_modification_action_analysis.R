# 1. Import raw data


select_important_variables <- function(data) {
  data %>%
    # Recode social_info
    mutate(social_info = if_else(social_info == 64, 0, social_info)) %>%
    # Modify social_info_use based on condition
    mutate(social_info_use = if_else(cell == social_info, "copy", "ignore")) %>%
    # Set gender values greater than 2 to NA
    mutate(gender = if_else(gender > 2, NA_real_, gender)) %>%
    # Select and rename columns
    
    select(
      player = uniqueID,
      gender,
      group,
      unique_round = unique_rounds,
      round,
      trial,
      tot_points,
      points,
      cell,
      social_info,
      social_info_use,
      gem_present = gempresent,
      gem_selected = gem,
      round_gem_found,
      gem_found
    )
}

check_for_clean <- function(data) {
  
  # Extract rows with missing values
  miss <- data %>% 
    dplyr::filter(is.na(cell) | is.na(player) | is.na(unique_round) | 
             is.na(gem_found) | is.na(social_info))
  
  # Check if there are any missing rows
  if (nrow(miss) > 0) {
    print(paste(nrow(miss), "rows excluded. Call 'miss' dataframe to check."))
    return(miss)
  } else {
    return("No missing data.")
  }
}

clean_data <- function(data) {
  data %>% filter(!is.na(cell), !is.na(player), !is.na(unique_round), 
                  !is.na(gem_found), !is.na(social_info))
}

# 2. Process selection of data to create the actual data set

## Function for the color classes that are present later

calc_color_class <- function(points) {
  color_class <- case_when(
    is.na(points) | is.nan(points) ~ NA_real_,
    points < -60 ~ -7,
    points >= -60 & points < -40 ~ -5,
    points >= -40 & points < -20 ~ -3,
    points >= -20 & points < 0 ~ -1,
    points >= 0 & points < 20 ~ 1,
    points >= 20 & points < 40 ~ 3,
    points >= 40 & points < 60 ~ 5,
    points >= 60 & points < 150 ~ 7,
    points >= 150 ~ 9
  ) / 2
  return(color_class)
}

## Function to process data

process_data <- function(data, trim_beyond_gem = TRUE) {
  
  replace_na_with_nan <- function(data) {
    data %>%
      mutate(round_gem_found = ifelse(is.na(round_gem_found), NaN, round_gem_found))
  }
  
  create_length_phases <- function(data) {
    data %>%
      group_by(player, unique_round) %>%
      arrange(unique_round, trial) %>%
      mutate(similar = lag(cell) == cell) %>%
      arrange(unique_round, desc(trial)) %>%
      mutate(length_exploitation = as.numeric(ifelse(!is.nan(round_gem_found) & round_gem_found > 0, 0, match(FALSE, similar, nomatch = max(trial)) - 1))) %>%
      arrange(unique_round, trial) %>%
      mutate(length_exploration = if_else(is.na(match(TRUE, duplicated(cell))), 25, match(TRUE, duplicated(cell), nomatch = max(trial)) - 1)) %>%
      mutate(length_accumulation = ifelse(!is.nan(round_gem_found) & round_gem_found > 0, round_gem_found - length_exploration, 25 - length_exploration - length_exploitation))
  }
  
  create_phases <- function(data) {
    data <- data %>%
      group_by(player, unique_round) %>%
      mutate(
        phases = case_when(
          trial <= length_exploration ~ "exploration",
          trial > length_exploration & trial <= (25 - length_exploitation) ~ "accumulation",
          trial > (25 - length_exploitation) ~ "exploitation",
          TRUE ~ "other" # Any other case
        ),
        phases = if_else(!is.na(round_gem_found) & trial > round_gem_found, "gem", phases)
      ) %>%
      ungroup()
    return(data)
  }
  
  create_thresholds <- function(data) {
    data <- data %>%
      group_by(player, unique_round) %>%
      mutate(
        
        threshold_accumulation = points[match(length_exploration, trial)],
        threshold_accumulation = if_else(length_accumulation == 0, NaN, threshold_accumulation),
        threshold_exploitation = points[match(25 - length_exploitation, trial)],
        threshold_exploitation = if_else(length_exploitation == 0, NaN, threshold_exploitation),
        
        trial_exploitation = 26 - length_exploitation,
        trial_exploitation = if_else(trial_exploitation > 25, NaN, trial_exploitation),
        trial_exploitation = if_else(length_exploitation == 0, NaN, trial_exploitation),
        
        trial_accumulation = length_exploration + 1,
        trial_accumulation = if_else(trial_accumulation > 25, NaN, trial_accumulation),
        trial_accumulation = if_else(length_accumulation == 0, NaN, trial_accumulation),
        
        threshold_exit_exploration = points[match(length_exploration, trial)],
        trial_exit_exploration = length_exploration,
        threshold_exit_accumulation = points[match(length_exploration + length_accumulation, trial)],
        threshold_exit_accumulation = if_else(length_exploration + length_accumulation >= 25, NaN, threshold_exit_accumulation),
        trial_exit_accumulation = if_else(length_exploration + length_accumulation >= 25, NaN, length_exploration + length_accumulation)
        
      ) %>%
      ungroup()
    
    return(data)
  }
  
  count_unique_cells <- function(data) {
    data <- data %>%
      group_by(player, unique_round) %>%
      # Number unique cells visited this round
      mutate(n_unique_cells = n_distinct(cell))
  }
  
  create_variables_on_round_cell_history <- function(data) {
    data %>%
      group_by(cell, .add = TRUE) %>%
      arrange(unique_round, trial) %>%
      mutate(visited_this_round = cumsum(duplicated(cell))) %>%
      mutate(known_cell = visited_this_round > 0) %>%
      ungroup()
  }
  
  # Function to adjust data by removing trials after gem was found
  trim_gem_trials <- function(data, trim_beyond_gem) {
    # Group by 'player' and 'unique_round'
    data <- data %>%
      group_by(player, unique_round) %>%
      
      # Create 'phase_gem_found' column based on conditions related to 'gem_found' and 'round_gem_found'
      mutate(
        phase_gem_found = case_when(
          gem_found > 0 & round_gem_found <= length_exploration ~ "exploration",
          gem_found > 0 & round_gem_found > length_exploration ~ "accumulation",
          TRUE ~ NA_character_ # Any other case
        )
      ) %>%
      
      # Update several columns based on the value of 'phase_gem_found'
      mutate(
        trial_accumulation = case_when(
          phase_gem_found == "exploration" ~ NaN,
          TRUE ~ trial_accumulation
        ),
        trial_exploitation = case_when(
          phase_gem_found %in% c("exploration", "accumulation") ~ NaN,
          TRUE ~ trial_exploitation
        ),
        threshold_accumulation = case_when(
          phase_gem_found == "exploration" ~ NaN,
          TRUE ~ threshold_accumulation
        ),
        threshold_exploitation = case_when(
          phase_gem_found %in% c("exploration", "accumulation") ~ NaN,
          TRUE ~ threshold_exploitation
        ),
        length_accumulation = case_when(
          phase_gem_found == "exploration" ~ NaN,
          TRUE ~ length_accumulation
        ),
        length_exploitation = case_when(
          phase_gem_found %in% c("exploration", "accumulation") ~ NaN,
          TRUE ~ length_exploitation
        )
      ) %>%
      ungroup() 
    
    if (trim_beyond_gem) {
    data <- data %>%
      group_by(player, unique_round) %>%
      # Conditionally filter rows where 'trial' is less than or equal to 'round_gem_found' + 1 
      # and 'round_gem_found' is not NA
      filter(trim_beyond_gem & (trial < round_gem_found + 1 | is.na(round_gem_found)))  %>%     
      # Ungroup the data
      ungroup()
    }
    # Return the processed data
    return(data)
  }
  
  data <- replace_na_with_nan(data)
  data <- create_length_phases(data)
  data <- create_phases(data)
  data <- create_thresholds(data)
  data <- count_unique_cells(data)
  data <- create_variables_on_round_cell_history(data)
  
  data <- data %>%
    mutate(color_class = calc_color_class(points))
  
  data <- trim_gem_trials(data, trim_beyond_gem)
  data <- data %>% 
  return(data)
}

# Function to create a summary file

create_summary_data <- function(data) {
  summary_data <- data %>%
    mutate(
      n_copy = ifelse(social_info_use == "copy", 1, 0)
    ) %>%
    group_by(unique_round) %>%
    mutate(
      total_n_copy = sum(n_copy)
    ) %>%
    ungroup() %>%
    select(unique_round, player, gender, group, round, tot_points, gem_present, gem_found, round_gem_found, n_unique_cells, length_exploitation, length_exploration, length_accumulation, threshold_accumulation, threshold_exploitation, trial_accumulation, trial_exploitation, phase_gem_found, total_n_copy) %>%
    distinct() %>%
    mutate(
      threshold_accumulation_color_class = calc_color_class(threshold_accumulation),
      threshold_exploitation_color_class = calc_color_class(threshold_exploitation)
    )
  
  return(summary_data)
}


# Function to determine meaningful social_information and add copy variables

determine_social_information <- function(data) {
  
  data <- data %>%
    arrange(player, round, unique_round, trial) %>%
    group_by(player, round, unique_round) %>%
    mutate(similar_social_info = lag(social_info) == social_info,
           similar_social_info = replace_na(similar_social_info, FALSE)) %>%
    mutate(social_value = cumsum(similar_social_info)) %>%
    mutate(n_copy_no_social_value = sum(social_info_use == "copy" & social_value == 0 & visited_this_round == 0),
           n_copy_social_value = sum(social_info_use == "copy" & social_value > 0 & visited_this_round == 0)) %>%
    ungroup()
  
  return(data)
}

# Function to add new variables to summary_data

add_social_value_variables <- function(data, summary_data) {
  
  new_variables <- data %>%
    group_by(unique_round) %>%
    summarise(
      n_copy_no_social_value = sum(social_info_use == "copy" & social_value == 0 & visited_this_round == 0),
      n_copy_social_value = sum(social_info_use == "copy" & social_value > 0 & visited_this_round == 0),
      mean_social_value = mean(social_value[social_info_use == "copy" & social_value > 0 & known_cell == FALSE], na.rm = TRUE),
      median_social_value = median(social_value[social_info_use == "copy" & social_value > 0 & known_cell == FALSE], na.rm = TRUE),
      mean_trial_social_value = mean(trial[social_info_use == "copy" & social_value > 0 & known_cell == FALSE], na.rm = TRUE),
      median_trial_social_value = median(trial[social_info_use == "copy" & social_value > 0 & known_cell == FALSE], na.rm = TRUE),
      median_points_per_trial = median(points, na.rm = TRUE)
    )
  
  # Merge new variables with summary_data
  updated_summary_data <- left_join(summary_data, new_variables, by = "unique_round")
  
  return(updated_summary_data)
}


# 3. Create action data for action move analyses

## Create action data with previous moves and its difference to the current move

create_action_data <- function(action_data){
  action_data <- action_data %>%
    group_by(player, unique_round) %>%
    mutate(previous_points = lag(points),
           max_points_so_far = ifelse(trial == 1, NA, cummax(lag(points, default = first(points)))),
           previous_knowledge = lag(known_cell),
           points_change = points - previous_points,
           previous_color_class = lag(color_class),
           color_class_change = color_class - previous_color_class,
           color_class_change_category = case_when(
             color_class_change > 0 ~ "increase",
             color_class_change == 0 ~ "same",
             color_class_change < 0 ~ "decrease",
             TRUE ~ NA_character_ # For NA values in color_class_change
           )
    ) %>%
    # Create the first_copy column. No rows are filtered out.
    arrange(unique_round, player, trial) %>%
    group_by(unique_round, player, cell) %>%
    mutate(first_copy = ifelse(row_number() == 1 & social_info_use == "copy", TRUE, FALSE)) %>%
    ungroup()  %>%
    
    # Create the action_type column
    mutate(
      action_type = case_when(
        social_info_use == "ignore" & known_cell == FALSE ~ "personal_exploration",
        social_info_use == "copy" & known_cell == FALSE & first_copy == TRUE ~ "social_exploration",
        known_cell == TRUE ~ "repeat",
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup()
  
  return(action_data)
}

clean_action_data <- function(data) {
  data <- data %>% filter(!is.na(cell), !is.na(player), !is.na(unique_round), 
                          !is.na(gem_found), !is.na(social_info))
  return(data)
}

## Though determined in analyse, still fit here for structure

add_social_cluster <- function(data) {
  data <- data %>%
    mutate(
    social_value_cluster = case_when(
      social_info_use == "copy" & known_cell == FALSE & social_value > -1 & social_value == 0 ~ 1,
      social_info_use == "copy" & known_cell == FALSE & social_value > -1 & social_value >= 1 & social_value <= 5 ~ 2,
      social_info_use == "copy" & known_cell == FALSE & social_value > -1 & social_value > 6 ~ 3,
      TRUE ~ NA_real_
    )
  )
  return(data)
}

## stuff for points

create_player_summary <- function(data, gem_found_value) {
  if (gem_found_value != 2) {
    data <- data %>%
      filter(gem_found == gem_found_value)
  }
  
  summary_data <- data %>%
    group_by(player, group, gender) %>%
    summarise(
      mean_tot_points = mean(tot_points, na.rm = TRUE),
      n_gem = sum(gem_found, na.rm = TRUE),
      mean_n_copy = mean(total_n_copy, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(summary_data)
}

## a. Functions to help create personal exploration data better

### Compute mean and sum values grouped by player and group
create_player_pers_explore_summary <- function(data) {
  data %>%
    group_by(player, group) %>%
    summarise(
      mean_previous_points = mean(previous_points, na.rm = TRUE),
      mean_points = mean(points, na.rm = TRUE),
      mean_diff_points = mean(points_change, na.rm = TRUE),
      sum_exploration = sum(previous_points, na.rm = TRUE) # I noticed there was an empty sum() in the original, so I assumed you meant to sum 'previous_points'. Adjust if needed.
    )
}

### Compute average and standard deviation of previous_points, and other metrics grouped by trial
create_average_pers_explore_summary <- function(data) {
  data %>%
    group_by(trial) %>%
    summarise(
      avg_previous_points = mean(previous_points, na.rm = TRUE),
      sdavg_previous_points = sd(previous_points, na.rm = TRUE),
      avg_diff = mean(points_change, na.rm = TRUE),
      composition_previous_knowledge = sum(previous_knowledge) / (n() - sum(previous_knowledge))
    )
}

### Compute count and percentage grouped by trial
create_trial_pers_explore_summary <- function(data) {
  data %>%
    group_by(trial) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100)
}


## b. social exploration

### Compute mean values grouped by player and group
create_player_soc_explore_summary <- function(data) {
  data %>%
    group_by(player, group) %>%
    summarise(
      mean_previous_points = mean(previous_points, na.rm = TRUE),
      mean_points = mean(points, na.rm = TRUE),
      mean_diff_points = mean(points_change, na.rm = TRUE)
    )
}

### Compute average and standard deviation of previous_points, and other metrics grouped by trial
create_average_soc_explore_summary <- function(data) {
  data %>%
    group_by(trial) %>%
    summarise(
      avg_previous_points = mean(previous_points, na.rm = TRUE),
      sdavg_previous_points = sd(previous_points, na.rm = TRUE),
      avg_diff = mean(points_change, na.rm = TRUE),
      composition_previous_knowledge = sum(previous_knowledge) / (n() - sum(previous_knowledge))
    )
}

### Compute count, mean_social_value, and percentage grouped by trial
create_trial_soc_explore_summary <- function(data) {
  data %>%
    group_by(trial) %>%
    summarise(
      count = n(),
      mean_social_value = mean(social_value, na.rm = TRUE)
    ) %>%
    mutate(percentage = count / sum(count) * 100)
}

## c. repeat action

### Add initial color class for each cell in each unique round
### Calculate the color class change and classify it as 'increase', 'decrease', or 'same'
create_repeat_data <- function(data) {
  
  # Group by unique_round and cell, then compute initial values
  data <- data %>%
    group_by(unique_round, cell) %>%
    mutate(
      initial_cell_color_class = first(color_class),
      initial_cell_points = first(points),
      initial_cell_trial = first(trial)
    ) %>%
    ungroup()
  
  # Compute differences for repeated choices
  data <- data %>%
    mutate(
      repeated_choice_class_diff = color_class - initial_cell_color_class,
      repeated_choice_trial_diff = trial - initial_cell_trial,
      repeated_choice_points_diff = points - initial_cell_points,
      repeated_choice_class_diff_category = case_when(
        repeated_choice_class_diff > 0 ~ "increase",
        repeated_choice_class_diff == 0 ~ "same",
        repeated_choice_class_diff < 0 ~ "decrease"
      )
    )
  
  # Calculate repeat_length
  data <- data %>%
    group_by(unique_round, cell) %>%
    mutate(repeat_length = ifelse(phases == "gem", NaN, max(repeated_choice_trial_diff, na.rm = TRUE))) %>%
    ungroup()
  
  return(data)
}

### Creates a summary 
create_repeat_summary <- function(data) {
  summary_data <- data %>%
    select(unique_round, player, group, round,
           initial_cell_color_class, initial_cell_points, initial_cell_trial, repeat_length) %>%
    distinct()
  
  return(summary_data)
}

### Creates average measures for each repetition length
create_average_measures_repeated <- function(data) {
  avg_data <- data %>%
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
  
  return(avg_data)
}


create_repetition_data <- function(action_data) {
  repetition_data <- action_data %>%
    group_by(unique_round, cell) %>%
    mutate(initial_cell_color_class = first(color_class),
           initial_cell_points = first(points),
           initial_cell_trial = first(trial)) %>%
    ungroup()
  
  repetition_data <- repetition_data %>%
    mutate(repeated_choice_class_diff = color_class - initial_cell_color_class,
           repeated_choice_trial_diff = trial - initial_cell_trial,
           repeated_choice_points_diff = points - initial_cell_points,
           repeated_choice_class_diff_category = case_when(
             repeated_choice_class_diff > 0 ~ "increase",
             repeated_choice_class_diff == 0 ~ "same",
             repeated_choice_class_diff < 0 ~ "decrease"))
  
  repetition_data <- repetition_data %>%
    group_by(unique_round, cell) %>%
    mutate(repeat_length = ifelse(phases == "gem", NaN, max(repeated_choice_trial_diff, na.rm = TRUE))) %>%
    ungroup()
  
  return(repetition_data)
}










adjust_nan_lengths <- function(data) {
  data %>%
    mutate(
      length_exploration = ifelse(is.nan(length_exploration), 1, length_exploration),
      length_accumulation = ifelse(is.nan(length_accumulation), 0, length_accumulation),
      length_exploitation = ifelse(is.nan(length_exploitation), 0, length_exploitation)
    )
}