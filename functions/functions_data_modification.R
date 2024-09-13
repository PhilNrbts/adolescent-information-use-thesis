# Functions to create merged dataset

## Function to load data and add a 'group' column
load_data <- function(file_path, group_name) {
  data <- read_csv(file = file_path)
  data <- data %>% mutate(group = group_name)
  return(data)
  }


## Function to clean varibles and rows that cannot be used
clean_data <- function(data) {
  ### Remove the 'cells' variable to prevent confusion
  data <- data %>% select(-cells)
  
  ### Remove rows where 'cell', 'env_number', or 'social_info' are NA
  data <- data %>% filter(!is.na(cell), !is.na(env_number), !is.na(social_info))
  
}

## Load the data
explore_data <- load_data("./data/social/data_social_coord.csv", "adults")
explore_data_adolescence <- load_data("./data/social/data_social_coord_schoolbactch_1.csv", "adolescents")

## Clean data
explore_data <- clean_data(explore_data)
explore_data_adolescence <- clean_data(explore_data_adolescence)

## Extend unique_rounds and player for merger later
explore_data$unique_rounds <- explore_data$unique_rounds + max(explore_data_adolescence$unique_rounds)
explore_data$player <- explore_data$player + max(explore_data_adolescence$player)


## Merge the data frames
data <- bind_rows(explore_data, explore_data_adolescence)
write.csv(data, file = "data/social/combined_social_coord.csv")

##########
# Function for the color classes that are present later

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

# Function to process data

process_data <- function(data, trim_beyond_gem = TRUE) {
  
  replace_na_with_nan <- function(data) {
    data %>%
      mutate(round_gem_found = ifelse(is.na(round_gem_found), NaN, round_gem_found))
  }
  
  create_length_phases <- function(data) {
    data %>%
      group_by(player, unique_rounds) %>%
      arrange(unique_rounds, trial) %>%
      mutate(similar = lag(cell) == cell) %>%
      arrange(unique_rounds, desc(trial)) %>%
      mutate(length_exploitation = match(FALSE, similar, nomatch = max(trial)) - 1) %>%
      arrange(unique_rounds, trial) %>%
      mutate(length_exploration = if_else(is.na(match(TRUE, duplicated(cell))), 25, match(TRUE, duplicated(cell), nomatch = max(trial)) - 1))  %>%
      mutate(length_accumulation = 25 - length_exploration - length_exploitation)
  }
  
  create_phases <- function(data) {
    data <- data %>%
      group_by(player, unique_rounds) %>%
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
      group_by(player, unique_rounds) %>%
      mutate(
        trial_exploitation = 26 - length_exploitation,
        trial_exploitation = if_else(trial_exploitation > 25, NaN, trial_exploitation),
        trial_exploitation = if_else(length_exploitation == 0, NaN, trial_exploitation),
        
        trial_accumulation = length_exploration + 1,
        trial_accumulation = if_else(trial_accumulation > 25, NaN, trial_accumulation),
        trial_accumulation = if_else(length_accumulation == 0, NaN, trial_accumulation),
        
        threshold_accumulation = points[match(length_exploration, trial)],
        threshold_accumulation = if_else(length_accumulation == 0, NaN, threshold_accumulation),
        
        threshold_exploitation = points[match(25 - length_exploitation, trial)],
        threshold_exploitation = if_else(length_exploitation == 0, NaN, threshold_exploitation)
      ) %>%
      ungroup()
    
    return(data)
  }
  
  create_variables_on_round_cell_history <- function(data) {
    data %>%
      group_by(cell, .add = TRUE) %>%
      arrange(unique_rounds, trial) %>%
      mutate(visited_this_round = cumsum(duplicated(cell))) %>%
      mutate(known_cell = visited_this_round > 0) %>%
      mutate(n_unique_cells = n_distinct(cell)) %>%
      ungroup()
  }
  
  # Function to adjust data by removing trials after gem was found
  trim_gem_trials <- function(data, trim_beyond_gem) {
    # Group by 'player' and 'unique_rounds'
    data <- data %>%
      group_by(player, unique_rounds) %>%
      
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
    
    data <- data %>%
      group_by(player, unique_rounds) %>%
      # Conditionally filter rows where 'trial' is less than or equal to 'round_gem_found' + 1 or 'round_gem_found' is NA
      filter(ifelse(trim_beyond_gem, trial <= round_gem_found + 1 | is.na(round_gem_found), TRUE)) %>%
      # Ungroup the data
      ungroup()
    
    # Return the processed data
    return(data)
  }
  
  data <- replace_na_with_nan(data)
  data <- create_length_phases(data)
  data <- create_phases(data)
  data <- create_thresholds(data)
  data <- create_variables_on_round_cell_history(data)
  
  data <- data %>%
    mutate(color_class = calc_color_class(points))
  
  data <- trim_gem_trials(data, trim_beyond_gem)
  
  return(data)
}

# Function to create a summary file

create_summary_data <- function(data) {
  summary_data <- data %>%
    select(unique_rounds, player, group, performance_group_f, round, env_number, tot_points, gempresent, gem_found, round_gem_found, n_unique_cells, length_exploitation, length_exploration, length_accumulation, threshold_accumulation, threshold_exploitation, trial_accumulation, trial_exploitation, phase_gem_found) %>%
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
    arrange(player, round, unique_rounds, trial) %>%
    group_by(player, round, unique_rounds) %>%
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
    group_by(unique_rounds) %>%
    summarise(
      n_copy_no_social_value = sum(social_info_use == "copy" & social_value == 0 & visited_this_round == 0),
      n_copy_social_value = sum(social_info_use == "copy" & social_value > 0 & visited_this_round == 0),
      mean_social_value = mean(social_value[social_info_use == "copy" & social_value > 0 & known_cell == FALSE], na.rm = TRUE),
      median_social_value = median(social_value[social_info_use == "copy" & social_value > 0 & known_cell == FALSE], na.rm = TRUE),
      median_points_per_trial = median(points, na.rm = TRUE)
    )
  
  # Merge new variables with summary_data
  updated_summary_data <- left_join(summary_data, new_variables, by = "unique_rounds")
  
  return(updated_summary_data)
}