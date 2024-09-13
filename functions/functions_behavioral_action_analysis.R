calculate_cohens_h <- function(p1, p2) {
  effect_size_h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
  return(effect_size_h)
}

create_action_data <- function(action_data){
  action_data <- action_data %>%
    group_by(player, unique_round) %>%
    mutate(previous_points = lag(points),
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
  ungroup()
}


likelihoods_func <- function(data, social_info_use_val, social_value_cond = NULL) {
  if (social_info_use_val == "ignore") {
    data_filtered <- data %>%
      filter(social_info_use == "ignore", known_cell == FALSE)
  } else {
    data_filtered <- data %>%
      filter(social_info_use == "copy", social_value > social_value_cond, known_cell == FALSE)
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

logistic_likelihoods_func <- function(data, social_info_use_val, social_value_cond = NULL) {
  if (social_info_use_val == "ignore") {
    data_filtered <- data %>%
      filter(social_info_use == "ignore", known_cell == FALSE)
  } else {
    data_filtered <- data %>%
      filter(social_info_use == "copy", social_value > social_value_cond, known_cell == FALSE)
  }
  
  data_filtered %>%
    group_by(previous_color_class) %>%
    summarise(
      total = n(),
      likelihood_success = sum(points_change >= 0) / total,
      likelihood_fail = sum(points_change < 0) / total
      
    ) %>%
    na.omit()
}

plot_logistic_likelihoods <- function(likelihoods, plot_title) {
  # Pivot the data to a longer format
  likelihoods_long <- likelihoods %>%
    pivot_longer(cols = c(likelihood_success,likelihood_fail),
                 names_to = "Change",
                 values_to = "Likelihood")
  
  # Convert Change to a factor and specify level order
  likelihoods_long$Change <- factor(likelihoods_long$Change, 
                                    levels = c("likelihood_success", "likelihood_fail"))
  
  # Create the plot
  ggplot(likelihoods_long, aes(x = as.factor(previous_color_class), y = Likelihood, fill = Change)) +
    geom_bar(stat = "identity", color = "black") +
    labs(x = "Previous Color Class", y = "Likelihood", fill = "Color Class Change",
         title = plot_title) +
    scale_fill_manual(values = c("likelihood_success" = "green", "likelihood_fail" = "orange")) +
    guides(fill = guide_legend(reverse = FALSE)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_diff_logistic_likelihoods <- function(likelihoods, plot_title) {
  # Pivot the data to a longer format
  likelihoods_long <- likelihoods %>%
    pivot_longer(cols = c(likelihood_success,increase_social,likelihood_fail),
                 names_to = "Change",
                 values_to = "Likelihood")
  
  # Convert Change to a factor and specify level order
  likelihoods_long$Change <- factor(likelihoods_long$Change, 
                                    levels = c("likelihood_success","increase_social","likelihood_fail"))
  
  # Create the plot
  ggplot(likelihoods_long, aes(x = as.factor(previous_color_class), y = Likelihood, fill = Change)) +
    geom_bar(stat = "identity", color = "black") +
    labs(x = "Previous Color Class", y = "Likelihood", fill = "Color Class Change",
         title = plot_title) +
    scale_fill_manual(values = c("likelihood_success" = "green","increase_social" = "yellow", "likelihood_fail" = "orange")) +
    guides(fill = guide_legend(reverse = FALSE)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
      filter(social_info_use == "ignore", known_cell == FALSE)
  } else {
    data_filtered <- data %>%
      filter(social_info_use == "copy", social_value > social_value_cond, known_cell == FALSE)
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

create_value_cluster <- function(data) {
  data <- data %>%
    group_by(player) %>%  # Assuming you want row numbers by player. Change as needed.
    mutate(
      social_value_cluster = case_when(
        social_info_use == "copy" & known_cell == FALSE & social_value == 0 ~ 1,
        social_info_use == "copy" & known_cell == FALSE & social_value >= 1 & social_value <= 5 ~ 2,
        social_info_use == "copy" & known_cell == FALSE & social_value > 6 ~ 3,
        TRUE ~ NA_real_
      ),
      first_copy = row_number() == 1 & social_info_use == "copy",
      action_type = case_when(
        social_info_use == "ignore" & known_cell == FALSE ~ "personal_exploration",
        social_info_use == "copy" & known_cell == FALSE & first_copy == TRUE ~ "social_exploration",
        known_cell == TRUE ~ "repeat",
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup()
  
  return(data)
}

# Exploration

##Fit models
fit_percentage_model <- function(data) {
  lm(percentage ~ trial + I(trial^2), data = data)
}

fit_knowledge_model <- function(data) {
  lm(composition_previous_knowledge ~ trial + I(trial^2), data = data)
}

fit_prev_points_model <- function(data) {
  lm(avg_previous_points ~ trial + I(trial^2), data = data)
}

fit_avg_diff_model <- function(data) {
  lm(avg_diff ~ trial, data = data)
}

generate_report <- function(dependent_var, data, use_quadratic = TRUE) {
  formula_linear <- as.formula(paste(dependent_var, "~ trial"))
  
  model_linear <- lm(formula_linear, data = data)
  AIC_linear <- AIC(model_linear)
  
  if (use_quadratic) {
    formula_quad <- as.formula(paste(dependent_var, "~ trial + I(trial^2)"))
    model_quad <- lm(formula_quad, data = data)
    AIC_quad <- AIC(model_quad)
    
    # Determine the better model
    better_model <- ifelse(AIC_quad < AIC_linear, "quadratic", "linear")
  } else {
    better_model <- "linear"
  }
  
  if (better_model == "quadratic") {
    summary_model <- summary(model_quad)
  } else {
    summary_model <- summary(model_linear)
  }
  
  # Extract relevant statistics
  adj_r2 <- summary_model$adj.r.squared
  linear_coef <- summary_model$coefficients["trial", "Estimate"]
  linear_t <- summary_model$coefficients["trial", "t value"]
  
  if (better_model == "quadratic") {
    quad_coef <- summary_model$coefficients["I(trial^2)", "Estimate"]
    quad_t <- summary_model$coefficients["I(trial^2)", "t value"]
  }
  
  # Generate the narrative
  narrative <- sprintf(
    "For %s, a %s model was better (AIC = %.2f) than a linear model (AIC = %.2f), adjusted R^2 = %.2f.",
    dependent_var, better_model, (ifelse(better_model == "quadratic", AIC_quad, NA)), AIC_linear, adj_r2
  )
  
  narrative <- paste(
    narrative,
    sprintf(
      "Linear term: b = %.3f, t(%d) = %.2f.",
      linear_coef, summary_model$df[2], linear_t
    )
  )
  
  if (better_model == "quadratic") {
    narrative <- paste(
      narrative,
      sprintf(
        "Quadratic term: b = %.3f, t(%d) = %.2f.",
        quad_coef, summary_model$df[2], quad_t
      )
    )
  }
  
  return(narrative)
}

plot_personal_exploration_trials <- function(data, model_type = "linear") {
  
  if(model_type == "linear") {
    
    formula <- percentage ~ trial
    
    coef_label <- "Slope ="
    
    coef_value <- coef(lm(formula, data = data))[2]
    
  } else {
    
    formula <- percentage ~ poly(trial, 2)
    
    coef_label <- "Quadratic Term ="
    
    coef_value <- coef(lm(formula, data = data))[3]
    
  }
  
  p <- ggplot(data, aes(x = trial,y = percentage)) +
        geom_point(aes(color = "Data Points"), size = 2.5, shape = 19) +

    geom_col(aes(y = percentage), fill = "steelblue", alpha = 1.7) + # Adjusted alpha for transparency
    geom_line(aes(color = "Linear Fit"), size = 1.2) +
    
    geom_smooth(aes(y = percentage), method = "lm", formula = formula, se = TRUE, color = "red", size = 1.5) +
    
    labs(title="Percentage of Personal Exploration over Trials",
         
         x="Trial",
         
         y="Percentage") +
    
    theme_minimal() +
    
    annotate("text", x = max(data$trial) - 0.8, y = max(data$percentage) - 0.9, 
             
             label = paste(coef_label, round(coef_value, 3)), hjust = 1, color = "red")
  
  print(p)
  
}




plot_effect_previous_exploitation <- function(data) {
  model_quad <- lm(composition_previous_knowledge ~ poly(trial, 2), data = data)
  slope_quad <- coef(model_quad)[3]
  
  p <- ggplot(data, aes(x = trial, y = composition_previous_knowledge)) +
    geom_point(aes(color = "Data Points"), size = 2.5, shape = 19) +
    geom_line(aes(color = "Trend Line"), size = 1.2) +
    geom_smooth(aes(color = "Quadratic Fit"), method = "lm", formula = y ~ poly(x, 2), se = TRUE, fill = "grey70", size = 1.2) +
    labs(title="Effect of Previous Trials from Exploitation on Personal Exploration",
         x="Trial", 
         y="Percentage") +
    theme_light() +
    annotate("text", x = max(data$trial) * 0.8, y = max(data$composition_previous_knowledge) * 0.9, 
             label = paste("Slope =", round(slope_quad, 3)), hjust = 1, color = "red")
  print(p)
}

plot_relationship_previous_points_trials <- function(data, model_type = "quadratic") {
  
  if(model_type == "linear") {
    model <- lm(avg_diff ~ trial, data = data)
    formula <- avg_diff ~ trial
    fit_label <- "Linear Fit"
    coef_label <- "Slope ="
    coef_value <- coef(model)[2]
  } else {
    model <- lm(avg_diff ~ poly(trial, 2), data = data)
    formula <- avg_diff ~ poly(trial, 2)
    fit_label <- "Quadratic Fit"
    coef_label <- "Quadratic Term ="
    coef_value <- coef(model)[3]
  }
  
  p <- ggplot(data, aes(x = trial, y = avg_previous_points)) +
    geom_point(aes(color = "Data Points"), size = 2.5, shape = 19) +
    geom_line(aes(color = "Trend Line"), size = 1.2) +
    geom_smooth(aes(color = "Linear Fit"), method = "lm", se = TRUE, fill = "grey70", size = 1.2) +
    labs(title="Relationship between Previous Points and Trials",
         x="Trial", 
         y="Average Points") +
    theme_light() +
    annotate("text", x = max(data$trial) * 0.8, y = max(data$avg_previous_points) * 0.9, 
             label = paste("Slope =", round(slope_quad, 3)), hjust = 1, color = "red")
  print(p)
}

plot_points_gained_exploration <- function(data) {
  model_linear <- lm(avg_diff ~ trial, data = data)
  slope_linear <- coef(model_linear)[2]
  
  p <- ggplot(data, aes(x = trial, y = avg_diff)) +
    geom_point(aes(color = "Data Points"), size = 2.5, shape = 19) +
    geom_line(aes(color = "Trend Line"), size = 1.2) +
    geom_smooth(aes(color = "Linear Fit"), method = "lm", se = TRUE, fill = "grey70", size = 1.2) +
    labs(title="Points Gained through Exploration per Trial",
         x="Trial", 
         y="Points Gained") +
    theme_light() +
    annotate("text", x = max(data$trial) * 0.8, y = max(data$avg_diff) * 0.9, 
             label = paste("Slope =", round(slope_linear, 3)), hjust = 1, color = "red")
  print(p)
}
