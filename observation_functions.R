

## For heat maps functions

get_first_trials <- function(data, turns_range) {
  first_trials <- data[data$trial %in% turns_range, c("player","trial","unique_rounds","cell","x","y")]
  first_trials_counts <- as.data.frame(table(first_trials$cell))
  first_trials_counts$cell <- as.numeric(as.character(first_trials_counts$Var1))
  first_trials_counts$x <- first_trials_counts$cell %% 8
  first_trials_counts$y <- first_trials_counts$cell %/% 8
  first_trials_counts$rel_Freq <- first_trials_counts$Freq / sum(first_trials_counts$Freq) * nrow(first_trials_counts)
  return(first_trials_counts)
}

get_weighted_metrics <- function(first_trials_counts) {
  w_avg_x <- weighted.mean(first_trials_counts$x - 3.5, first_trials_counts$Freq)
  w_avg_y <- weighted.mean(first_trials_counts$y - 3.5, first_trials_counts$Freq)
  w_std_x <- sqrt(weighted.var(first_trials_counts$x - 3.5, first_trials_counts$Freq))
  w_std_y <- sqrt(weighted.var(first_trials_counts$y - 3.5, first_trials_counts$Freq))
  return(list(w_avg_x = w_avg_x, w_avg_y = w_avg_y, w_std_x = w_std_x, w_std_y = w_std_y))
}

get_matrices <- function(first_trials_counts) {
  mat <- matrix(0, nrow = 8, ncol = 8)
  for(i in 1:nrow(first_trials_counts)) {
    mat[first_trials_counts$y[i] + 1, first_trials_counts$x[i] + 1] <- first_trials_counts$Freq[i]
  }
  return(mat)
}

get_rel_matrix <- function(first_trials_counts) {
  rel_mat <- matrix(0, nrow = 8, ncol = 8)
  for(i in 1:nrow(first_trials_counts)) {
    rel_mat[first_trials_counts$y[i] + 1, first_trials_counts$x[i] + 1] <- first_trials_counts$rel_Freq[i]
  }
  rel_mat <- round(rel_mat, 2)
  return(rel_mat)
}

get_quadrant_freq <- function(mat) {
  quadrant_freq <- c(
    Q1 = sum(mat[1:4, 5:8]),
    Q2 = sum(mat[1:4, 1:4]),
    Q3 = sum(mat[5:8, 1:4]),
    Q4 = sum(mat[5:8, 5:8])
  )
  return(quadrant_freq)
}

get_chisq_test_result <- function(quadrant_freq) {
  chisq_test_result <- chisq.test(quadrant_freq)
  return(chisq_test_result)
}

get_first_move_metrics <- function(w_avg_x, w_avg_y, w_std_x, w_std_y) {
  first_move_metrics <- data.frame(
    Metric = c("avg x", "avg y", "std x", "std y"),
    Value = c(round(w_avg_x, 4), round(w_avg_y, 4), round(w_std_x, 3), round(w_std_y, 3))
  )
  return(first_move_metrics)
}

generate_heatmap <- function(mat, cellnote, title) {
  heatmap.2(mat, trace = "none", dendrogram = "none", Rowv = FALSE, Colv = FALSE,
            margins = c(5, 5), cellnote = cellnote, notecex = 1, notecol = "black",
            main = title)
}

get_joined_data <- function(data, summary_data) {
  joined_data <- data %>%
    left_join(summary_data, by = c("unique_rounds", "player", "round")) %>%
    group_by(unique_rounds) %>%
    filter(trial < trial_exploitation)
  return(joined_data)
}

get_new_data <- function(joined_data) {
  new_data <- joined_data %>%
    count(player, cell) %>%
    group_by(player) %>%
    mutate(rel_Freq = n / sum(n),
           Var1 = as.character(cell),
           x = cell %% 8,
           y = cell %/% 8)
  return(new_data)
}

get_first_trials_counts <- function(new_data) {
  first_trials_counts <- new_data %>%
    group_by(cell, x, y) %>%
    summarise(rel_Freq = sum(rel_Freq), .groups = "drop") %>%
    mutate(rel_Freq = (rel_Freq / n_distinct(new_data$player)) * 64)
  return(first_trials_counts)
}
