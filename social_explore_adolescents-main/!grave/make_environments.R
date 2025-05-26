# this script generates environment files for based on some specs
# setup range and variances
# NOT SURE WHICH ONES WE USED IN THE EXPERIMEN TNOW
library(jsonlite)
library(tidyverse)

envirionmentMeanAdol <- seq(-50, 50, length.out = 8)
envirionmentVarianceAdol <- rep(c(2, 10, 15, 25), length.out = 8)
dat_social <- expand.grid(x1 = 1:8, x2 = 1:8)

# number of hidden gems
gem_envs <- list()
# make gem environments
for (i in 1:6) {
  # shuffle environment
  EnvirionemntAdol <- expand.grid(Mean = envirionmentMeanAdol, Variance = envirionmentVarianceAdol) %>%
    .[sample(1:nrow(.)), ]
  index <- sample(x = 1:length(EnvirionemntAdol[, 1]), size = 2)
  Gem <- rnorm(2, mean = 150, sd = 10)
  EnvirionemntAdol[index, 1] <- Gem
  gem_envs[[i]] <- EnvirionemntAdol %>%
    mutate(env_idx = i) %>%
    mutate(x = dat_social$x1, y = dat_social$x2)
} # to be nested for SI - no SI

no_gem_envs <- list()
# make no gem environment
for (i in 1:6) {
  # shuffle environment
  EnvirionemntAdol <- expand.grid(Mean = envirionmentMeanAdol, Variance = envirionmentVarianceAdol) %>%
    .[sample(1:nrow(.)), ]
  no_gem_envs[[i]] <- EnvirionemntAdol %>%
    mutate(env_idx = i) %>%
    mutate(x = dat_social$x1, y = dat_social$x2)
} # to be nested for SI - no SI

# make sure not to overwrite

# gems
gem_json1 <- gem_envs[[1]] %>% toJSON(dataframe = "columns")
gem_json2 <- gem_envs[[2]] %>% toJSON(dataframe = "columns")
gem_json3 <- gem_envs[[3]] %>% toJSON(dataframe = "columns")
gem_json4 <- gem_envs[[4]] %>% toJSON(dataframe = "columns")
gem_json5 <- gem_envs[[5]] %>% toJSON(dataframe = "columns")
gem_json6 <- gem_envs[[6]] %>% toJSON(dataframe = "columns")



gem_json <- cat('{"1":', gem_json1,
  ',"2":', gem_json2,
  ',"3":', gem_json3,
  ',"4":', gem_json4,
  ',"5":', gem_json5,
  ',"6":', gem_json6,
  "}",
  sep = " ",
  file = "environments_gem_150_var25max.json"
)

# no gems
no_gem_json1 <- no_gem_envs[[1]] %>% toJSON(dataframe = "columns")
no_gem_json2 <- no_gem_envs[[2]] %>% toJSON(dataframe = "columns")
no_gem_json3 <- no_gem_envs[[3]] %>% toJSON(dataframe = "columns")
no_gem_json4 <- no_gem_envs[[4]] %>% toJSON(dataframe = "columns")
no_gem_json5 <- no_gem_envs[[5]] %>% toJSON(dataframe = "columns")
no_gem_json6 <- no_gem_envs[[6]] %>% toJSON(dataframe = "columns")

cat('{"1":', no_gem_json1,
  ',"2":', no_gem_json2,
  ',"3":', no_gem_json3,
  ',"4":', no_gem_json4,
  ',"5":', no_gem_json5,
  ',"6":', no_gem_json6,
  "}",
  sep = " ",
  file = "environments_no_gem_var25max.json"
)




# make plots

environments_with_gems <- gem_envs %>%
  do.call(rbind, .) %>%
  dplyr::rowwise() %>%
  mutate(value = rnorm(1, mean = Mean, sd = Variance)) %>%
  ggplot(aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_continuous_divergingx(palette = "Spectral", mid = 0) +
  ggtitle("Environ with Gems") +
  facet_wrap(~env_idx, scales = "free") +
  theme_minimal(14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1)
ggsave(environments_with_gems, filename = "environments_with_gem.png")

environments_without_gems <- no_gem_envs %>%
  do.call(rbind, .) %>%
  dplyr::rowwise() %>%
  mutate(value = rnorm(1, mean = Mean, sd = Variance)) %>%
  ggplot(aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_continuous_divergingx(palette = "Spectral", mid = 0) +
  ggtitle("Environ without Gems") +
  facet_wrap(~env_idx, scales = "free") +
  theme_minimal(14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1)
