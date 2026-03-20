library(tidyverse)
library(tidymodels)
library(ggdendro)
library(heatmaply)
library(htmlwidgets)

wrapped_2025 <- read_csv("songs_csv/wrapped_2025.csv")

dir.create("plots_clustering", showWarnings = FALSE)

wrapped_small <- wrapped_2025 |>
  select(
    `Track Name`,
    `Artist Name(s)`,
    Danceability,
    Energy,
    Loudness,
    Speechiness,
    Acousticness,
    Instrumentalness,
    Liveness,
    Valence,
    Tempo,
    `Duration (ms)`
  ) |>
  drop_na()

wrapped_rec <- recipe(
  ~ Danceability + Energy + Loudness + Speechiness +
    Acousticness + Instrumentalness + Liveness +
    Valence + Tempo + `Duration (ms)`,
  data = wrapped_small
) |>
  step_center(all_numeric_predictors()) |>
  step_scale(all_numeric_predictors())

wrapped_prep <- wrapped_rec |>
  prep()

wrapped_baked <- wrapped_prep |>
  bake(new_data = NULL) |>
  mutate(label = paste(wrapped_small$`Track Name`, wrapped_small$`Artist Name(s)`, sep = " - "))

wrapped_mat <- wrapped_baked |>
  column_to_rownames("label")

wrapped_dist <- wrapped_mat |>
  dist(method = "euclidean")

wrapped_single <- wrapped_dist |>
  hclust(method = "single")

wrapped_average <- wrapped_dist |>
  hclust(method = "average")

wrapped_complete <- wrapped_dist |>
  hclust(method = "complete")

plot_single <- ggdendrogram(wrapped_single) +
  labs(title = "Spotify Wrapped 2025 - Single linkage") +
  theme_minimal()

plot_average <- ggdendrogram(wrapped_average) +
  labs(title = "Spotify Wrapped 2025 - Average linkage") +
  theme_minimal()

plot_complete <- ggdendrogram(wrapped_complete) +
  labs(title = "Spotify Wrapped 2025 - Complete linkage") +
  theme_minimal()

plot_single
plot_average
plot_complete

ggsave(
  "plots_clustering/wrapped_single_linkage.png",
  plot = plot_single,
  width = 12,
  height = 8,
  dpi = 300
)

ggsave(
  "plots_clustering/wrapped_average_linkage.png",
  plot = plot_average,
  width = 12,
  height = 8,
  dpi = 300
)

ggsave(
  "plots_clustering/wrapped_complete_linkage.png",
  plot = plot_complete,
  width = 12,
  height = 8,
  dpi = 300
)

wrapped_heatmap <- heatmaply(
  wrapped_mat,
  Rowv = as.dendrogram(wrapped_complete),
  scale = "none"
)

wrapped_heatmap

htmlwidgets::saveWidget(
  wrapped_heatmap,
  "plots_clustering/wrapped_heatmap_complete.html",
  selfcontained = TRUE
)