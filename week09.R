library(tidyverse)
library(compmus)

# Clocks - Coldplay Chromagram Euclidian
chroma_raw <- read_csv("songs_csv/clocks_chroma.csv")
mfcc_raw <- read_csv("songs_csv/clocks_MFCC.csv")

chroma_plot_euclidian <- chroma_raw |>
  compmus_wrangle_chroma() |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |>
  ggplot(aes(
    x = start + duration / 2,
    width = duration,
    y = pitch_class,
    fill = value
  )) +
  geom_tile() +
  labs(
    title = "Clocks (Coldplay) - Chromagram",
    x = "Time (s)", y = NULL, fill = "Magnitude"
  ) +
  theme_minimal() +
  scale_fill_viridis_c()

chroma_plot_euclidian

dir.create("plots", showWarnings = FALSE)
ggsave("plots/clocks_chromagram_euclidian.png", chroma_plot_euclidian, width = 10, height = 4, dpi = 300)


# Clocks - Coldplay Chromagram Manhattan

chroma_plot_manhattan <- chroma_raw |>
  compmus_wrangle_chroma() |>
  mutate(pitches = map(pitches, compmus_normalise, "manhattan")) |>
  compmus_gather_chroma() |>
  ggplot(aes(
    x = start + duration / 2,
    width = duration,
    y = pitch_class,
    fill = value
  )) +
  geom_tile() +
  labs(
    title = "Clocks (Coldplay) - Chromagram Manhattan",
    x = "Time (s)", y = NULL, fill = "Magnitude"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(0, 300, by = 20))

chroma_plot_manhattan

dir.create("plots", showWarnings = FALSE)
ggsave("plots/clocks_chromagram_manhattan.png", chroma_plot_manhattan, width = 10, height = 4, dpi = 300)

# Clocks - Coldplay - Chromagram Chebyshev

chroma_plot_chebyshev <- chroma_raw |>
  compmus_wrangle_chroma() |>
  mutate(pitches = map(pitches, compmus_normalise, "chebyshev")) |>
  compmus_gather_chroma() |>
  ggplot(aes(
    x = start + duration / 2,
    width = duration,
    y = pitch_class,
    fill = value
  )) +
  geom_tile() +
  labs(
    title = "Clocks (Coldplay) - Chromagram Chebyshev",
    x = "Time (s)", y = NULL, fill = "Magnitude"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(0, 300, by = 20))

chroma_plot_chebyshev

dir.create("plots", showWarnings = FALSE)
ggsave("plots/clocks_chromagram_chebyshev.png", chroma_plot_chebyshev, width = 10, height = 4, dpi = 300)


# Clocks - Coldplay - Cepstogram

cepstro_plot <- mfcc_raw |>
  compmus_wrangle_timbre() |> 
  mutate(timbre = map(timbre, compmus_normalise, "manhattan")) |>
  compmus_gather_timbre() |>
  ggplot(aes(x = start + duration/2, width = duration, y = mfcc, fill = value)) +
  geom_tile() +
  labs(title="Clocks (Coldplay) - Cepstrogram (Manhattan)", x="Time (s)", y=NULL, fill="Magnitude") +
  scale_fill_viridis_c() +
  theme_classic()

cepstro_plot
ggsave("plots/clocks_cepstrogram.png", cepstro_plot, width = 10, height = 4, dpi = 300)


# Clocks - Coldplay - Timbre SSM

timbre_ssm_plot <- mfcc_raw |>
  compmus_wrangle_timbre() |> 
  filter(row_number() %% 100L == 0L) |> 
  mutate(timbre = map(timbre, compmus_normalise, "manhattan")) |>
  compmus_self_similarity(timbre, "cosine") |> 
  ggplot(
    aes(
    x = xstart + xduration / 2,
    width  = 100 * xduration,
    y = ystart + yduration / 2,
    height = 100 * yduration,
    fill = d
  )) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(
    title = "Clocks (Coldplay) - Self-similarity (timbre)",
    x = "", y = ""
  )

timbre_ssm_plot
ggsave("plots/clocks_ssm_timbre.png", timbre_ssm_plot, width = 6, height = 6, dpi = 300)

# Clocks - Coldplay - Chroma SSM

chroma_ssm_plot <- chroma_raw |>
  compmus_wrangle_chroma() |> 
  filter(row_number() %% 25L == 0L) |> 
  mutate(pitches = map(pitches, compmus_normalise, "manhattan")) |>
  compmus_self_similarity(pitches, "cosine") |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = 25 * xduration,
      y = ystart + yduration / 2,
      height = 25 * yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(
    title = "Clocks (Coldplay) - Self-similarity (chroma)",
    x = "", y = ""
  )

chroma_ssm_plot
ggsave("plots/clocks_ssm_chroma.png", chroma_ssm_plot, width = 6, height = 6, dpi = 300)


