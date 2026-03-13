library(tidyverse)
library(compmus)

afraid_to_feel_ACT <- read_csv("songs_csv/afraid_to_feel_ACT.csv")
afraid_to_feel_DFT <- read_csv("songs_csv/afraid_to_feel_DFT.csv")
# Novelty function

# DFT plot
dft_plot <- afraid_to_feel_DFT |> 
  pivot_longer(-TIME, names_to = "tempo") |> 
  mutate(tempo = as.numeric(tempo)) |> 
  ggplot(aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(
    title = "Afraid to Feel - DFT",
    x = "Time (s)",
    y = "Tempo (BPM)"
  ) +
  theme_classic()

dft_plot

dir.create("plots_tempo", showWarnings = FALSE)
ggsave("plots_tempo/afraid_to_feel_DFT.png", dft_plot, width = 10, height = 4, dpi = 300)


# ACT plot
act_plot <- afraid_to_feel_ACT |> 
  pivot_longer(-TIME, names_to = "tempo") |> 
  mutate(tempo = as.numeric(tempo)) |> 
  ggplot(aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(
    title = "Afraid to Feel - Autocorrelation Tempogram",
    x = "Time (s)",
    y = "Tempo (BPM)"
  ) +
  theme_classic()

act_plot

dir.create("plots_tempo", showWarnings = FALSE)
ggsave("plots_tempo/afraid_to_feel_ACT.png", act_plot, width = 10, height = 4, dpi = 300)