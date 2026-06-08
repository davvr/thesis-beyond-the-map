# pipeline/ideology/ideology_evolution.R
# Monthly evolution of ideological self-placement (ESCIDEOL, 1-10 scale)
# across the 6 most recent CIS barometers.
#
# Output: a combined plot with
#   (a) heatmap: month (x) vs ideology bin 1-10 (y), fill = weighted %
#   (b) overlaid line: weighted mean ideology per month
#
# Saved to docs/images/ideology/ideology_evolution.png

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(fs)
})

source("R/scraping/list_barometers.R")

# ---------------------------------------------------------------------------
# 1. Read ESCIDEOL + weight from every barometer ZIP
# ---------------------------------------------------------------------------
zip_files <- dir_ls("data-raw/cis", glob = "*.zip")

read_ideology <- function(zip_path) {
  td <- tempfile(); dir.create(td)
  unzip(zip_path, exdir = td)
  sav <- dir_ls(td, glob = "*.sav")[1]
  raw <- read_sav(sav)
  unlink(td, recursive = TRUE)
  
  study_id <- str_extract(path_file(zip_path), "\\d+")
  
  tibble(
    study_id = study_id,
    ideol    = as.integer(zap_labels(raw$ESCIDEOL)),
    weight   = as.numeric(raw$PESO)
  )
}

raw_all <- map_dfr(zip_files, read_ideology)

# ---------------------------------------------------------------------------
# 2. Clean + attach month labels from the catalogue
# ---------------------------------------------------------------------------
catalogue <- list_barometers(n = 20) |>
  mutate(month_label = format(date, "%b %Y")) |>
  select(study_id, date, month_label)

clean <- raw_all |>
  filter(!ideol %in% c(98, 99), !is.na(ideol)) |>
  left_join(catalogue, by = "study_id") |>
  arrange(date) |>
  mutate(month_label = factor(month_label, levels = unique(month_label)))

# ---------------------------------------------------------------------------
# 3. Weighted distribution per month (for heatmap)
# ---------------------------------------------------------------------------
dist <- clean |>
  group_by(month_label, ideol) |>
  summarise(w = sum(weight), .groups = "drop") |>
  group_by(month_label) |>
  mutate(pct = 100 * w / sum(w)) |>
  ungroup()

# ---------------------------------------------------------------------------
# 4. Weighted mean per month (for line)
# ---------------------------------------------------------------------------
means <- clean |>
  group_by(month_label) |>
  summarise(mean_ideol = weighted.mean(ideol, weight), .groups = "drop")

# ---------------------------------------------------------------------------
# 5. Plot: heatmap + overlaid mean line
# ---------------------------------------------------------------------------
# Dark theme matching the CIS Tracker site palette
bg_col     <- "#0e0f11"
text_col   <- "#e8eaf0"
accent_col <- "#c8f135"
grid_col   <- "#2a2d34"

p <- ggplot(dist, aes(x = month_label, y = ideol)) +
  geom_tile(aes(fill = pct), color = bg_col, linewidth = 0.5) +
  scale_fill_gradient(
    low = "#16181c", high = accent_col,
    name = "% (weighted)"
  ) +
  # mean line + points (rescaled directly on the 1-10 y-axis)
  geom_line(data = means, aes(x = month_label, y = mean_ideol, group = 1),
            color = "#f11123", linewidth = 1.1) +
  geom_point(data = means, aes(x = month_label, y = mean_ideol),
             color = "#f11123", size = 2.5) +
  geom_text(data = means, aes(x = month_label, y = mean_ideol,
                              label = sprintf("%.2f", mean_ideol)),
            color = "#f11123", vjust = -1.1, size = 3.2, fontface = "bold") +
  scale_y_continuous(breaks = 1:10,
                     labels = c("1\nLeft", 2:9, "10\nRight"),
                     expand = expansion(mult = 0.02)) +
  labs(
    title    = "Ideological self-placement",
    subtitle = "Weighted distribution per barometer (tiles) and monthly mean (red line)",
    x = NULL, y = NULL,
    caption  = "Source: CIS · ESCIDEOL (1-10) · beyond-the-map TFM, UC3M 2026"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = bg_col, color = NA),
    panel.background = element_rect(fill = bg_col, color = NA),
    panel.grid       = element_blank(),
    text             = element_text(color = text_col),
    axis.text        = element_text(color = text_col),
    plot.title       = element_text(color = accent_col, face = "bold", size = 18),
    plot.subtitle    = element_text(color = "#9aa0ad", size = 11),
    plot.caption     = element_text(color = "#5a5f6b", size = 8),
    legend.text      = element_text(color = text_col),
    legend.title     = element_text(color = text_col),
    legend.background = element_rect(fill = bg_col, color = NA),
    legend.key       = element_rect(fill = bg_col, color = NA)
  )

dir_create("docs/images/ideology")
out_path <- "docs/images/ideology/ideology_evolution.png"
ggsave(out_path, p, width = 11, height = 7, dpi = 200, bg = bg_col)
cat("Saved to:", out_path, "\n")
