# pipeline/problems/problems.R
# Top problems facing Spain (PESPANNA1/2/3, multi-response) across the 6 most
# recent CIS barometers. Generates 3 PNGs for the CIS Tracker site:
#   problems_last.png       - horizontal bars, latest barometer
#   problems_agg.png        - horizontal bars, 6-month average
#   problems_evolution.png  - line chart, top problems over the 6 months
#
# A mention counts if a problem appears in PESPANNA1, 2 OR 3, so percentages
# are "% of respondents who cite X among their three main problems" and do not
# sum to 100. Weighted by PESO.

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(fs)
})

# study_id -> publication date 
source("R/read_catalogue.R")
catalogue <- read_catalogue()

month_levels <- catalogue$month_label[order(catalogue$date)]

# ---------------------------------------------------------------------------
# 1. Read PESPANNA1/2/3 + weight from every barometer ZIP
# ---------------------------------------------------------------------------
zip_files <- dir_ls("data-raw/cis", glob = "*.zip")

read_problems <- function(zip_path) {
  td <- tempfile(); dir.create(td)
  unzip(zip_path, exdir = td)
  sav <- dir_ls(td, glob = "*.sav")[1]
  raw <- read_sav(sav)
  unlink(td, recursive = TRUE)
  
  tibble(
    study_id = str_extract(path_file(zip_path), "\\d+"),
    weight   = as.numeric(raw$PESO),
    p1       = as.integer(zap_labels(raw$PESPANNA1)),
    p2       = as.integer(zap_labels(raw$PESPANNA2)),
    p3       = as.integer(zap_labels(raw$PESPANNA3))
  )
}

raw_all <- map_dfr(zip_files, read_problems)

# Code -> label lookup from the most recent barometer as reference.
ref_zip <- zip_files[which.max(as.integer(str_extract(path_file(zip_files), "\\d+")))]
td <- tempfile(); dir.create(td); unzip(ref_zip, exdir = td)
ref_raw <- read_sav(dir_ls(td, glob = "*.sav")[1]); unlink(td, recursive = TRUE)
labels_vec <- attr(ref_raw$PESPANNA1, "labels")
code_lookup <- tibble(code = as.integer(labels_vec), label = names(labels_vec))

drop_codes <- c(996, 997, 998, 999)

# ---------------------------------------------------------------------------
# 2. Long format + weighted respondent base per barometer
# ---------------------------------------------------------------------------
long <- raw_all |>
  pivot_longer(c(p1, p2, p3), names_to = "slot", values_to = "code") |>
  filter(!is.na(code), !code %in% drop_codes) |>
  left_join(catalogue, by = "study_id") |>
  left_join(code_lookup, by = "code")

# Weighted number of respondents per barometer (the % denominator)
resp_weight <- raw_all |>
  group_by(study_id) |>
  summarise(total_w = sum(weight, na.rm = TRUE), .groups = "drop")

# ---------------------------------------------------------------------------
# 3. Stop-word-aware label shortener for the legend
#    Keeps the substantive words, drops Spanish articles/prepositions, and
#    truncates if still too long.
# ---------------------------------------------------------------------------
stop_words <- c("el","la","los","las","un","una","unos","unas","de","del",
                "y","o","a","en","con","para","por","e","u","al","lo",
                "su","sus","que","relacionados","relacionadas")

short_label <- function(x, max_words = 6) {
  words <- str_split(x, "\\s+")[[1]]
  kept  <- words[!str_to_lower(str_remove_all(words, "[[:punct:]]")) %in% stop_words]
  if (length(kept) == 0) kept <- words
  out <- paste(head(kept, max_words), collapse = " ")
  if (length(kept) > max_words) out <- paste0(out, "…")
  str_to_sentence(out)
}

# ---------------------------------------------------------------------------
# Site palette + theme
# ---------------------------------------------------------------------------
bg_col     <- "#0e0f11"
text_col   <- "#e8eaf0"
dim_col    <- "#9aa0ad"
accent_col <- "#c8f135"
muted_col  <- "#5a5f6b"

base_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = bg_col, color = NA),
    panel.background = element_rect(fill = bg_col, color = NA),
    panel.grid.major = element_line(color = "#1c1f25"),
    panel.grid.minor = element_blank(),
    text             = element_text(color = text_col),
    axis.text        = element_text(color = text_col),
    plot.title       = element_text(color = accent_col, face = "bold", size = 18),
    plot.subtitle    = element_text(color = dim_col, size = 11),
    plot.caption     = element_text(color = muted_col, size = 8),
    legend.text      = element_text(color = text_col),
    legend.title     = element_text(color = text_col),
    legend.background = element_rect(fill = bg_col, color = NA),
    legend.key       = element_rect(fill = bg_col, color = NA)
  )

wrap_label <- function(x, width = 40) str_wrap(x, width = width)

dir_create("docs/images/problems")

# ---------------------------------------------------------------------------
# 4. VIEW A — Latest month, top 12 (denominator = that month only)
# ---------------------------------------------------------------------------
latest_id  <- catalogue$study_id[which.max(catalogue$date)]
latest_lab <- catalogue$month_label[which.max(catalogue$date)]
latest_base <- resp_weight$total_w[resp_weight$study_id == latest_id]

bars_last <- long |>
  filter(study_id == latest_id) |>
  group_by(code, label) |>
  summarise(w = sum(weight, na.rm = TRUE), .groups = "drop") |>
  mutate(pct = 100 * w / latest_base) |>
  slice_max(pct, n = 12) |>
  mutate(label = wrap_label(label))

p_last <- ggplot(bars_last, aes(x = pct, y = reorder(label, pct))) +
  geom_col(fill = accent_col, width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            hjust = -0.15, color = text_col, size = 3.4) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.12))) +
  coord_cartesian(clip = "off") +
  labs(title = "Top problems facing Spain",
       subtitle = paste0("Latest barometer — ", latest_lab,
                         " · % of respondents citing each as a main problem"),
       x = NULL, y = NULL,
       caption = "Source: CIS · PESPANNA1/2/3 · beyond-the-map TFM, UC3M 2026") +
  base_theme + theme(panel.grid.major.y = element_blank())

ggsave("docs/images/problems/problems_last.png", p_last,
       width = 11, height = 7, dpi = 200, bg = bg_col)

# ---------------------------------------------------------------------------
# 5. VIEW B — 6-month AVERAGE, top 12
#    Mean of the monthly percentages (each month weighted within itself),
#    so the scale matches the individual months instead of summing them.
# ---------------------------------------------------------------------------
monthly_pct <- long |>
  group_by(study_id, code, label) |>
  summarise(w = sum(weight, na.rm = TRUE), .groups = "drop") |>
  left_join(resp_weight, by = "study_id") |>
  mutate(pct = 100 * w / total_w)

bars_agg <- monthly_pct |>
  group_by(code, label) |>
  summarise(pct = mean(pct), .groups = "drop") |>   # average across the 6 months
  slice_max(pct, n = 12) |>
  mutate(label = wrap_label(label))

p_agg <- ggplot(bars_agg, aes(x = pct, y = reorder(label, pct))) +
  geom_col(fill = accent_col, width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            hjust = -0.15, color = text_col, size = 3.4) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.12))) +
  coord_cartesian(clip = "off") +
  labs(title = "Top problems facing Spain",
       subtitle = paste0("6-month average (", month_levels[length(month_levels)], " – ",
                         month_levels[1],
                         ") · % of respondents citing each as a main problem"),
       
       x = NULL, y = NULL,
       caption = "Source: CIS · PESPANNA1/2/3 · beyond-the-map TFM, UC3M 2026") +
  base_theme + theme(panel.grid.major.y = element_blank())

ggsave("docs/images/problems/problems_agg.png", p_agg,
       width = 11, height = 7, dpi = 200, bg = bg_col)

# ---------------------------------------------------------------------------
# 6. VIEW C — Evolution of the top 7 problems (Nov 2025 -> Apr 2026)
# ---------------------------------------------------------------------------
top7_codes <- bars_agg |> slice_max(pct, n = 7) |> pull(code)

evo <- monthly_pct |>
  filter(code %in% top7_codes) |>
  left_join(catalogue |> select(study_id, month_label), by = "study_id") |>
  mutate(month_label = factor(month_label, levels = month_levels),
         label_short = map_chr(label, short_label))

# Order legend by % in the latest month (descending)
legend_order <- evo |>
  filter(study_id == latest_id) |>
  arrange(desc(pct)) |>
  pull(label_short)

evo <- evo |>
  mutate(label_short = factor(label_short, levels = legend_order))

palette7 <- c("#c8f135", "#f11123", "#15a6ef", "#ffa503",
              "#00c8b0", "#ef4a92", "#9d97e8")

p_evo <- ggplot(evo, aes(x = month_label, y = pct,
                         color = label_short, group = label_short)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.3) +
  scale_color_manual(values = palette7, name = NULL) +
  labs(title = "Evolution of main problems",
       subtitle = "Top 7 problems · % of respondents citing each as a main problem",
       x = NULL, y = "%",
       caption = "Source: CIS · PESPANNA1/2/3 · beyond-the-map TFM, UC3M 2026") +
  base_theme +
  theme(legend.position = "right",
        legend.key.width = unit(1.5, "lines"),
        legend.text = element_text(size = 10),
        plot.margin = margin(10, 10, 10, 10)) +
  guides(color = guide_legend(byrow = TRUE))

ggsave("docs/images/problems/problems_evolution.png", p_evo,
       width = 14, height = 7, dpi = 200, bg = bg_col)

cat("Saved 3 PNGs to docs/images/problems/\n")
