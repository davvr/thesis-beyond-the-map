# R/render_map.R
# Centralized module for rendering provincial election maps.

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(mapSpain)
  library(colorspace)
  library(cowplot)
  library(shadowtext)
})

render_cis_map <- function(results_enriched, out_path) {
  
  HEX_PP_OSCURO <- "#014163"
  
  # 1. Palette definition and provincial winner assignment
  party_base <- tribble(
    ~party,     ~base_hex,
    "PP",       "#15a6ef", "PSOE",     "#f11123", "Vox",      "#74cd30",
    "Sumar",    "#ef4a92", "ERC",      "#ffa503", "Junts",    "#00c8b0",
    "EH Bildu", "#00ae8f", "PNV",      "#48b049",
    "BNG",      "#aed0ef", "CC",       "#ffd800", "UPN",      "#00589c"
  )
  
  party_colours <- tribble(
    ~party,   ~bin,      ~hex,
    "PP", "0-25",   "#b1e0f9", "PP", "25-30",  "#7fcdf5", "PP", "30-35",  "#4cb9f2",
    "PP", "35-40",  "#15a6ef", "PP", "40-45",  "#0687c7", "PP", "45-50",  "#036596",
    "PP", "50plus", "#014163",
    "PSOE", "0-25",   "#fbb0b5", "PSOE", "25-30",  "#f87d87", "PSOE", "30-35",  "#f44956",
    "PSOE", "35-40",  "#f11123", "PSOE", "40-45",  "#ca0413", "PSOE", "45-50",  "#98020b",
    "PSOE", "50plus", "#640106",
    "EH Bildu", "0-25",   "#c0f8ef", "EH Bildu", "25-30",  "#76f0db", "EH Bildu", "30-35",  "#22e8c7",
    "EH Bildu", "35-40",  "#00b999", "EH Bildu", "40-45",  "#009076", "EH Bildu", "45-50",  "#006653",
    "EH Bildu", "50plus", "#003a2d",
    "PNV", "0-25",   "#c3e8c3", "PNV", "25-30",  "#9dd99d", "PNV", "30-35",  "#77ca77",
    "PNV", "35-40",  "#4fbb4f", "PNV", "40-45",  "#399a39", "PNV", "45-50",  "#297429",
    "PNV", "50plus", "#184c18",
    "Vox", "0-25",   "#d3edae", "Vox", "25-30",  "#b9e286", "Vox", "30-35",  "#9ed75e",
    "Vox", "35-40",  "#74cd30", "Vox", "40-45",  "#5da821", "Vox", "45-50",  "#467d18",
    "Vox", "50plus", "#2f530f"
  )
  
  results_final_mapa <- results_enriched |>
    bind_cols(
      results_enriched |> select(starts_with("p_votos_")) |> mutate(id_row = row_number()) |>
        pivot_longer(cols = -id_row, names_to = "partido_temp", values_to = "pct_temp") |>
        group_by(id_row) |> slice_max(pct_temp, n = 1, with_ties = FALSE) |> ungroup() |>
        transmute(ganador_raw = gsub("p_votos_", "", partido_temp), ganador_pct = pct_temp)
    ) |>
    mutate(
      ganador = case_when(
        ganador_raw == "pp" ~ "PP", ganador_raw == "psoe" ~ "PSOE",
        ganador_raw == "vox" ~ "Vox",
        ganador_raw == "sumar" ~ "Sumar", ganador_raw == "jxcat__junts" ~ "Junts",
        ganador_raw == "eh_bildu" ~ "EH Bildu", ganador_raw == "eajpnv" ~ "PNV",
        ganador_raw == "erc" ~ "ERC", ganador_raw == "bng" ~ "BNG",
        ganador_raw == "cca" ~ "CC",
        ganador_raw == "upn" ~ "UPN", TRUE ~ toupper(ganador_raw)
      ),
      ganador_bin = cut(ganador_pct, breaks = c(0, 25, 30, 35, 40, 45, 50, 101),
                        labels = c("0-25", "25-30", "30-35", "35-40", "40-45",
                                   "45-50", "50plus"),
                        include.lowest = TRUE, right = FALSE)
    ) |>
    left_join(party_colours, by = c("ganador" = "party", "ganador_bin" = "bin")) |>
    left_join(party_base,    by = c("ganador" = "party")) |>
    mutate(final_hex = coalesce(hex, base_hex, "#d3d3d3")) |>
    select(-ganador_raw, -hex, -base_hex)
  
  # 2. Spatial topology and seat distribution
  mapa_provincias_raw <- esp_get_prov(year = "2021", epsg = 3857)
  
  # Suppress sf warnings on st_cast
  melilla_main <- suppressWarnings(mapa_provincias_raw |> filter(cpro == "52") |> st_cast("POLYGON") |>
                                     mutate(area = st_area(geometry)) |> slice_max(area, n = 1) |> select(-area))
  ceuta_main <- suppressWarnings(mapa_provincias_raw |> filter(cpro == "51") |> st_cast("POLYGON") |>
                                   mutate(area = st_area(geometry)) |> slice_max(area, n = 1) |> select(-area))
  
  mapa_provincias <- mapa_provincias_raw |> filter(!cpro %in% c("51", "52")) |>
    bind_rows(melilla_main, ceuta_main)
  
  data_mapa <- results_final_mapa |> filter(nombre_de_provincia != "España") |>
    mutate(cpro = str_pad(as.character(codigo_de_provincia), width = 2, pad = "0"))
  
  mapa_completo <- mapa_provincias |> left_join(data_mapa, by = "cpro")
  
  manual_offsets <- tibble(
    cpro   = c("08", "48", "20"),
    move_x = c(160000, -20000, 50000),
    move_y = c(-120000, 80000, 80000)
  )
  
  # Suppress sf warnings on st_centroid
  centroids <- suppressWarnings(mapa_provincias |> st_centroid()) |> select(cpro, ine.prov.name) |>
    mutate(real_x = st_coordinates(geometry)[,1], real_y = st_coordinates(geometry)[,2]) |>
    st_drop_geometry() |> left_join(manual_offsets, by = "cpro") |>
    mutate(
      cx = if_else(!is.na(move_x), real_x + move_x, real_x),
      cy = if_else(!is.na(move_y), real_y + move_y, real_y),
      is_displaced = !is.na(move_x)
    )
  
  seats_data <- results_enriched |> filter(nombre_de_provincia != "España") |>
    distinct(codigo_de_provincia, .keep_all = TRUE) |>
    select(codigo_de_provincia, matches("diputados_")) |>
    pivot_longer(cols = starts_with("diputados_"), names_to = "party_raw", values_to = "seats") |>
    filter(seats > 0) |>
    mutate(
      party_clean = str_remove(party_raw, "diputados_"),
      party = case_when(
        party_clean == "pp" ~ "PP", party_clean == "psoe" ~ "PSOE",
        party_clean == "vox" ~ "Vox", party_clean == "sumar" ~ "Sumar",
        party_clean == "erc" ~ "ERC", party_clean %in% c("jxcat__junts", "junts") ~ "Junts",
        party_clean %in% c("eh_bildu", "bildu") ~ "EH Bildu",
        party_clean %in% c("eajpnv", "pnv") ~ "PNV", party_clean == "bng" ~ "BNG",
        party_clean %in% c("cca", "cc") ~ "CC", party_clean == "upn" ~ "UPN",
        TRUE ~ "Otros"
      ),
      cpro = str_pad(as.character(codigo_de_provincia), width = 2, pad = "0")
    ) |>
    left_join(party_base |> select(party, base_hex), by = "party")
  
  province_stats <- seats_data |> group_by(cpro) |> summarise(total_seats = sum(seats)) |>
    mutate(
      cols_grid = case_when(
        cpro %in% c("08", "28") ~ 10, cpro == "46" ~ 6, cpro %in% c("41", "03", "29") ~ 5,
        cpro %in% c("48", "20", "11", "15") ~ 4,
        total_seats == 1 ~ 1, total_seats <= 4 ~ 2, total_seats <= 9 ~ 3, TRUE ~ 4
      )
    )
  
  seats_expanded <- seats_data |> left_join(province_stats, by = "cpro") |>
    arrange(cpro, desc(seats), party) |> uncount(seats) |>
    group_by(cpro) |> mutate(seat_id = row_number()) |> ungroup()
  
  SPACING <- 22000
  
  lines_data <- centroids |> filter(is_displaced) |> left_join(province_stats, by = "cpro") |>
    mutate(
      total_rows = ceiling(total_seats / cols_grid),
      approx_text_y = cy + ((total_rows - 1) * SPACING / 2) + 24000,
      end_y = case_when(cpro == "08" ~ approx_text_y + 5000, move_y > 0 ~ cy - 30000, TRUE ~ cy + 40000)
    )
  
  seats_coords <- seats_expanded |> left_join(centroids, by = "cpro") |>
    mutate(row_idx = floor((seat_id - 1) / cols_grid), col_idx_internal = (seat_id - 1) %% cols_grid) |>
    group_by(cpro, row_idx) |> mutate(dots_in_this_row = n()) |> ungroup() |>
    mutate(
      total_rows = ceiling(total_seats / cols_grid),
      x_pos = cx + (col_idx_internal * SPACING) - ((dots_in_this_row - 1) * SPACING / 2),
      y_pos = cy - (row_idx * SPACING) + ((total_rows - 1) * SPACING / 2)
    )
  
  label_data <- province_stats |> left_join(centroids, by = "cpro") |>
    left_join(mapa_completo |> st_drop_geometry() |> select(cpro, final_hex), by = "cpro") |>
    mutate(
      total_rows = ceiling(total_seats / cols_grid),
      y_label = cy + ((total_rows - 1) * SPACING / 2) + 24000,
      provincia_limpia = case_when(
        ine.prov.name == "Rioja, La" ~ "La Rioja", ine.prov.name == "Coruña, A" ~ "A Coruña",
        ine.prov.name == "Palmas, Las" ~ "Las Palmas", ine.prov.name == "Balears, Illes" ~ "Illes Balears",
        str_detect(ine.prov.name, "Alicante") ~ "Alicante", str_detect(ine.prov.name, "Valencia") ~ "Valencia",
        str_detect(ine.prov.name, "Castellón") ~ "Castellón", str_detect(ine.prov.name, "Araba") ~ "Álava",
        str_detect(ine.prov.name, "Bizkaia") ~ "Bizkaia", str_detect(ine.prov.name, "Gipuzkoa") ~ "Gipuzkoa",
        TRUE ~ ine.prov.name
      )
    )
  
  # 3. Zoom generation module (Madrid, Ceuta, Melilla)
  crear_zoom <- function(codigo_prov, nombre_prov, n_cols_grid, spacing_custom,
                         size_dots, radio_vista = NULL, draw_line_below = FALSE) {
    prov_sf    <- mapa_completo  |> filter(cpro == codigo_prov)
    prov_seats <- seats_expanded |> filter(cpro == codigo_prov)
    
    bbox <- st_bbox(prov_sf)
    alto_prov <- bbox$ymax - bbox$ymin
    centro <- suppressWarnings(st_centroid(prov_sf)) |> st_coordinates()
    cx_local <- centro[1]; cy_local <- centro[2]
    offset_y_val <- if(!is.null(radio_vista) && codigo_prov %in% c("51", "52")) -radio_vista * 0.15 else 0
    
    if (nrow(prov_seats) == 0) {
      coords_calc <- tibble(x_pos = numeric(0), y_pos = numeric(0), base_hex = character(0))
    } else {
      coords_calc <- prov_seats |> mutate(seat_id_local = row_number(),
                                          row_idx = floor((seat_id_local - 1) / n_cols_grid), col_idx = (seat_id_local - 1) %% n_cols_grid) |>
        group_by(row_idx) |> mutate(dots_in_row = n()) |> ungroup() |>
        mutate(total_rows = ceiling(n() / n_cols_grid),
               x_pos = cx_local + (col_idx * spacing_custom) - ((dots_in_row - 1) * spacing_custom / 2),
               y_pos = cy_local - (row_idx * spacing_custom) + ((total_rows - 1) * spacing_custom / 2) + offset_y_val)
    }
    
    if (nrow(coords_calc) > 0) {
      min_x_content <- min(bbox$xmin, min(coords_calc$x_pos))
      max_x_content <- max(bbox$xmax, max(coords_calc$x_pos))
    } else {
      min_x_content <- bbox$xmin; max_x_content <- bbox$xmax
    }
    
    if (!is.null(radio_vista)) {
      x_line_start <- cx_local - (radio_vista * 0.9); x_line_end <- cx_local + (radio_vista * 0.9)
      y_linea <- cy_local - (radio_vista * 0.8); y_texto <- cy_local + (radio_vista * 0.25)
    } else {
      margen_x <- spacing_custom * 0.5
      x_line_start <- min_x_content - margen_x; x_line_end <- max_x_content + margen_x
      y_linea <- bbox$ymin - (alto_prov * 0.15)
      y_texto <- if (nrow(coords_calc) > 0) max(coords_calc$y_pos) + (spacing_custom * 0.9) else bbox$ymax + 20000
    }
    p <- ggplot() + geom_sf(data = prov_sf, aes(fill = final_hex), color = "transparent", size = 0.2)
    if (nrow(coords_calc) > 0) {
      p <- p + geom_point(data = coords_calc, aes(x = x_pos, y = y_pos, fill = base_hex),
                          shape = 21, color = "black", size = size_dots, stroke = 0.3)
    }
    p <- p + geom_shadowtext(aes(x = cx_local, y = y_texto, label = nombre_prov), 
                             color = "#c8f135", bg.color = "black", bg.r = 0.1, 
                             fontface = "bold", size = 3.5, vjust = 0) +
      scale_fill_identity() + theme_void() + theme(plot.background = element_blank())
    
    if (is.null(radio_vista)) {
      p <- p + expand_limits(y = c(y_linea - 500, y_texto + 500), x = c(x_line_start, x_line_end)) + coord_sf(clip = "off")
    } else {
      p <- p + coord_sf(xlim = c(cx_local - radio_vista, cx_local + radio_vista),
                        ylim = c(cy_local - radio_vista, cy_local + radio_vista), expand = FALSE, clip = "off")
    }
    if (draw_line_below) {
      p <- p + geom_segment(aes(x = x_line_start, xend = x_line_end, y = y_linea, yend = y_linea), color = "#c8f135", linewidth = 0.5)
    }
    return(p)
  }
  
  plot_madrid  <- crear_zoom("28", "Madrid", 10, 15000, 4.8, NULL, TRUE)
  plot_ceuta   <- crear_zoom("51", "Ceuta", 1, 5000, 4.8, 8000, TRUE)
  plot_melilla <- crear_zoom("52", "Melilla", 1, 3000, 4.8, 7500, FALSE)
  
  # 4. Polar chart for national aggregates
  df_espana <- results_final_mapa |> filter(nombre_de_provincia == "España")
  partidos_interes <- c("pp", "psoe", "vox", "sumar", "erc", "jxcat__junts", "eh_bildu", "eajpnv", "bng", "cca", "upn")
  
  df_seats_nac <- df_espana |> select(starts_with("diputados_")) |>
    pivot_longer(everything(), names_to = "partido", values_to = "seats") |>
    mutate(partido = str_remove(partido, "^diputados_")) |>
    filter(partido %in% partidos_interes, seats > 0) |> arrange(desc(seats)) |>
    mutate(pct_seats = seats / sum(seats), pct_shift = pct_seats / 2, x = 1.0, partido = factor(partido, levels = partido))
  
  orden_partidos <- levels(df_seats_nac$partido)
  df_seats_nac <- df_seats_nac |> mutate(pct_seats = pct_seats + pct_shift)
  
  df_votes_nac <- df_espana |> select(starts_with("p_votos_")) |>
    pivot_longer(everything(), names_to = "partido", values_to = "pct_votes") |>
    mutate(partido = str_remove(partido, "^p_votos_"), pct_votes = pct_votes / 100) |>
    filter(partido %in% orden_partidos) |>
    mutate(pct_votes = pct_votes / sum(pct_votes), pct_shift = pct_votes / 2, x = 2.5, partido = factor(partido, levels = orden_partidos))
  
  df_votes_nac <- df_votes_nac |> mutate(pct_votes = pct_votes + pct_shift)
  
  eps <- 1e-6
  if (nrow(df_seats_nac) > 0) df_seats_nac$pct_seats[1] <- df_seats_nac$pct_seats[1] + eps
  if (nrow(df_votes_nac) > 0) df_votes_nac$pct_votes[1] <- df_votes_nac$pct_votes[1] + eps
  
  colores_nacional <- c(pp = "#15a6ef", psoe = "#f11123", vox = "#74cd30", sumar = "#ef4a92",
                        erc = "#ffa503", jxcat__junts = "#00c8b0", eh_bildu = "#00ae8f",
                        eajpnv = "#48b049", bng = "#aed0ef", cca = "#ffd800", upn = "#00589c")
  
  plot_resumen_nacional <- ggplot() +
    geom_col(data = df_seats_nac, aes(x = x, y = pct_seats, fill = partido), width = 2, linewidth = 0, color = NA) +
    geom_col(data = df_votes_nac, aes(x = x, y = pct_votes, fill = partido), width = 1, linewidth = 0, color = NA) +
    geom_vline(xintercept = 2.0, color = "white", linewidth = 1.5) +
    scale_fill_manual(values = colores_nacional) + scale_x_continuous(limits = c(0, 3)) +
    coord_polar(theta = "y", start = pi / 2, direction = -1) +
    geom_shadowtext(aes(x = 1.0, y = 0.02), label = "Seats", color = "#c8f135", bg.color = "black", bg.r = 0.1, size = 3, fontface = "bold") +
    geom_shadowtext(aes(x = 2.9, y = 0.02), label = "Votes", color = "#c8f135", bg.color = "black", bg.r = 0.1, size = 3, fontface = "bold") +
    theme_void() + theme(panel.background = element_rect(fill = "transparent", color = NA),
                         plot.background  = element_rect(fill = "transparent", color = NA),
                         legend.position  = "none")
  
  # 5. Main cartographic composition
  OFFSET_CANARIAS <- -200000
  codigos_zoom_data <- c("28", "51", "52")
  seats_coords_main <- seats_coords |> filter(!cpro %in% codigos_zoom_data)
  label_data_main   <- label_data   |> filter(!cpro %in% codigos_zoom_data)
  lines_data_main   <- lines_data   |> filter(!cpro %in% codigos_zoom_data)
  
  mapa_shift <- mapa_completo
  idx_can <- which(mapa_shift$cpro %in% c("35", "38"))
  st_geometry(mapa_shift)[idx_can] <- st_geometry(mapa_shift)[idx_can] + c(0, OFFSET_CANARIAS)
  st_crs(mapa_shift) <- st_crs(mapa_completo)
  
  seats_shift <- seats_coords_main |> mutate(y_pos = if_else(cpro %in% c("35", "38"), y_pos + OFFSET_CANARIAS, y_pos))
  labels_shift <- label_data_main |> mutate(y_label = if_else(cpro %in% c("35", "38"), y_label + OFFSET_CANARIAS, y_label))
  
  sf_canarias_orig <- mapa_completo |> filter(cpro %in% c("35", "38"))
  bbox_can_orig <- st_bbox(sf_canarias_orig)
  rect_canarias <- tibble(xmin = bbox_can_orig$xmin - 40000, xmax = bbox_can_orig$xmax + 20000,
                          ymin = bbox_can_orig$ymin - 20000 + OFFSET_CANARIAS, ymax = bbox_can_orig$ymax + 80000 + OFFSET_CANARIAS)
  
  bbox_38 <- st_bbox(mapa_completo |> filter(cpro == "38")); bbox_35 <- st_bbox(mapa_completo |> filter(cpro == "35"))
  mid_x_can <- (bbox_38$xmax + bbox_35$xmin) / 2
  cy_can <- (bbox_can_orig$ymin + bbox_can_orig$ymax) / 2 + OFFSET_CANARIAS
  
  linea_diagonal <- tibble(x = mid_x_can - 18000 + 9000, xend = mid_x_can + 18000 + 9000,
                           y = cy_can - 49000 - 23000, yend = cy_can + 49000 - 23000)
  
  centroids_zoom <- suppressWarnings(mapa_provincias |> filter(cpro == "28") |> st_centroid()) |>
    mutate(cx = st_coordinates(geometry)[,1], cy = st_coordinates(geometry)[,2])

  mapa_base <- ggplot() +
    geom_sf(data = mapa_shift |> filter(!cpro %in% c("07", "35", "38")), aes(fill = final_hex), color = "white", size = 0.2) +
    geom_sf(data = mapa_shift |> filter(cpro %in% c("07", "35", "38")), aes(fill = final_hex), color = "transparent", size = 0.2) +
    geom_segment(data = rect_canarias, aes(x = xmin, xend = xmax, y = ymax, yend = ymax), color = "black", linewidth = 0.4) +
    geom_segment(data = rect_canarias, aes(x = xmax, xend = xmax, y = ymin, yend = ymax), color = "black", linewidth = 0.4) +
    geom_segment(data = linea_diagonal, aes(x = x, y = y, xend = xend, yend = yend), color = "black", linewidth = 0.4) +
    geom_segment(data = lines_data_main, aes(x = real_x, y = real_y, xend = cx, yend = end_y), color = "black", linewidth = 0.5) +
    geom_text(data = centroids_zoom, aes(x = cx, y = cy), label = "*", color = "white", size = 15, fontface = "bold") +
    geom_point(data = seats_shift, aes(x = x_pos, y = y_pos, fill = base_hex), shape = 21, color = "black", size = 4.8, stroke = 0.3) +
    geom_shadowtext(data = labels_shift,
                    aes(x = cx, y = y_label, label = provincia_limpia,
                        color = if_else(is_displaced, "#c8f135", if_else(final_hex == HEX_PP_OSCURO, "white", "#c8f135", missing = "#c8f135"))),
                    bg.color = "black", bg.r = 0.1, size = 3.4, fontface = "bold") +
    scale_fill_identity() + scale_color_identity() + theme_void() +
    theme(panel.background = element_rect(fill = "transparent", color = NA), plot.background  = element_rect(fill = "transparent", color = NA))
  
  
  
  # 6. Legend tables and national statistics
  national_stats <- results_enriched |> filter(nombre_de_provincia == "España") |>
    mutate(total_validos = votos_validos) |> select(total_validos, matches("^(votos|diputados)_")) |>
    pivot_longer(cols = matches("^(votos|diputados)_"), names_to = c(".value", "party_raw"), names_pattern = "(votos|diputados)_(.*)") |>
    mutate(
      pct_nacional = votos / total_validos * 100,
      party = case_when(
        party_raw == "pp" ~ "PP", party_raw == "psoe" ~ "PSOE", party_raw == "vox" ~ "Vox", party_raw == "sumar" ~ "Sumar",
        party_raw == "erc" ~ "ERC", party_raw %in% c("jxcat__junts", "junts") ~ "Junts",
        party_raw %in% c("eh_bildu", "bildu") ~ "EH Bildu", party_raw %in% c("eajpnv", "pnv") ~ "PNV",
        party_raw == "bng" ~ "BNG", party_raw %in% c("cca", "cc") ~ "CC", party_raw == "upn" ~ "UPN", TRUE ~ NA_character_
      )
    ) |> filter(!is.na(party))
  
  layout_manual <- tribble(
    ~party,     ~col, ~row,
    "PP",       1,    1,
    "PSOE",     1,    2,
    "Vox",      1,    3,
    "Sumar",    1,    4,
    "ERC",      1,    5,
    "Junts",    2,    1,
    "EH Bildu", 2,    2,
    "PNV",      2,    3,
    "BNG",      2,    4,
    "CC",       2,    5,
    "UPN",      2,    6
  )
  
  projected_winners <- results_final_mapa |> filter(nombre_de_provincia != "España") |> distinct(ganador) |> pull(ganador)
  
  legend_data <- layout_manual |> left_join(national_stats, by = "party") |> left_join(party_base, by = "party") |>
    mutate(label_pct = sprintf("%.2f", pct_nacional), label_seats = as.character(diputados),
           base_hex = replace_na(base_hex, "#999999"), is_winner = party %in% projected_winners)
  
  crear_bloque_leyenda <- function(datos, mostrar_headers = FALSE) {
    prep <- datos |> mutate(y_base = -(row - 1) * 1.5, x_base = 0)
    tiles <- prep |> filter(is_winner) |> pmap_dfr(function(base_hex, x_base, y_base, ...) {
      colors <- colorRampPalette(c(lighten(base_hex, 0.6), base_hex, darken(base_hex, 0.2)))(7)
      tibble(x_base = x_base, tile_id = 1:7, fill = colors, ymin = y_base + 0.8, ymax = y_base + 1.1)
    })
    min_y <- min(prep$y_base)
    
    p <- ggplot()
    
    # FIX: Only draw rects if there are winners in this specific legend block
    if (nrow(tiles) > 0) {
      p <- p + geom_rect(data = tiles, aes(xmin = x_base + tile_id - 0.48, xmax = x_base + tile_id + 0.48, ymin = ymin, ymax = ymax, fill = fill), color = NA)
    }
    
    p <- p +
      geom_rect(data = prep, aes(xmin = 0.5, xmax = 7.5, ymin = y_base - 0.1, ymax = y_base + 0.75 + 0.1, fill = base_hex), color = NA) +
      geom_text(data = prep, aes(x = 0.7, y = y_base + 0.375, label = party), color = "white", fontface = "bold", hjust = 0, size = 4.5) +
      geom_text(data = prep, aes(x = 8.5, y = y_base + 0.375, label = label_pct, color = base_hex), fontface = "bold", hjust = 0.5, size = 4.5) +
      geom_text(data = prep, aes(x = 10.5, y = y_base + 0.375, label = label_seats, color = base_hex), fontface = "bold", hjust = 0.5, size = 4.5) +
      scale_fill_identity() + scale_color_identity() + theme_void() +
      theme(panel.background = element_rect(fill = "transparent", color = NA), plot.background = element_rect(fill = "transparent", color = NA)) +
      coord_fixed(ratio = 1) + xlim(0, 11.5) + ylim(min_y - 0.5, 2)
    
    if (mostrar_headers) {
      headers <- tibble(label = c("-25%", "25%", "30%", "35%", "40%", "45%", "50%+", "Vote %", "Seats"),
                        x = c(1, 2, 3, 4, 5, 6, 7, 8.5, 10.5), y = 1.6)
      p <- p + geom_text(data = headers, aes(x = x, y = y, label = label), color = "#c8f135", fontface = "bold", size = 3, hjust = 0.5)
    }
    return(p)
  }
  
  plot_leyenda_izq  <- crear_bloque_leyenda(legend_data |> filter(col == 1) |> arrange(row), TRUE)
  plot_leyenda_dcha <- crear_bloque_leyenda(legend_data |> filter(col == 2) |> arrange(row), FALSE)
  
  # 7. Final rendering
  mapa_final <- ggdraw() +
    draw_plot(mapa_base,             x = 0,    y = 0,     width = 1,    height = 1) +
    draw_plot(plot_madrid,           x = 0.02, y = 0.60,  width = 0.24, height = 0.23) +
    draw_plot(plot_ceuta,            x = 0.02, y = 0.42,  width = 0.22, height = 0.19) +
    draw_plot(plot_melilla,          x = 0.02, y = 0.24,  width = 0.22, height = 0.19) +
    draw_plot(plot_resumen_nacional, x = 0.06, y = 0.83,  width = 0.18, height = 0.15) +
    draw_plot(plot_leyenda_izq,      x = 0.5,  y = 0.085, width = 0.25, height = 0.30) +
    draw_plot(plot_leyenda_dcha,     x = 0.73, y = 0.052, width = 0.25, height = 0.40)
  
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(out_path, mapa_final, width = 16, height = 13, dpi = 200, bg = "#0e0f11")
  cat("Map saved to:", out_path, "\n")
}