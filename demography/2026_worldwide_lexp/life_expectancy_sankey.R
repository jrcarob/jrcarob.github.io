library(tidyverse)
library(ggplot2)
library(ggflags)
library(showtext)

# ============================================================================
# 0. LOAD FONT
# ============================================================================
font_add_google("Roboto Condensed", "roboto_condensed")
showtext_auto()

# ============================================================================
# 1. DATA
# ============================================================================

region_colors <- c(
  "Europe"    = "#2E5AAC",
  "Asia"      = "#C44536", 
  "Americas"  = "#D4A843",
  "Oceania"   = "#3AA688",
  "Mid. East" = "#E07A3E"
)

country_iso2 <- c(
  "Monaco" = "mc", "San Marino" = "sm", "Japan" = "jp", 
  "South Korea" = "kr", "Andorra" = "ad", "Switzerland" = "ch",
  "Australia" = "au", "Italy" = "it", "Singapore" = "sg",
  "Spain" = "es", "Liechtenstein" = "li", "Malta" = "mt",
  "Norway" = "no", "Sweden" = "se", "France" = "fr",
  "UAE" = "ae", "Iceland" = "is", "Canada" = "ca",
  "Ireland" = "ie", "Israel" = "il", "Portugal" = "pt",
  "Qatar" = "qa", "Luxembourg" = "lu", "Netherlands" = "nl",
  "Belgium" = "be", "New Zealand" = "nz", "Austria" = "at",
  "Finland" = "fi", "Greece" = "gr", "Denmark" = "dk",
  "U.S." = "us", "Maldives" = "mv", "Cyprus" = "cy",
  "Chile" = "cl", "Costa Rica" = "cr"
)

data_2026 <- tribble(
  ~rank_2026, ~country_2026, ~value_2026, ~region_2026,
  1,  "Monaco",        88.7, "Europe",
  2,  "San Marino",    86.0, "Europe",
  3,  "Japan",         85.1, "Asia",
  4,  "South Korea",   84.6, "Asia",
  5,  "Andorra",       84.5, "Europe",
  6,  "Switzerland",   84.4, "Europe",
  7,  "Australia",     84.3, "Oceania",
  8,  "Italy",         84.2, "Europe",
  9,  "Singapore",     84.1, "Asia",
  10, "Spain",         84.1, "Europe",
  11, "Liechtenstein", 84.1, "Europe",
  12, "Malta",         83.8, "Europe",
  13, "Norway",        83.8, "Europe",
  14, "Sweden",        83.7, "Europe",
  15, "France",        83.7, "Europe",
  16, "UAE",           83.4, "Mid. East",
  17, "Iceland",       83.3, "Europe",
  18, "Canada",        83.1, "Americas",
  19, "Ireland",       82.9, "Europe",
  20, "Israel",        82.9, "Mid. East",
  21, "Portugal",      82.9, "Europe",
  22, "Qatar",         82.8, "Mid. East",
  23, "Luxembourg",    82.6, "Europe",
  24, "Netherlands",   82.6, "Europe",
  25, "Belgium",       82.6, "Europe",
  26, "New Zealand",   82.5, "Oceania",
  27, "Austria",       82.5, "Europe",
  28, "Finland",       82.4, "Europe",
  29, "Greece",        82.4, "Europe",
  30, "Denmark",       82.4, "Europe",
  45, "U.S.",          79.3, "Americas",
  
)

data_2100 <- tribble(
  ~rank_2100, ~country_2100, ~value_2100, ~region_2100,
  1,  "Monaco",              94.7, "Europe",
  2,  "Japan",               94.4, "Asia",
  3,  "San Marino",          94.0, "Europe",
  4,  "South Korea",         93.1, "Asia",
  5,  "Andorra",             93.0, "Europe",
  6,  "Liechtenstein",       92.9, "Europe",
  7,  "Spain",               92.9, "Europe",
  8,  "Switzerland",         92.8, "Europe",
  9,  "Italy",               92.8, "Europe",
  10, "Singapore",           92.7, "Asia",
  11, "Australia",           92.6, "Oceania",
  12, "Malta",               92.3, "Europe",
  13, "Sweden",              92.2, "Europe",
  14, "France",              92.1, "Europe",
  15, "Norway",              92.1, "Europe",
  16, "Iceland",             92.0, "Europe",
  17, "Portugal",            91.9, "Europe",
  18, "Ireland",             91.8, "Europe",
  19, "Canada",              91.7, "Americas",
  20, "Maldives",            91.6, "Asia",
  21, "UAE",                 91.6, "Mid. East",
  22, "Israel",              91.5, "Mid. East",
  23, "Netherlands",         91.5, "Europe",
  24, "Austria",             91.5, "Europe",
  25, "Belgium",             91.4, "Europe",
  26, "Greece",              91.4, "Europe",
  27, "Cyprus",              91.4, "Europe",
  28, "Finland",             91.3, "Europe",
  29, "Chile",               91.3, "Americas",
  30, "Costa Rica",          91.2, "Americas",
  48, "U.S.",                89.2, "Americas"
)

# ============================================================================
# 2. PREPARE ALL COUNTRIES WITH Y-POSITIONS
# ============================================================================

# All 2026 countries with y positions (rank 1 at top → y=30)
data_2026_y <- data_2026 |> 
  mutate(y_2026 = 31 - rank_2026, iso2 = country_iso2[country_2026])

# All 2100 countries with y positions
data_2100_y <- data_2100 |> 
  mutate(y_2100 = 31 - rank_2100, iso2 = country_iso2[country_2100])

# Classify countries
common_countries <- intersect(data_2026$country_2026, data_2100$country_2100)
dropped_countries <- setdiff(data_2026$country_2026, data_2100$country_2100)
new_countries <- setdiff(data_2100$country_2100, data_2026$country_2026)

# Bottom position for dropped/new ribbons
bottom_y <- 0.5

# ============================================================================
# 3. CREATE SPLINE CURVES FOR ALL RIBBONS
# ============================================================================

create_spline <- function(y_start, y_end, n = 200) {
  t <- seq(0, 1, length.out = n)
  x <- t
  y <- y_start + (y_end - y_start) * (3*t^2 - 2*t^3)
  tibble(x = x, y = y)
}

ribbon_width <- 0.9

# --- 3a. COMMON: normal ribbons between positions ---
common_data <- data_2026_y |> 
  filter(country_2026 %in% common_countries) |> 
  inner_join(
    data_2100_y |> filter(country_2100 %in% common_countries),
    by = c("country_2026" = "country_2100")
  ) |> 
  rename(country = country_2026, region = region_2026) |> 
  select(country, y_2026, y_2100, value_2026, value_2100, region, iso2 = iso2.x)

ribbon_common <- map_dfr(1:nrow(common_data), function(i) {
  row <- common_data[i, ]
  spline <- create_spline(row$y_2026, row$y_2100, n = 200)
  spline |> mutate(
    country = row$country, region = row$region, iso2 = row$iso2,
    ymin = y - ribbon_width/2, ymax = y + ribbon_width/2
  )
})

# --- 3b. DROPPED: ribbons from position DOWN to bottom ---
# These countries have 2026 data but no 2100 data
# They flow from their 2026 position to bottom_y
dropped_data <- data_2026_y |> 
   filter(country_2026 %in% dropped_countries) |> 
   mutate(
     y_2100 = bottom_y,
     value_2100 = NA_real_  # No 2100 value
   ) |> 
   rename(country = country_2026, region = region_2026) |> 
   select(country, y_2026, y_2100, value_2026, value_2100, region, iso2)
 
 ribbon_dropped <- map_dfr(1:nrow(dropped_data), function(i) {
   row <- dropped_data[i, ]
   spline <- create_spline(row$y_2026, row$y_2100, n = 200)
   spline |> mutate(
     country = row$country, region = row$region, iso2 = row$iso2,
     ymin = y - ribbon_width/2, ymax = y + ribbon_width/2
   )
 })

# --- DROPPED: taper from full to zero ---
ribbon_dropped <- map_dfr(1:nrow(dropped_data), function(i) {
  row <- dropped_data[i, ]
  n_points <- 200
  t <- seq(0, 1, length.out = n_points)
  x <- t
  y <- row$y_2026 + (row$y_2100 - row$y_2026) * (3*t^2 - 2*t^3)
  
  # TAPER: width decreases from full to 0
  width_factor <- (1 - t)^1.5
  half_width <- (ribbon_width / 2) * width_factor
  
  tibble(
    x = x, y = y,
    country = row$country,
    region = row$region,
    iso2 = row$iso2,
    ymin = y - half_width,
    ymax = y + half_width
  )
})

# --- 3c. NEW: ribbons from bottom UP to position ---
# These countries have 2100 data but no 2026 data
# They flow from bottom_y to their 2100 position
new_data <- data_2100_y |> 
   filter(country_2100 %in% new_countries) |> 
   mutate(
     y_2026 = bottom_y,
     value_2026 = NA_real_  # No 2026 value
   ) |> 
   rename(country = country_2100, region = region_2100) |> 
   select(country, y_2026, y_2100, value_2026, value_2100, region, iso2)
 
 ribbon_new <- map_dfr(1:nrow(new_data), function(i) {
   row <- new_data[i, ]
   spline <- create_spline(row$y_2026, row$y_2100, n = 200)
   spline |> mutate(
     country = row$country, region = row$region, iso2 = row$iso2,
     ymin = y - ribbon_width/2, ymax = y + ribbon_width/2
   )
 })

# --- NEW: taper from zero to full ---
ribbon_new <- map_dfr(1:nrow(new_data), function(i) {
  row <- new_data[i, ]
  n_points <- 200
  t <- seq(0, 1, length.out = n_points)
  x <- t
  y <- row$y_2026 + (row$y_2100 - row$y_2026) * (3*t^2 - 2*t^3)
  
  # TAPER: width increases from 0 to full
  width_factor <- t^1.5
  half_width <- (ribbon_width / 2) * width_factor
  
  tibble(
    x = x, y = y,
    country = row$country,
    region = row$region,
    iso2 = row$iso2,
    ymin = y - half_width,
    ymax = y + half_width
  )
})

# Combine all ribbons
ribbon_all <- bind_rows(ribbon_common, ribbon_dropped, ribbon_new)

# ============================================================================
# 4. VALUES AT EXTREME EDGES
# ============================================================================
# Helper: sample the spline at any x-position
calc_spline_y <- function(y_start, y_end, t) {
  y_start + (y_end - y_start) * (3*t^2 - 2*t^3)
}

# Helper: always show one decimal place
fmt_value <- function(v) {
  ifelse(is.na(v), "", sprintf("%.1f", v))
}

# Common: values at both extremes
left_values_common <- common_data |> 
  mutate(x_pos = 0.07, y_pos = calc_spline_y(y_2026, y_2100, 0.07), label_text = fmt_value(value_2026))

right_values_common <- common_data |> 
  mutate(x_pos = 0.93, y_pos = y_2100, label_text = fmt_value(value_2100))

# Dropped: only 2026 value at left
left_values_dropped <- dropped_data |> 
  mutate(x_pos = 0.07, y_pos = calc_spline_y(y_2026, y_2100, 0.07), label_text = fmt_value(value_2026))

# New: only 2100 value at right
right_values_new <- new_data |> 
  mutate(x_pos = 0.93, y_pos = calc_spline_y(y_2026, y_2100, 0.93), label_text = fmt_value(value_2100))

# Combine all values
left_values_all <- bind_rows(left_values_common, left_values_dropped)
right_values_all <- bind_rows(right_values_common, right_values_new)

# ============================================================================
# 5. NODE DATA
# ============================================================================

# 2026 nodes: ALL countries that appear in 2026 (common + dropped)
nodes_2026_all <- data_2026_y |> 
  mutate(x = 0, node_y = y_2026, country = country_2026, region = region_2026) |> 
  select(country, value_2026, node_y, region, iso2, x)

# 2100 nodes: ALL countries that appear in 2100 (common + new)
nodes_2100_all <- data_2100_y |> 
  mutate(x = 1, node_y = y_2100, country = country_2100, region = region_2100) |> 
  select(country, value_2100, node_y, region, iso2, x)

# ============================================================================
# 6. BUILD PLOT
# ============================================================================

bg_color <- "#F5F0E8"

p <- ggplot() +
  # --- ALL ribbons ---
  geom_ribbon(
    data = ribbon_all,
    aes(x = x, ymin = ymin, ymax = ymax, group = country, fill = region),
    alpha = 0.95, color = NA
  ) +
  scale_fill_manual(values = region_colors, name = NULL) +

  # --- VALUES AT LEFT EXTREME ---
  geom_text(
    data = left_values_all,
    aes(x = x_pos, y = y_pos, label = label_text),
    size = 18, fontface = "bold", color = "white", 
    family = "roboto_condensed", vjust = 0.3, hjust = 0.3
  ) +

  # --- VALUES AT RIGHT EXTREME ---
  geom_text(
    data = right_values_all,
    aes(x = x_pos, y = y_pos, label = label_text),
    size = 18, fontface = "bold", color = "white", 
    family = "roboto_condensed", vjust = 0.3, hjust = 0.6
  ) +

  # --- 2026 NODES (all 30 countries) ---
  geom_point(
    data = nodes_2026_all,
    aes(x = x - 0.015, y = node_y),
    size = 15, color = "black", fill = "white", shape = 21, stroke = 1.0
  ) +
  ggflags::geom_flag(
    data = nodes_2026_all,
    aes(x = x - 0.015, y = node_y, country = iso2),
    size = 13
  ) +

  # --- 2100 NODES (all 30 countries) ---
  geom_point(
    data = nodes_2100_all,
    aes(x = x + 0.015, y = node_y),
    size = 15, color = "black", fill = "white", shape = 21, stroke = 1.0
  ) +
  ggflags::geom_flag(
    data = nodes_2100_all,
    aes(x = x + 0.015, y = node_y, country = iso2),
    size = 13
  ) +

  # --- 2026 LABELS (country name only) ---
  geom_text(
    data = nodes_2026_all,
    aes(x = x - 0.075, y = node_y, label = country),
    hjust = 1, size = 18, fontface = "bold",
    family = "roboto_condensed"
  ) +

  # --- 2100 LABELS (country name only) ---
  geom_text(
    data = nodes_2100_all,
    aes(x = x + 0.075, y = node_y, label = country),
    hjust = 0, size = 18, fontface = "bold",
    family = "roboto_condensed"
  ) +

  # --- TITLE (larger, like original) ---
  annotate("text", x = 0.5, y = 34.5, 
           label = "THE WORLD'S HIGHEST", 
           size = 28, fontface = "bold", 
           color = "grey40", family = "roboto_condensed") +
  annotate("text", x = 0.5, y = 33.0, 
           label = "Life Expectancy", 
           size = 86, fontface = "bold", 
           family = "serif") +
  
  # --- YEAR LABELS ---
  annotate("text", x = -0.1, y = 31, 
           label = "2026", 
           size = 36, fontface = "bold", 
           family = "roboto_condensed") +
  annotate("text", x = 1.1, y = 31, 
           label = "2100p", 
           size = 36, fontface = "bold", 
           family = "roboto_condensed") +
  
  # --- REGION COLOR KEY (below title, like original) ---
  # Europe
  annotate("rect", xmin = 0.0, xmax = 0.18, ymin = 30.5, ymax = 31.0, 
           fill = region_colors["Europe"]) +
  annotate("text", x = 0.09, y = 30.75, label = "EUROPE", 
           size = 12, color = "white", 
           fontface = "bold", family = "roboto_condensed") +
  # Asia
  annotate("rect", xmin = 0.20, xmax = 0.38, ymin = 30.5, ymax = 31.0, 
           fill = region_colors["Asia"]) +
  annotate("text", x = 0.29, y = 30.75, label = "ASIA", 
           size = 12, color = "white", 
           fontface = "bold", family = "roboto_condensed") +
  # Americas
  annotate("rect", xmin = 0.40, xmax = 0.58, ymin = 30.5, ymax = 31.0, 
           fill = region_colors["Americas"]) +
  annotate("text", x = 0.49, y = 30.75, label = "AMERICAS", 
           size = 12, color = "white", 
           fontface = "bold", family = "roboto_condensed") +
  # Oceania
  annotate("rect", xmin = 0.60, xmax = 0.78, ymin = 30.5, ymax = 31.0, 
           fill = region_colors["Oceania"]) +
  annotate("text", x = 0.69, y = 30.75, label = "OCEANIA", 
           size = 12, color = "white", 
           fontface = "bold", family = "roboto_condensed") +
  # Mid. East
  annotate("rect", xmin = 0.80, xmax = 0.98, ymin = 30.5, ymax = 31.0, 
           fill = region_colors["Mid. East"]) +
  annotate("text", x = 0.89, y = 30.75, label = "MID. EAST", 
           size = 12, color = "white", 
           fontface = "bold", family = "roboto_condensed") +
  
  # ==========================================================================
# US DATA AT BOTTOM
# ==========================================================================

# Separator line
annotate("segment", x = -0.3, xend = 1.25, y = 0.2, yend = 0.2, 
         color = "grey10", linewidth = 0.6) +
  
  # --- US RIBBON (faded horizontal) — DRAWN FIRST so text/flags sit on top ---
  geom_segment(
    data = tibble(x = 0.02, xend = 0.98, y = -0.5),
    aes(x = x, xend = xend, y = y, yend = y),
    color = region_colors["Americas"], size = 14, lineend = "butt"
  ) +
  
  # US annotation text
  annotate("text", x = 0.5, y = -0.5, 
           label = "The U.S. ranks 45th and is projected to drop to 48th by 2100", 
           hjust = 0.5, size = 13, fontface = "italic", 
           color = "grey20", family = "roboto_condensed") +
  
  # --- US 2026 LABEL ---
  annotate("text", x = -0.07, y = -0.5, label = "U.S.", 
           hjust = 1, size = 18, fontface = "bold",
           family = "roboto_condensed") +
  
  # --- US 2026 VALUE ---
  annotate("text", x = 0.08, y = -0.5, label = "79.3", 
           hjust = 0.5, size = 18, fontface = "bold", color = "white",
           family = "roboto_condensed") +
  
  # --- US 2026 NODE ---
  geom_point(
    data = tibble(x = 0 - 0.015, y = -0.5),
    aes(x = x, y = y),
    size = 15, color = "black", fill = "white", shape = 21, stroke = 1.0
  ) +
  ggflags::geom_flag(
    data = tibble(x = 0 - 0.015, y = -0.5, iso2 = "us"),
    aes(x = x, y = y, country = iso2),
    size = 13
  ) +
  
  # --- US 2100 VALUE ---
  annotate("text", x = 0.92, y = -0.5, label = "89.2", 
           hjust = 0.5, size = 18, fontface = "bold", color = "white",
           family = "roboto_condensed") +
  
  # --- US 2100 NODE ---
  geom_point(
    data = tibble(x = 1 + 0.015, y = -0.5),
    aes(x = x, y = y),
    size = 15, color = "black", fill = "white", shape = 21, stroke = 1.0
  ) +
  ggflags::geom_flag(
    data = tibble(x = 1 + 0.015, y = -0.5, iso2 = "us"),
    aes(x = x, y = y, country = iso2),
    size = 13
  ) +
  
  # --- US 2100 LABEL ---
  annotate("text", x = 1.08, y = -0.5, label = "U.S.", 
           hjust = 0, size = 18, fontface = "bold",
           family = "roboto_condensed") +
  
  ############## SOURCE ##############

  annotate("text", x = 0.5, y = -2.0, 
           label = "Shows the expected lifespan of a baby born in 2026 and 2100, based on projected mortality rates.",
           hjust = 0.5, size = 24, color = "grey50",
           family = "roboto_condensed") +
  annotate("text", x = 0.5, y = -2.6, 
           label = "Excludes territories and dependencies.", 
           hjust = 0.5, size = 24, color = "grey50",
           family = "roboto_condensed") +
  annotate("text", x = 0.5, y = -3.2, 
           label = "José Caro - @caroisallin - Based on Visual Capitalist", 
           hjust = 0.5, size = 24, color = "grey50",
           family = "roboto_condensed") +
  annotate("text", x = 0.5, y = -3.8, 
           label = "Source: UN World Population Prospects 2024 via Our World in Data", 
           hjust = 0.5, size = 24, color = "grey50",
           family = "roboto_condensed") +
  
  # --- SCALES: Wider margins to prevent cut-off ---
  scale_x_continuous(limits = c(-0.55, 1.55), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-7, 38.5), expand = c(0, 0)) +
  coord_equal(ratio = 0.09) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA),
    legend.position = "none",
    plot.margin = margin(10, 20, 20, 10)
  )

# ============================================================================
# 7. SAVE
# ============================================================================
ggsave("life_expectancy_sankey_complete.png", plot = p, 
       width = 18, height = 24, dpi = 300, bg = bg_color)
