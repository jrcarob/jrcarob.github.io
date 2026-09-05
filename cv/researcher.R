library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# 1. Dataset in Tidy Format
df <- bind_rows(
  data.frame(
    Year = rep(2019:2026, 1),
    Item = "Journal articles",
    Type = rep(c("whole", "fractional"), each = 8),
    Count = c(2, 3, 5, 6, 7, 7, 7, 9, 
              2, 3, 3.42, 3.75, 4.00, 4.00, 4.00, 4.58)
  ),
  #data.frame(
  #  Year = rep(2015:2026, 2),
  #  Item = "Preprints, WPs",
  #  Type = rep(c("whole", "fractional"), each = 12),
  #  Count = c(1, 1, 1, 1, 3, 3, 4, 4, 9, 10, 12, 17,
  #            0.2, 0.2, 0.2, 0.2, 1.0, 1.0, 1.33, 1.33, 4.00, 4.50, 5.00, 6.20)
  #),
  data.frame(
    Year = rep(2019:2027, 2),
    Item = "Presentations",
    Type = rep(c("whole", "fractional"), each = 9),
    Count = c(1, 3, 5, 7, 10, 11, 16, 21, 21,
              1, 1.67, 3.00, 3.58, 4.42, 4.75, 6.42, 7.83, 7.83)
  ),
  data.frame(
    Year = rep(c(2021, 2022, 2023, 2024, 2025, 2026, 2027), 2),
    Item = "Google Scholar",
    Type = rep(c("whole", "fractional"), each = 7),
    Count = rep(c(1, 3, 9, 22, 44, 61, 61), 1)
  ),
  data.frame(
    Year = rep(c(2021, 2022, 2023, 2024, 2025, 2026, 2027), 2),
    Item = "Scopus",
    Type = rep(c("whole", "fractional"), each = 7),
    Count = rep(c(1, 2, 5, 8, 14, 22, 31), 1)
  )
  # data.frame(
  #   Year = rep(c(2013, 2019, 2022, 2023, 2024, 2025, 2026), 2),
  #   Item = "Courses, tutorials",
  #   Type = rep(c("whole", "fractional"), each = 7),
  #   Count = rep(c(4, 5, 10, 11, 14, 17, 19), 2)
  # )
)

# Enforce factor order
df$Item <- factor(df$Item, levels = c(
  "Journal articles", 
#  "Preprints, WPs", 
  "Presentations", 
  "Google Scholar",
  "Scopus"
))
df$Type <- factor(df$Type, levels = c("fractional", "whole"))

# Color definitions
cols <- c(
  "Journal articles"  = "#2B7BBA",
  "Preprints, WPs"    = "#D95F02",
  "Presentations"     = "#7570B3",
  "Google Scholar"    = "#1B9E77",
  "Scopus"            = "#D95F02"
)

# Helper function to create subplots
base_plot <- function(data_subset, y_max, y_breaks) {
  ggplot(data_subset, aes(x = Year, y = Count, color = Item, linetype = Type)) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.3, show.legend = FALSE) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.8) +
    scale_x_continuous(breaks = 2018:2027, limits = c(2018.5, 2027.5)) +
    scale_y_continuous(limits = c(-4, y_max), breaks = y_breaks) +
    scale_color_manual(values = cols) +
    scale_linetype_manual(values = c("fractional" = "dashed", "whole" = "solid")) +
    theme_light(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.minor = element_blank(),
      legend.title = element_blank()
    ) +
    labs(x = NULL, y = NULL)
}

# 2. Build Subplots
p1 <- base_plot(filter(df, Item %in% c("Journal articles")), 21, seq(0, 20, 5)) +
  labs(y = "Cumulative output") +
  annotate("text", x = 2019.2, y = 20, label = "Publications", fontface = "bold", hjust = 0, size = 3.8) +
  annotate("text", x = 2019.2, y = 14, label = "Journals/Books:\nβ whole: 9\nβ fractional: 4.58", color = "#2B7BBA", hjust = 0, size = 3) # +
  # annotate("text", x = 2019.2, y = 14, label = "Preprints/WPs:\nβ whole: 1.423\nβ fractional: 0.564", color = "#D95F02", hjust = 0, size = 3)

p2 <- base_plot(filter(df, Item == "Presentations"), 30, seq(0, 30, 5)) +
  labs(x = "Year") +
  annotate("text", x = 2019.2, y = 25, label = "Presentations", fontface = "bold", hjust = 0, size = 3.8) +
  annotate("text", x = 2019.2, y = 18, label = "β whole: 21\nβ fractional: 7.83", color = "black", hjust = 0, size = 3)

p3 <- base_plot(filter(df, Item %in% c("Google Scholar", "Scopus")), 70, seq(0, 70, 10)) +
  annotate("text", x = 2019.2, y = 60, label = "Citations", fontface = "bold", hjust = 0, size = 3.8) +
  annotate("text", x = 2019.2, y = 50, label = "Google Scholar", color = "#1B9E77", hjust = 0, size = 3)+
  annotate("text", x = 2019.2, y = 43, label = "Scopus", color = "#D95F02", hjust = 0, size = 3)

# 3. Assemble with Patchwork
final_plot <- (p1 | p2 | p3) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Cumulative whole and fractional count of publications, presentations, and\ncitations (2018 - October 2026) by item type",
    subtitle = "Fractional counting considers the inverse number of authors involved in a publication as collaborators' contribution."
  ) &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10.5)
  )

print(final_plot)
