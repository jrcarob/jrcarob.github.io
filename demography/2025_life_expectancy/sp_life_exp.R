# Load libraries
library(tidyverse)
library(scales)

# 1. READ the file (Year is "1908-1912", not a number)
# ~/Desktop/Omega/00_Projects/01_Paper_Projects/00_ongoing/1stGROUP/demography2/R Codes/MortalityLaws
lt <- read_table("~/Desktop/Phi/site_personal_web/blog/2025/12/bltper_1x5.txt", 
                 skip = 2,
                 col_types = cols(
                   Year = col_character(),
                   Age  = col_character(),
                   ex   = col_double(),
                   .default = col_character()
                 )) %>%
  filter(!is.na(ex)) %>%
  mutate(Age = as.numeric(str_remove(Age, "\\+"))) %>%
  mutate(start_year = as.numeric(str_sub(Year, 1, 4))) %>%
  mutate(x = start_year) %>%
  select(x, Age, ex)

# 2. Filter for the specific age groups
# Important: ex is REMAINING life expectancy, so total = Age + ex
df <- lt %>%
  filter(Age %in% c(0, 10, 25, 45, 65, 80)) %>%
  mutate(
    total_life_exp = Age + ex,  # Total life expectancy
    age_group = case_when(
      Age == 0  ~ "At birth",
      Age == 10 ~ "10 year old",
      Age == 25 ~ "25 year old",
      Age == 45 ~ "45 year old",
      Age == 65 ~ "65 year old",
      Age == 80 ~ "80 year old"
    )
  ) %>%
  mutate(age_group = factor(age_group,
                            levels = c("At birth", "10 year old", "25 year old",
                                       "45 year old", "65 year old", "80 year old")))

# Get the first data point for each age group for label positioning
first_points <- df %>%
  group_by(age_group) %>%
  slice_min(x, n = 1) %>%
  ungroup()

# 3. Create the plot
ggplot(df, aes(x = x, y = total_life_exp, color = age_group)) +
  geom_line(linewidth = 1.2) +
  
  # Event markers as lollipops (solid line with dot at top)
  annotate("segment", x = 1918, xend = 1918, y = 30, yend = 90, 
           color = "gray70", linewidth = 0.5) +
  annotate("point", x = 1918, y = 90, color = "gray70", size = 2.5) +
  
  annotate("segment", x = 1936, xend = 1936, y = 30, yend = 90, 
           color = "gray70", linewidth = 0.5) +
  annotate("point", x = 1936, y = 90, color = "gray70", size = 2.5) +
  
  annotate("segment", x = 1941, xend = 1941, y = 30, yend = 90, 
           color = "gray70", linewidth = 0.5) +
  annotate("point", x = 1941, y = 30, color = "gray70", size = 2.5) +
  
  # Event labels
  annotate("text", x = 1918, y = 95, label = "1918\nSpanish\nFlu", 
           size = 3.5, color = "gray50", lineheight = 0.9) +
  annotate("text", x = 1936, y = 95, label = "1936\nSpanish\nCivil War", 
           size = 3.5, color = "gray50", lineheight = 0.9) +
  annotate("text", x = 1941, y = 26, label = "1941\nPost-War crisis", 
           size = 3.5, color = "gray50", lineheight = 0.9) +
  annotate("text", x = 2020, y = 95, label = "2021\nCovid-19", 
           size = 3.5, color = "gray50", lineheight = 0.9) +
  
  # Age group labels at the beginning of each line (using actual first data points)
  geom_text(data = first_points, 
            aes(x = x - 1, y = total_life_exp, label = age_group, color = age_group),
            hjust = 1, fontface = "bold", size = 4, show.legend = FALSE) +
  
  # Y-axis labels on the right
  annotate("text", x = 2025, y = 90, label = "90 years", 
           color = "gray60", size = 5, hjust = 0) +
  annotate("text", x = 2025, y = 80, label = "80 years", 
           color = "gray60", size = 5, hjust = 0) +
  annotate("text", x = 2025, y = 70, label = "70 years", 
           color = "gray60", size = 5, hjust = 0) +
  annotate("text", x = 2025, y = 60, label = "60 years", 
           color = "gray60", size = 5, hjust = 0) +
  annotate("text", x = 2025, y = 50, label = "50 years", 
           color = "gray60", size = 5, hjust = 0) +
  annotate("text", x = 2025, y = 40, label = "40 years", 
           color = "gray60", size = 5, hjust = 0) +
  annotate("text", x = 2025, y = 30, label = "30 years", 
           color = "gray60", size = 5, hjust = 0) +
  
  # Scales
  scale_x_continuous(breaks = c(1910, 1950, 2023),
                     #labels = c("1910", "1950", "2023"),
                     limits = c(1885, 2040),
                     #sequence(1900, 2025, by = 10),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(22, 99.9),
                     breaks = seq(30, 90, 10),
                     expand = c(0, 0)) +
  
  # Color scheme matching the chart
  scale_color_manual(values = c(
    "At birth"    = "#5778bb",
    "10 year old" = "#c85b3e",
    "25 year old" = "#bc8331",
    "45 year old" = "#6fac91",
    "65 year old" = "#9c70ad",
    "80 year old" = "#b04959"
  )) +
  
  # Labels
  labs(title = "Life expectancy for Spanish people\nof different ages",
       subtitle = "Total period life expectancy for people who have reached a given age.",
       x = NULL,
       y = NULL,
       caption = "Data source: Human Mortality Database (2024); UN WPP (2024); @caroisallin. R code at: https://github.com/jrcarob") +
  # Theme
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 26, face = "bold", hjust = 0, 
                              margin = margin(b = 5), family = "sans"),
    plot.subtitle = element_text(size = 14, hjust = 0, color = "gray40",
                                 margin = margin(b = 20)),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray80", linewidth = 0.5, linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "gray40", size = 14),
    axis.ticks.x = element_blank(),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0,
                                margin = margin(t = 15)),
    plot.margin = margin(15, 10, 15, 15),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Save the plot
ggsave("life_expectancy_spain.png", width = 12, height = 8, dpi = 300, bg = "white")
