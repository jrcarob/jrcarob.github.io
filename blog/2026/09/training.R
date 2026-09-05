# Calendar Heatmap in R
# This script creates a calendar heatmap visualization

# Load required libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(readxl)  # For reading Excel files

# Read data from Excel file
# Replace "your_file.xlsx" with the path to your Excel file
# file_path <- "your_file.xlsx"  # Change this to your actual file path
# sheet_name <- "Sheet1"         # Change this if your data is in a different sheet

# Read the Excel file
kms <- read_excel("blog/2026/09/running.xlsx", sheet = "2025")

# Clean and prepare the data
kms <- kms %>%
  # Ensure date column is properly formatted
  mutate(date = as.Date(date)) %>%
  # Remove any rows with missing dates or kms
  filter(!is.na(date), !is.na(kms)) %>%
  # Ensure kms is numeric
  mutate(kms = as.numeric(kms)) %>%
  # Remove any negative values or outliers if needed
  filter(kms >= 0)

# Display basic info about the data
cat("Data Summary:\n")
cat("Date range:", as.character(min(kms$date)), "to", as.character(max(kms$date)), "\n")
cat("Total days:", nrow(kms), "\n")
cat("Total kilometers:", sum(kms$kms, na.rm = TRUE), "km\n")
cat("Average daily kilometers:", round(mean(kms$kms, na.rm = TRUE), 2), "km\n")

# Prepare the daily data for a calendar layout (weekday, week-of-month, monthly totals)
create_kms_calendar_heatmap <- function(data, title = "Daily Kilometers Calendar Heatmap") {

  # Use data as-is (already has date and kms columns)
  calendar_data_prep <- data %>%
    rename(value = kms)  # Rename kms to value for consistency with the plotting function

  # Calculate monthly totals for display in labels
  monthly_totals <- calendar_data_prep %>%
    mutate(
      year = year(date),
      month = month(date)
    ) %>%
    group_by(year, month) %>%
    summarise(
      total_kms = sum(value, na.rm = TRUE),
      .groups = 'drop'
    )

  # Add calendar components
  calendar_data <- calendar_data_prep %>%
    mutate(
      year = year(date),
      month = month(date),
      day = day(date),
      week = week(date),
      weekday = wday(date, week_start = 1), # Monday = 1
      month_name = month(date, label = TRUE, abbr = TRUE),
      # Calculate week of month
      week_of_month = ceiling(day / 7)
    ) %>%
    # Adjust week calculation for proper calendar layout
    group_by(year, month) %>%
    mutate(# Get the first day of the month
      first_day_of_month = floor_date(date, "month"),
      # Get the weekday of the first day (1=Monday, 7=Sunday)
      first_weekday = wday(first_day_of_month, week_start = 1),
      # Calculate which week of the month this day belongs to
      # We need to account for the offset created by the first day's position
      week_of_month = ceiling((day + first_weekday - 1) / 7)
    ) %>%
    ungroup()

  # Merge with monthly totals and create labels with totals
  calendar_data <- calendar_data %>%
    left_join(monthly_totals, by = c("year", "month")) %>%
    mutate(
      month_year_label = paste0(month_name, " ", year, "\n(", round(total_kms, 1), " km)"),
      # Create a proper date for ordering (first day of each month)
      month_year_date = as.Date(paste(year, month, "01", sep = "-"))
    ) %>%
    arrange(month_year_date)

  # Get unique month-year combinations in chronological order
  month_order <- calendar_data %>%
    select(month_year_label, month_year_date) %>%
    distinct() %>%
    arrange(month_year_date) %>%
    pull(month_year_label)

  # Convert month_year_label to factor with chronological levels
  calendar_data$month_year_label <- factor(calendar_data$month_year_label,
                                           levels = month_order)

  calendar_data
}

# Draw the calendar heatmap from data already prepared by create_kms_calendar_heatmap()
plot_kms_calendar <- function(calendar_data, title = "Daily Kilometers Calendar Heatmap") {
  ggplot(calendar_data, aes(x = weekday, y = -week_of_month, fill = value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    facet_wrap(~ month_year_label, ncol = 4, scales = "free_y") +
    scale_x_continuous(
      breaks = 1:7,
      labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
      position = "top"
    ) +
    scale_y_continuous(breaks = NULL) +
    scale_fill_gradient2(
      low = "lightgray",
      mid = "yellow",
      high = "darkred",
      midpoint = median(calendar_data$value, na.rm = TRUE),
      name = "Kilometers"
    ) +
    labs(
      title = title,
      subtitle = paste("Year:", paste(unique(calendar_data$year), collapse = ", ")),
      x = "",
      y = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.text.x.top = element_text(size = 9, margin = margin(t = 5, b = 5)),
      axis.text.x.bottom = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.spacing = unit(1, "lines"),
      strip.text = element_text(size = 11, face = "bold", margin = margin(b = 8)),
      strip.background = element_blank(),
      strip.placement = "outside",
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "bottom"
    )
}

# Create and display the kilometers calendar heatmap
kms_calendar_plot <- plot_kms_calendar(
  create_kms_calendar_heatmap(kms),
  "Daily Kilometers Calendar Heatmap"
)
print(kms_calendar_plot)

# Save the plot (optional)
# ggsave("kms_calendar_heatmap.png", kms_calendar_plot, width = 12, height = 8, dpi = 300)

# Alternative: Using viridis color scheme (good for accessibility)
kms_calendar_viridis <- plot_kms_calendar(
  create_kms_calendar_heatmap(kms),
  "Daily Kilometers"
) +
  scale_fill_viridis_c(name = "Kilometers", option = "plasma", trans = "sqrt")

print(kms_calendar_viridis)

# Monthly summary statistics
monthly_stats <- kms %>%
  mutate(
    year_month = floor_date(date, "month")
  ) %>%
  group_by(year_month) %>%
  summarise(
    total_kms = sum(kms, na.rm = TRUE),
    avg_daily_kms = mean(kms, na.rm = TRUE),
    max_daily_kms = max(kms, na.rm = TRUE),
    days_with_data = n(),
    .groups = 'drop'
  )

print("Monthly Summary:")
print(monthly_stats)

# Alternative version with zero values for missing dates
# (useful if you want to show all days of the year, even those without data)
create_complete_calendar <- function(data) {
  # Get full date range
  date_range <- seq(floor_date(min(data$date), "year"), 
                    ceiling_date(max(data$date), "year") - days(1), 
                    by = "day")
  
  # Create complete date sequence and merge with data
  complete_data <- data.frame(date = date_range) %>%
    left_join(data, by = "date") %>%
    mutate(kms = ifelse(is.na(kms), 0, kms))  # Fill missing with 0
  
  return(complete_data)
}

# Uncomment the lines below if you want to include zero values for missing dates
# kms_data_complete <- create_complete_calendar(kms_data)
# complete_calendar_plot <- create_kms_calendar_heatmap(
#   kms_data_complete, 
#   "Complete Calendar - Daily Kilometers (Missing Days = 0)"
# )
# print(complete_calendar_plot)
