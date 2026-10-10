# Projected age distribution of Spain, 1950-2100 (UN WPP 2024, medium variant).
# Data: spain_wpp2024.csv = rows for LocID 724 from
# WPP2024_PopulationByAge5GroupSex_Medium.csv.gz (population.un.org/wpp).

library(dplyr)
library(ggplot2)
library(grid)
library(showtext)

font_add_google("Bebas Neue", "bebas")
font_add_google("Space Mono", "spacemono")
showtext_opts(dpi = 200)
showtext_auto()

script_arg <- grep("--file=", commandArgs(FALSE), value = TRUE)
here <- if (interactive()) "." else dirname(sub("--file=", "", script_arg))
wpp <- read.csv(file.path(here, "spain_wpp2024.csv"), fileEncoding = "UTF-8-BOM")

groups <- c("0-9", "10-19", "20-29", "30-39", "40-49",
            "50-59", "60-69", "70-79", "80-89", "≥90")
d <- wpp |>
  mutate(group = groups[pmin(AgeGrpStart %/% 10, 9) + 1]) |>
  summarise(pop = sum(PopTotal), .by = c(Time, group)) |>
  mutate(pct = 100 * pop / sum(pop), .by = Time) |>
  mutate(group = factor(group, levels = rev(groups))) |>   # first level stacks on top
  arrange(Time, desc(group)) |>
  mutate(mid = cumsum(pct) - pct / 2, .by = Time)        # band centre, from the bottom

fills <- c("#0B4A8C", "#1565C8", "#3F8DE6", "#8FBDF2", "#DCEAFB",
           "#F7E0E0", "#E07A84", "#D3303F", "#9C2330", "#55161B")
names(fills) <- groups
dark <- groups[c(1:3, 7:10)]                               # white text on these

inner <- filter(d, Time %in% seq(1960, 2090, 10))
# Edge boxes sit at their band centre, pushed down only when they would overlap
spread <- function(y, gap = 3.2) {
  o <- order(-y)
  z <- y[o]
  z[1] <- min(z[1], 103)                                  # allow one step above
  for (i in seq_along(z)[-1]) z[i] <- min(z[i], z[i - 1] - gap)
  y[o] <- z
  y
}
# Slivers under 1.5% get small unboxed text stacked above the panel corner,
# youngest lowest (d is already sorted bottom band first)
edges <- filter(d, Time %in% c(1950, 2100))
tiny <- filter(edges, pct < 1.5) |>
  mutate(ylab = 100 + 1.4 * row_number(), .by = Time)
edges <- filter(edges, pct >= 1.5) |>
  mutate(ylab = spread(mid), .by = Time)

p <- ggplot(d, aes(Time, pct, fill = group)) +
  geom_area(colour = "black", linewidth = 0.25) +
  geom_vline(xintercept = seq(1960, 2090, 10),
             colour = "white", alpha = 0.15, linewidth = 0.3) +
  geom_text(data = inner, aes(y = mid, label = sprintf("%.1f", pct),
                              colour = group %in% dark,
                              size = ifelse(pct < 1.5, 1.6, 2.6)),
            family = "bebas") +
  # Border as a layer (not panel.border) so the edge boxes can sit on top of it
  annotate("rect", xmin = 1950, xmax = 2100, ymin = 0, ymax = 100,
           fill = NA, colour = "black", linewidth = 2.2) +
  geom_label(data = edges, aes(y = ylab, label = sprintf("%.1f", pct)),
             fill = "white", colour = "black", family = "bebas", size = 3.4,
             label.r = unit(0, "pt"), linewidth = 0.5) +
  geom_text(data = tiny, aes(y = ylab, label = sprintf("%.1f", pct)),
            family = "bebas", size = 2.2, colour = "black") +
  geom_text(data = filter(edges, Time == 2100),
            aes(x = 2104.5, y = ylab, label = group),
            hjust = 0, family = "bebas", size = 3, colour = "black") +
  annotate("text", x = 2088, y = 103.2, label = "AGE GROUPS", hjust = 1,
           family = "bebas", size = 4.2) +
  annotate("segment", x = 2089, xend = 2107, y = 103.2, yend = 103.2,
           linewidth = 0.4) +
  annotate("segment", x = 2107, xend = 2107, y = 103.2, yend = 100.6,
           linewidth = 0.4,
           arrow = arrow(length = unit(1.6, "mm"), type = "closed")) +
  scale_fill_manual(values = fills, guide = "none") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "black"),
                      guide = "none") +
  scale_size_identity() +
  scale_x_continuous(breaks = seq(1950, 2100, 10), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(1950, 2100), ylim = c(0, 100), clip = "off") +
  theme_void(base_family = "bebas") +
  theme(axis.text.x = element_text(size = 12, margin = margin(t = 4)),
        plot.margin = margin(22, 34, 6, 16),
        plot.background = element_rect(fill = "white", colour = NA))

subtitle <- paste(
  "Spain has one of the lowest fertility rates in Europe and",
  "one of the longest life expectancies in the world. This",
  "chart shows how the age distribution has changed since",
  "1950, including projections to 2100. Values are in",
  "percentages, with the % sign omitted to save space.", sep = "\n")

out <- file.path(here, "spain_age_distribution.png")
png(out, width = 8.1, height = 10.8, units = "in", res = 200)
grid.newpage()
grid.rect(gp = gpar(fill = "white", col = NA))
# Header: flag (civil flag, 1:2:1 stripes), title, subtitle
fv <- viewport(x = 0.03, y = 0.985, width = 0.235, height = 0.13, just = c("left", "top"))
pushViewport(fv)
grid.rect(y = c(7/8, 1/2, 1/8), height = c(1/4, 1/2, 1/4),
          gp = gpar(fill = c("#AA151B", "#F1BF00", "#AA151B"), col = NA))
grid.rect(gp = gpar(fill = NA, col = "black", lwd = 4))
popViewport()
grid.text("PROJECTED AGE DISTRIBUTION OF SPAIN", x = 0.285, y = 0.962, just = "left",
          gp = gpar(fontfamily = "bebas", fontsize = 31))
grid.text(subtitle, x = 0.285, y = 0.932, just = c("left", "top"),
          gp = gpar(fontfamily = "spacemono", fontsize = 9.6, lineheight = 1.15))
# Chart
print(p, vp = viewport(y = 0.03, height = 0.81, just = "bottom"))
# Footer
grid.text("DATA: UN WORLD POPULATION PROSPECTS 2024 (MEDIUM VARIANT)", x = 0.97, y = 0.016,
          just = "right", gp = gpar(fontfamily = "bebas", fontsize = 9))
grid.text("@caroisallin", x = 0.03, y = 0.016,
          just = "left", gp = gpar(fontfamily = "bebas", fontsize = 9))
invisible(dev.off())
message("Wrote ", out)
