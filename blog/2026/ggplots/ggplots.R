library(tidyverse)
library(ggridges)
library(patchwork)

p1 <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  geom_smooth() +
  labs(title = "1: geom_point() + geom_smooth()") +
  theme(plot.title = element_text(face = "bold"))

p2 <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_hex() +
  labs(title = "2: geom_hex()") +
  guides(fill = FALSE) +
  theme(plot.title = element_text(face = "bold"))

p3 <- ggplot(mpg, aes(x = drv, fill = drv)) +
  geom_bar() +
  labs(title = "3: geom_bar()") +
  guides(fill = FALSE) +
  theme(plot.title = element_text(face = "bold"))

p4 <- ggplot(mpg, aes(x = cty)) +
  geom_histogram(binwidth = 2, color = "white") +
  labs(title = "4: geom_histogram()") +
  theme(plot.title = element_text(face = "bold"))

p5 <- ggplot(mpg, aes(x = cty, y = drv, fill = drv)) +
  geom_violin() +
  guides(fill = FALSE) +
  labs(title = "5: geom_violin()") +
  theme(plot.title = element_text(face = "bold"))

p6 <- ggplot(mpg, aes(x = cty, y = drv, fill = drv)) +
  geom_boxplot() +
  guides(fill = FALSE) +
  labs(title = "6: geom_boxplot()") +
  theme(plot.title = element_text(face = "bold"))

p7 <- ggplot(mpg, aes(x = cty, fill = drv)) +
  geom_density(alpha = 0.7) +
  guides(fill = FALSE) +
  labs(title = "7: geom_density()") +
  theme(plot.title = element_text(face = "bold"))

p8 <- ggplot(mpg, aes(x = cty, y = drv, fill = drv)) +
  geom_density_ridges() +
  guides(fill = FALSE) +
  labs(title = "8: ggridges::geom_density_ridges()") +
  theme(plot.title = element_text(face = "bold"))

p9 <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_density_2d() +
  labs(title = "9: geom_density_2d()") +
  theme(plot.title = element_text(face = "bold"))

p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 +
  plot_layout(nrow = 3)
