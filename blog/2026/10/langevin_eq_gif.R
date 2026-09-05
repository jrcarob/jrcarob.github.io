# Langevin 2D simulation + GIF animation
#
# Requirements:
#   install.packages(c("ggplot2","gifski"))
#
# Usage:
#   Rscript langevin_eq_gif.R
#
# This script:
# - integrates a 2D Langevin equation with RK4 (stochastic term sampled on each derivative evaluation)
# - writes a dat file similar to the original
# - creates PNG frames sampling every `skip` steps and assembles them into Langevin_eq.gif

library(ggplot2)
library(gifski)

# -------- Parameters (match the original .plt) ----------
m       <- 1
gamma   <- 1
# r in original is unused
dt      <- 1.0e-2      # time step
dh      <- dt / 6.0    # RK4 coefficient (unused directly here)
dt2     <- dt / 2.0
t_fin   <- 100         # final time (numbre of seconds)
limit   <- ceiling(t_fin / dt)  # number of calculation steps

skip        <- 40                 # skip frames when making gif
noise_amp   <- 5e1                # corresponds to "R" in the original
L           <- 1.5e2              # plotting window length

# Output and initialization
outputfile <- "langevin_data_gif.dat"

# Optional: reproducible results
# set.seed(123)

# --- helper: derivative function ---
# state is numeric vector: c(x, y, vx, vy)
f <- function(state, t) {
  x  <- state[1]
  y  <- state[2]
  vx <- state[3]
  vy <- state[4]

  # dx/dt = vx, dy/dt = vy
  # dvx/dt = -gamma/m * vx + noise_amp * Gaussian()   (noise sampled on each call)
  # dvy/dt = -gamma/m * vy + noise_amp * Gaussian()
  c(
    vx,
    vy,
    - (gamma / m) * vx + noise_amp * rnorm(1),
    - (gamma / m) * vy + noise_amp * rnorm(1)
  )
}

# --- RK4 step for vector system ---
rk4_step <- function(state, t, dt, f) {
  k1 <- f(state, t)
  k2 <- f(state + (dt / 2) * k1, t + dt / 2)
  k3 <- f(state + (dt / 2) * k2, t + dt / 2)
  k4 <- f(state + dt * k3, t + dt)
  state + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
}

# Prepare storage
n_steps <- limit
res <- data.frame(
  t = numeric(n_steps + 1),
  x = numeric(n_steps + 1),
  y = numeric(n_steps + 1),
  vx = numeric(n_steps + 1),
  vy = numeric(n_steps + 1)
)

# initial conditions (match original)
state <- c(0.0, 0.0, 10.0, 10.0)  # x, y, vx, vy
t     <- 0.0

res[1, ] <- c(t, state)

# Write header to dat file (rounded to 2 decimals as original)
cat(sprintf("# dt=%.2f, m=%.2f, gamma=%.2f, R=%.2f\n", dt, m, gamma, noise_amp),
    file = outputfile)
cat("# t\t x\t y\t vx\t vy\n", file = outputfile, append = TRUE)
cat(sprintf("%.2f\t%.2f\t%.2f\t%.2f\t%.2f\n",
            round(t, 2), round(state[1], 2), round(state[2], 2),
            round(state[3], 2), round(state[4], 2)),
    file = outputfile, append = TRUE)

# Time integration loop
for (i in 1:n_steps) {
  t <- t + dt
  state <- rk4_step(state, t, dt, f)
  res[i + 1, ] <- c(t, state)

  cat(sprintf("%.2f\t%.2f\t%.2f\t%.2f\t%.2f\n",
              round(t, 2), round(state[1], 2), round(state[2], 2),
              round(state[3], 2), round(state[4], 2)),
      file = outputfile, append = TRUE)
}
message("Finish calculation!")

# ---------------- Animation (PNG frames -> GIF) ----------------
png_dir <- "frames_langevin"
if (!dir.exists(png_dir)) dir.create(png_dir)

frame_idxs <- seq(1, nrow(res), by = skip)  # indices of frames to produce
png_files <- character(length(frame_idxs))

xlim <- c(-L / 2, L / 2)
ylim <- c(-L / 2, L / 2)

for (fi in seq_along(frame_idxs)) {
  idx <- frame_idxs[fi]
  df_path <- res[1:idx, , drop = FALSE]
  last_point <- res[idx, ]

  # title like: italic(t) = 1.2 s   (we build expression to get italic t)
  time_val <- last_point$t
  title_expr <- bquote(italic(t) == .(format(round(time_val, 1), nsmall = 1)) ~ "s")

  p <- ggplot(df_path, aes(x = x, y = y)) +
    geom_path(color = "royalblue", size = 0.7) +
    geom_point(data = last_point, aes(x = x, y = y), color = "royalblue", size = 3) +
    coord_fixed(xlim = xlim, ylim = ylim) +
    labs(x = expression(italic(x)), y = expression(italic(y))) +
    ggtitle(title_expr) +
    theme_minimal(base_size = 18) +
    theme(
      panel.grid = element_line(color = "grey85"),
      axis.title = element_text(size = 18),
      plot.title = element_text(size = 20)
    )

  png_file <- file.path(png_dir, sprintf("frame_%05d.png", fi))
  png_files[fi] <- png_file
  ggsave(filename = png_file, plot = p, width = 960/96, height = 720/96, dpi = 96)
  # (width/height in inches: pixels / dpi)
}

# assemble gif using gifski
gif_file <- "Langevin_eq.gif"
# original gnuplot used "delay 8" which corresponds to 0.08 s per frame
gifski(png_files, gif_file = gif_file, width = 960, height = 720, delay = 0.08)
message("Finish animation! Output:", gif_file)

# (optional) clean up png frames:
# unlink(png_dir, recursive = TRUE)