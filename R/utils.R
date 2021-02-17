
get_jumps <- function(x) {
  diff(c(0, environment(x)$y))
}

get_cutoffs <- function(x) {
  sort(unique(c(environment(x$pos_ecdf)$x, environment(x$neg_ecdf)$x, -Inf)), decreasing = TRUE)
}

get_method <- function(x) {
  attr(x, "method", TRUE)
}

get_singularities <- function(x) {
  attr(x, "singularities", exact = TRUE)
}

margin <- function(val_range) {
  ifelse(val_range[2] > val_range[1], 0.05 * diff(val_range), 2)
}

pretty_lim <- function(val) {
  val_range <- range(val, na.rm = FALSE)
  val_range + margin(val_range) * c(-1, 1)
}

# --------------------------------------------------------
# Plot points

plot_points <- function(x, y,
                        xlim, ylim,
                        exact = (length(x) <= sample_n),
                        sample_n = 1000,
                        xlab = expression(x), ylab = expression(y),
                        add = FALSE, col = par("col"), pch = 21, cex = 1, bg = getOption("rkt_pride_colors"),
                        draw_grid = !add, h = NULL, v = NULL, grid_col = "gray83", grid_lty = 2, grid_lwd = 1,
                        ...) {
  # ------------------------------------------
  # input validation

  if (missing(xlim)) {
    if (length(x) == 0) {
      stop("Provide xlim or x")
    }
  }

  if (missing(ylim)) {
    if (length(y) == 0) {
      stop("Provide ylim or y")
    }
  }

  if (length(x) != length(y)) {
    stop("\"x\" and \"y\" musst have the same length")
  }

  if (!exact) {
    ids <- sort(sample(seq_along(x), sample_n, replace = FALSE))
    x <- x[ids]
    y <- y[ids]
    message(sprintf("Approximate plot using a sample of %i points.", length(x)))
  }

  # ------------------------------------------
  # plot

  if (!add) {
    if (missing(xlim)) {
      xlim <- pretty_lim(x)
    }
    if (missing(ylim)) {
      ylim <- pretty_lim(y)
    }
    old_par <- par(mar = c(5, 5, 2, 2) + 0.1)
    on.exit(par(old_par), add = TRUE)

    plot(NA, NA, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ...)
  }

  if (draw_grid) {
    abline(h = h, v = v, col = grid_col, lty = grid_lty, lwd = grid_lwd)
  }

  points(x, y, pch = pch, cex = cex, col = col, bg = bg, ...)

}

# --------------------------------------------------------
# Plot functions

plot_function <- function(f, xlim, ylim,
                          inter_points_n = 1000,
                          sing_points = get_singularities(f),
                          exact = (length(sing_points) <= 1000),
                          xlab = expression(x), ylab = expression(f(x)),
                          add = FALSE, col = par("col"), lty = par("lty"), lwd = par("lwd"),
                          draw_points = (points_n <= 100), pch = 21, cex = 1, points_col = col, points_bg = getOption("rkt_pride_colors"),
                          draw_limits = (limits_n <= 10), limits_col = col, limits_lty = 3, limits_lwd = 1,
                          draw_grid = !add, h = NULL, v = NULL, grid_col = "gray83", grid_lty = 2, grid_lwd = 1,
                          draw_area = FALSE, density = NULL, angle_set = 45, area_col = "gray93",
                          ...) {
  # ------------------------------------------
  # input validation

  if (missing(xlim)) {
    if (length(sing_points) == 0) {
      stop("Provide xlim or singularities")
    } else {
      xlim <- pretty_lim(sing_points)
    }
  } else {
    mask <- xlim[1] <= sing_points & sing_points <= xlim[2]
    sing_points <- sing_points[mask]
  }

  if (!exact) {
    sing_points <- c()
    message(sprintf("Approximate plot using %i interpolation points and %i singularity points.", inter_points_n, length(sing_points)))
  }

  # ------------------------------------------
  # select points for plot

  x <- seq(xlim[1], xlim[2], length.out = inter_points_n)
  x_add <- c(sing_points - 2*.Machine$double.eps, sing_points + 2*.Machine$double.eps)
  x <- sort(unique(c(x, x_add)))
  x <- setdiff(x, sing_points)
  mask <- xlim[1] <= x & x <= xlim[2]
  x <- x[mask]
  y <- f(x)
  mask <- is.finite(y)
  x <- x[mask]
  y <- y[mask]

  # ------------------------------------------
  # classify singularity points

  sing_points_values <- vapply(sing_points, f, FUN.VALUE = 1.0)

  mask <- is.finite(sing_points_values)
  points_x <- sing_points[mask]
  points_y <- sing_points_values[mask]
  points_n <- sum(mask)

  mask <- is.infinite(sing_points_values)
  limits_x <- sing_points[mask]
  limits_n <- length(limits_x)

  # ------------------------------------------
  # plot

  if (!add) {
    if (missing(ylim)) {
      if (draw_points) {
        ylim <- pretty_lim(c(y, points_y))
      } else {
        ylim <- pretty_lim(y)
      }
    }
    old_par <- par(mar = c(5, 5, 2, 2) + 0.1)
    on.exit(par(old_par), add = TRUE)

    plot(NA, NA, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ...)
  }

  if (draw_grid) {
    abline(h = h, v = v, col = grid_col, lty = grid_lty, lwd = grid_lwd)
  }

  if (draw_area){
    vertices_x <- c(x[1], x, x[length(x)])
    vertices_y <- c(0, y, 0)

    for (angle in angle_set) {
      polygon(vertices_x, vertices_y, col = area_col, border = NA, density = density, angle = angle)
    }
  }

  curve_id <- cut(x, breaks = sort(unique(c(xlim, sing_points))), right = TRUE, include.lowest = TRUE)

  for (cid in levels(curve_id)) {
    mask <- cid == curve_id
    lines(x[mask], y[mask], col = col, lty = lty, lwd = lwd, ...)
  }

  if (draw_limits) {
    abline(v = limits_x, col = limits_col, lty = limits_lty, lwd = limits_lwd)
  }

  if (draw_points) {
    points(points_x, points_y, pch = pch, cex = cex, col = points_col, bg = points_bg)
  }
}

# --------------------------------------------------------
# Plot curves

plot_curve <- function(f, xlim, ylim, slim,
                       inter_points_n = 1000,
                       sing_points = sort(unique(c(get_singularities(f$x), get_singularities(f$y)))),
                       exact = (length(sing_points) <= 1000),
                       xlab = expression(x), ylab = expression(y),
                       add = FALSE, col = par("col"), lty = par("lty"), lwd = par("lwd"),
                       draw_points = (points_n <= 100), pch = 21, cex = 1, points_col = col, points_bg = getOption("rkt_pride_colors"),
                       draw_grid = !add, h = NULL, v = NULL, grid_col = "gray83", grid_lty = 2, grid_lwd = 1,
                       draw_area = FALSE, density = NULL, angle_set = 45, area_col = "gray93",
                       ...) {
  # ------------------------------------------
  # input validation

  if (missing(slim)) {
    if (length(sing_points) == 0) {
      stop("Provide slim or singularities")
    } else {
      slim <- pretty_lim(sing_points)
    }
  } else {
    mask <- slim[1] <= sing_points & sing_points <= slim[2]
    sing_points <- sing_points[mask]
  }

  if (!exact) {
    sing_points <- c()
    message(sprintf("Approximate plot using %i interpolation points and %i singularity points.", inter_points_n, length(sing_points)))
  }

  # ------------------------------------------
  # select points for plot

  s <- seq(slim[1], slim[2], length.out = inter_points_n)
  s_add <- c(sing_points - 2*.Machine$double.eps, sing_points + 2*.Machine$double.eps)
  s <- sort(unique(c(s, s_add)))
  s <- setdiff(s, sing_points)
  mask <- slim[1] <= s & s <= slim[2]
  s <- s[mask]

  x <- f$x(s)
  y <- f$y(s)

  mask <- is.finite(x) & is.finite(y)
  s <- s[mask]
  x <- x[mask]
  y <- y[mask]

  # ------------------------------------------
  # classify singularity points

  x_d_values <- vapply(sing_points, f$x, FUN.VALUE = 1.0)
  y_d_values <- vapply(sing_points, f$y, FUN.VALUE = 1.0)

  mask <- is.finite(x_d_values) & is.finite(y_d_values)
  points_x <- x_d_values[mask]
  points_y <- y_d_values[mask]
  points_n <- sum(mask)

  # ------------------------------------------
  # plot

  if (!add) {
    if (missing(xlim)) {
      xlim <- pretty_lim(x)
    }
    if (missing(ylim)) {
      ylim <- pretty_lim(y)
    }
    old_par <- par(mar = c(5, 5, 2, 2) + 0.1)
    on.exit(par(old_par), add = TRUE)

    plot(NA, NA, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ...)
  }

  if (draw_grid) {
    abline(h = h, v = v, col = grid_col, lty = grid_lty, lwd = grid_lwd)
  }

  if (draw_area){
    vertices_x <- c(x[1], x, x[length(x)])
    vertices_y <- c(0, y, 0)

    for (angle in angle_set) {
      polygon(vertices_x, vertices_y, col = area_col, border = NA, density = density, angle = angle)
    }
  }

  curve_id <- cut(s, breaks = sort(unique(c(slim, sing_points))), right = TRUE, include.lowest = TRUE)

  for (cid in levels(curve_id)) {
    mask <- cid == curve_id
    lines(x[mask], y[mask], col = col, lty = lty, lwd = lwd, ...)
  }

  if (draw_points) {
    points(points_x, points_y, pch = pch, cex = cex, col = points_col, bg = points_bg)
  }
}
