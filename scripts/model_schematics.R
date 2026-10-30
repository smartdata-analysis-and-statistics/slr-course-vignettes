# Rebuild the conceptual teaching figures with: Rscript scripts/model_schematics.R
# Values are illustrative, not fitted to the practical's dataset.
dir.create("figures", showWarnings = FALSE)

draw_model <- function(random = FALSE) {
  navy <- "#26476b"
  green <- "#39752b"
  amber <- "#b86a16"
  muted <- "#5d6873"
  pale <- "#eef4f8"
  mu <- -0.20
  tau <- if (random) 0.22 else 0
  truth <- mu + if (random) c(-0.28, 0.16, -0.10, 0.34) else rep(0, 4)
  error <- c(0.10, -0.12, 0.14, -0.08)
  observed <- truth + error
  se <- c(0.11, 0.15, 0.10, 0.13)
  xx <- function(x) 0.18 + (x + 0.9) / 1.4 * 0.76

  par(mar = c(0, 0, 0, 0), family = "sans", xaxs = "i", yaxs = "i")
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  rect(0, 0, 1, 1, col = "white", border = NA)
  text(0.04, 0.955, if (random) "Random-effects model" else "Fixed-effect (common-effect) model",
       adj = 0, cex = 1.55, font = 2, col = navy)
  text(0.04, 0.909,
       if (random) "Different true effects, plus sampling error" else "One shared true effect, plus sampling error",
       adj = 0, cex = 1.08, col = muted)

  rect(0.035, 0.655, 0.965, 0.865, col = pale, border = NA)
  text(0.055, 0.828, "ASSUMED TRUE EFFECTS", adj = 0, cex = 0.78, font = 2, col = navy)
  segments(xx(-0.9), 0.69, xx(0.5), 0.69, col = "#c2d0db")
  if (random) {
    grid <- seq(-0.9, 0.5, length.out = 500)
    height <- 0.69 + dnorm(grid, mu, tau) / dnorm(mu, mu, tau) * 0.105
    polygon(c(xx(grid), rev(xx(grid))), c(height, rep(0.69, length(grid))),
            col = "#d9eacb", border = NA)
    lines(xx(grid), height, col = green, lwd = 2)
    text(0.945, 0.828, expression(theta[i] %~% N(mu, tau^2)), adj = 1, cex = 1.0, col = green)
    text(0.945, 0.776, expression(paste("Between-study variance: ", tau^2)),
         adj = 1, cex = 0.9, col = green)
  } else {
    arrows(xx(mu), 0.69, xx(mu), 0.795, length = 0.1, lwd = 3, col = green)
    text(0.945, 0.828, expression(theta[i] == theta ~ "for every study"), adj = 1, cex = 1.0, col = green)
    text(0.945, 0.776, expression(paste("Between-study variance: ", tau^2 == 0)),
         adj = 1, cex = 0.9, col = green)
  }
  segments(xx(mu), c(0.223, 0.657), xx(mu), c(0.617, 0.81), col = green, lty = 2, lwd = 1.5)
  text(xx(mu), 0.637, if (random) expression(mu ~ "(mean true effect)") else expression(theta ~ "(common true effect)"),
       cex = 0.9, col = green)

  text(0.055, 0.591, "OBSERVED STUDY ESTIMATES", adj = 0, cex = 0.78, font = 2, col = navy)
  yy <- c(0.535, 0.447, 0.359, 0.271)
  for (i in seq_along(yy)) {
    text(0.055, yy[i], paste("Study", LETTERS[i]), adj = 0, cex = 0.98, col = navy)
    segments(xx(-0.9), yy[i], xx(0.5), yy[i], col = "#edf0f3")
    ends <- xx(observed[i] + c(-1, 1) * 1.96 * se[i])
    segments(ends[1], yy[i], ends[2], yy[i], col = navy, lwd = 1.7)
    segments(ends, yy[i] - 0.008, ends, yy[i] + 0.008, col = navy, lwd = 1.7)
    segments(xx(truth[i]), yy[i], xx(truth[i]), yy[i] + 0.028, col = green)
    segments(xx(observed[i]), yy[i], xx(observed[i]), yy[i] + 0.028, col = amber)
    arrows(xx(truth[i]), yy[i] + 0.028, xx(observed[i]), yy[i] + 0.028,
           code = 2, length = 0.065, col = amber, lwd = 1.5)
    points(xx(truth[i]), yy[i], pch = 23, bg = "white", col = green, cex = 1.35, lwd = 2)
    points(xx(observed[i]), yy[i], pch = 21, bg = navy, col = "white", cex = 1.25, lwd = 0.7)
  }
  ticks <- seq(-0.8, 0.4, by = 0.2)
  segments(xx(-0.9), 0.217, xx(0.5), 0.217, col = muted)
  segments(xx(ticks), 0.217, xx(ticks), 0.209, col = muted)
  text(xx(ticks), 0.19, formatC(ticks, format = "f", digits = 1), cex = 0.84, col = muted)
  text(0.56, 0.155, "Effect size (log risk ratio; 0 = no effect)", cex = 0.92, col = navy)

  points(0.055, 0.109, pch = 23, bg = "white", col = green, cex = 1.15, lwd = 2)
  text(0.075, 0.109, "True effect (unobserved)", adj = 0, cex = 0.81, col = muted)
  segments(0.403, 0.109, 0.453, 0.109, col = navy, lwd = 1.5)
  points(0.428, 0.109, pch = 16, col = navy, cex = 0.95)
  text(0.466, 0.109, "Estimate + 95% CI", adj = 0, cex = 0.81, col = muted)
  arrows(0.735, 0.109, 0.78, 0.109, length = 0.06, col = amber, lwd = 1.5)
  text(0.792, 0.109, "Sampling error", adj = 0, cex = 0.81, col = muted)
  text(0.055, 0.065,
       if (random) expression(y[i] == theta[i] + e[i] ~ ";" ~ Var(y[i]) == tau^2 + v[i])
       else expression(y[i] == theta + e[i] ~ ";" ~ Var(y[i]) == v[i]),
       adj = 0, cex = 1.05, col = navy)
  text(0.945, 0.065, expression(e[i] %~% N(0, v[i])), adj = 1, cex = 1.05, col = navy)
  text(0.055, 0.022, "Illustrative values only. Bars show within-study sampling uncertainty, not between-study spread.",
       adj = 0, cex = 0.76, col = muted)
}

for (random in c(FALSE, TRUE)) {
  stem <- if (random) "random_effects_model" else "fixed_effect_model"
  svglite::svglite(file.path("figures", paste0(stem, ".svg")), width = 10.5, height = 7.5, pointsize = 12)
  draw_model(random)
  dev.off()
  ragg::agg_png(file.path("figures", paste0(stem, ".png")), width = 2100, height = 1500, res = 200)
  draw_model(random)
  dev.off()
}

# A second view: the distribution of estimates if each study were repeated.
# All densities use the same vertical scale, so narrower curves are taller.
draw_sampling <- function(random = FALSE) {
  navy <- "#26476b"
  green <- "#39752b"
  muted <- "#5d6873"
  mu <- -0.20
  tau <- 0.22
  truth <- mu + if (random) c(-0.28, 0.16, -0.10, 0.34) else rep(0, 4)
  observed <- truth + c(0.10, -0.12, 0.14, -0.08)
  se <- c(0.11, 0.15, 0.10, 0.13)
  grid <- seq(-0.9, 0.5, length.out = 800)
  xx <- function(x) 0.18 + (x + 0.9) / 1.4 * 0.70
  density_scale <- 0.020
  par(mar = c(0, 0, 0, 0), family = "sans", xaxs = "i", yaxs = "i")
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  text(0.04, 0.957,
       if (random) "Random effects: two sources of variation" else "Fixed effect: one centre, different precision",
       adj = 0, cex = 1.43, font = 2, col = navy)
  text(0.04, 0.913,
       if (random) "First draw a study's true effect; then sample an observed estimate." else "What if we repeated each study many times under the same conditions?",
       adj = 0, cex = 1.0, col = muted)

  rect(0.035, 0.665, 0.965, 0.872, col = "#f3f9e9", border = NA)
  text(0.055, 0.839, "TRUE EFFECTS ACROSS STUDIES", adj = 0, cex = 0.77, font = 2, col = green)
  text(0.945, 0.839,
       if (random) expression(theta[i] %~% N(mu, tau^2)) else expression(theta[i] == theta ~ ";" ~ tau^2 == 0),
       adj = 1, cex = 1.02, col = green)
  segments(xx(-0.9), 0.699, xx(0.5), 0.699, col = "#b9cbaa")
  if (random) {
    height <- 0.699 + dnorm(grid, mu, tau) * density_scale
    polygon(c(xx(grid), rev(xx(grid))), c(height, rep(0.699, length(grid))),
            col = "#d9eacb", border = NA)
    lines(xx(grid), height, col = green, lwd = 2)
    text(xx(mu), 0.792, expression(paste("Spread of true effects: ", tau)), cex = 0.9, col = green)
    points(xx(truth), rep(0.699, 4), pch = 23, bg = "white", col = green, cex = 1.15, lwd = 2)
  } else {
    arrows(xx(mu), 0.699, xx(mu), 0.794, length = 0.09, lwd = 3, col = green)
    text(0.945, 0.757, "No spread in true effects", adj = 1, cex = 0.91, col = green)
  }
  segments(xx(mu), c(0.151, 0.666), xx(mu), c(0.586, 0.817), col = green, lty = 2, lwd = 1.3)
  text(xx(mu), 0.641,
       if (random) expression(mu ~ "(mean true effect)") else expression(theta ~ "(common true effect)"),
       cex = 0.9, col = green)
  text(0.055, 0.602, "SAMPLING DISTRIBUTIONS WITHIN STUDIES", adj = 0,
       cex = 0.77, font = 2, col = navy)
  bases <- c(0.501, 0.394, 0.287, 0.180)
  for (i in seq_along(bases)) {
    base <- bases[i]
    text(0.055, base + 0.025, paste("Study", LETTERS[i]), adj = 0, cex = 0.95, col = navy)
    text(0.955, base + 0.025, sprintf("SE = %.2f", se[i]), adj = 1, cex = 0.82, col = muted)
    segments(xx(-0.9), base, xx(0.5), base, col = "#d8e1e8")
    height <- base + dnorm(grid, truth[i], se[i]) * density_scale
    polygon(c(xx(grid), rev(xx(grid))), c(height, rep(base, length(grid))),
            col = "#e2ebf4", border = NA)
    lines(xx(grid), height, col = navy, lwd = 1.8)
    segments(xx(truth[i]), base, xx(truth[i]), max(height), col = green, lty = 3, lwd = 1.4)
    points(xx(truth[i]), base, pch = 23, bg = "white", col = green, cex = 1.2, lwd = 2)
    points(xx(observed[i]), base, pch = 21, bg = navy, col = "white", cex = 1.05)
  }
  ticks <- seq(-0.8, 0.4, by = 0.2)
  segments(xx(-0.9), 0.145, xx(0.5), 0.145, col = muted)
  segments(xx(ticks), 0.145, xx(ticks), 0.138, col = muted)
  text(xx(ticks), 0.121, formatC(ticks, format = "f", digits = 1), cex = 0.8, col = muted)
  text(0.53, 0.087, "Effect size (log risk ratio; 0 = no effect)", cex = 0.91, col = navy)
  points(0.055, 0.049, pch = 23, bg = "white", col = green, cex = 1.05, lwd = 2)
  text(0.073, 0.049, "True effect", adj = 0, cex = 0.80, col = muted)
  points(0.28, 0.049, pch = 16, col = navy, cex = 0.85)
  text(0.298, 0.049, "One observed estimate", adj = 0, cex = 0.80, col = muted)
  text(0.945, 0.049,
       if (random) expression(y[i]~"|"~theta[i] %~% N(theta[i], v[i])) else expression(y[i] %~% N(theta, v[i])),
       adj = 1, cex = 1.0, col = navy)
  text(0.055, 0.015, "Illustrative values. Curve width reflects standard error within a study; narrower curves mean greater precision.",
       adj = 0, cex = 0.73, col = muted)
}

for (random in c(FALSE, TRUE)) {
  stem <- if (random) "random_effects_sampling" else "fixed_effect_sampling"
  svglite::svglite(file.path("figures", paste0(stem, ".svg")), width = 10.5, height = 8.0, pointsize = 12)
  draw_sampling(random)
  dev.off()
  ragg::agg_png(file.path("figures", paste0(stem, ".png")), width = 2100, height = 1600, res = 200)
  draw_sampling(random)
  dev.off()
}

# Descriptive diagram for the unweighted mean log RR. Unlike the fixed-effect
# figures, this shows observed study estimates, not latent true effects.
draw_unweighted <- function() {
  navy <- "#26476b"
  green <- "#39752b"
  amber <- "#b86a16"
  muted <- "#5d6873"
  d <- readRDS("ongoing_pregnancy_deae274.rds")
  d$rr <- (d$biosimilar_events / d$biosimilar_total) /
    (d$originator_events / d$originator_total)
  d$yi <- log(d$rr)
  mean_yi <- mean(d$yi)
  par(mar = c(0, 0, 0, 0), family = "sans", xaxs = "i", yaxs = "i")
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  text(0.04, 0.957, "Unweighted average of study log risk ratios",
       adj = 0, cex = 1.45, font = 2, col = navy)
  text(0.04, 0.912, "Each study estimate contributes equally, whatever its precision",
       adj = 0, cex = 1.0, col = muted)
  text(0.06, 0.855, "OBSERVED STUDY LOG-RRs", adj = 0, cex = 0.78, font = 2, col = navy)
  text(0.88, 0.855, "One equal contribution per study", adj = 0.5,
       cex = 0.78, font = 2, col = amber)
  x <- function(yi) 0.34 + (yi + 0.5) / 1.0 * 0.52
  ys <- seq(0.79, 0.46, length.out = nrow(d))
  for (i in seq_len(nrow(d))) {
    label <- sub(" et al.,", "", d$study_label[i])
    text(0.06, ys[i], label, adj = 0, cex = 0.78, col = navy)
    segments(x(-0.5), ys[i], x(0.5), ys[i], col = "#e8edf1")
    points(x(d$yi[i]), ys[i], pch = 21, bg = navy, col = "white", cex = 1.2)
    rect(0.88 - 0.026, ys[i] - 0.012, 0.88 + 0.026, ys[i] + 0.012,
         col = "#f8eadb", border = amber)
    text(0.88, ys[i], "1/7", cex = 0.66, col = amber)
  }
  axis_ticks <- seq(-0.4, 0.4, by = 0.2)
  segments(x(-0.5), 0.42, x(0.5), 0.42, col = muted)
  segments(x(axis_ticks), 0.42, x(axis_ticks), 0.412, col = muted)
  text(x(axis_ticks), 0.393, formatC(axis_ticks, format = "f", digits = 1),
       cex = 0.78, col = muted)
  segments(x(0), 0.41, x(0), 0.825, col = "#8a969f", lty = 2, lwd = 1.2)
  text(x(0), 0.835, "Log RR = 0 (RR = 1)", adj = 0.5, cex = 0.72, col = muted)
  segments(x(mean_yi), 0.41, x(mean_yi), 0.825, col = amber, lty = 2, lwd = 2)
  text(x(mean_yi), 0.438, sprintf("Mean = %.3f", mean_yi),
       adj = 0.5, cex = 0.78, font = 2, col = amber)
  text(0.60, 0.355, "Study log RR (biosimilar / GONAL-f)", cex = 0.88, col = navy)

  text(0.06, 0.295, "EQUAL-WEIGHT AVERAGE ON THE LOG SCALE", adj = 0, cex = 0.78, font = 2, col = navy)
  rect(0.06, 0.13, 0.31, 0.26, col = "#eef4f8", border = "#cbd8e1")
  text(0.185, 0.208, expression(hat(y)[1]), cex = 1.15, col = navy)
  text(0.375, 0.195, "+", cex = 1.3, col = muted)
  rect(0.43, 0.13, 0.68, 0.26, col = "#eef4f8", border = "#cbd8e1")
  text(0.555, 0.208, expression(hat(y)[2]), cex = 1.15, col = navy)
  text(0.735, 0.195, "+  ...  +", cex = 1.0, col = muted)
  rect(0.84, 0.13, 0.94, 0.26, col = "#eef4f8", border = "#cbd8e1")
  text(0.89, 0.208, expression(hat(y)[7]), cex = 1.15, col = navy)
  text(0.50, 0.092,
       sprintf("Mean log RR = %.3f; geometric mean RR = %.3f", mean_yi, exp(mean_yi)),
       cex = 0.92, col = amber)
  text(0.06, 0.039,
       "Geometric mean of observed study RRs; each study has weight 1/7, with no precision weighting.",
       adj = 0, cex = 0.72, col = muted)
}

svglite::svglite("figures/unweighted_average_logrr.svg", width = 10.5, height = 7.5, pointsize = 12)
draw_unweighted()
dev.off()
ragg::agg_png("figures/unweighted_average_logrr.png", width = 2100, height = 1500, res = 200)
draw_unweighted()
dev.off()
