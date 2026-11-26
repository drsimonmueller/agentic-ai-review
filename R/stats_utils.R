# statistical utilities

format_pval <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 1e-300 ~ "P < 1.0e-300",
    p < 0.001 ~ paste0("P = ", formatC(p, format = "e", digits = 1)),
    TRUE ~ paste0("P = ", formatC(p, format = "f", digits = 3))
  )
}

p_to_stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ "ns"
  )
}

# wilson binomial proportion 95% confidence interval
wilson_ci <- function(k, n, conf_level = 0.95) {
  stopifnot(length(k) == length(n))
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  p <- k / n
  denom <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  half <- (z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n)) / denom
  tibble::tibble(
    k = as.integer(k),
    n = as.integer(n),
    prop = p,
    conf_low = pmax(0, center - half),
    conf_high = pmin(1, center + half)
  )
}

# 2x2 fisher exact helper returning odds ratio, ci, and p-value
run_fisher_2x2 <- function(a, b, c_val, d) {
  mat <- matrix(c(a, b, c_val, d), nrow = 2L, byrow = TRUE)
  ft <- stats::fisher.test(mat)
  tibble::tibble(
    odds_ratio = unname(as.numeric(ft$estimate)),
    conf_low   = unname(as.numeric(ft$conf.int[1L])),
    conf_high  = unname(as.numeric(ft$conf.int[2L])),
    p_value    = unname(as.numeric(ft$p.value))
  )
}

# two-sided wilcoxon rank-sum test summary
run_wilcox_pair <- function(x, y, exact = NULL) {
  x_fin <- x[is.finite(x)]
  y_fin <- y[is.finite(y)]
  wt <- stats::wilcox.test(x_fin, y_fin, exact = exact)
  tibble::tibble(
    n_x       = length(x_fin),
    n_y       = length(y_fin),
    median_x  = stats::median(x_fin),
    median_y  = stats::median(y_fin),
    statistic = unname(as.numeric(wt$statistic)),
    p_value   = unname(as.numeric(wt$p.value))
  )
}
