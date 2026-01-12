# ==============================================================================
# modeling/fit_nb_gamm.R — Negative Binomial GAMM Fitting
# ==============================================================================
# PURPOSE
# -------
# Fit the canonical Negative Binomial GAM model for calls-per-night data with
# an effort offset (log recording_hours), a smooth temporal trend, and hierarchical
# random effects for site and detector.
#
# This function exists to enforce a single, auditable model specification used
# in Production inference.
# =============================================================================

#' Fit canonical Negative Binomial GAM for bat activity
#'
#' @export
fit_nb_gamm <- function(df, smooth_k = 7, method = "REML", quiet = FALSE) {

  # -------------------------
  # Input validation
  # -------------------------
  if (!is.data.frame(df)) stop("df must be a data frame")

  if (length(smooth_k) != 1) stop("smooth_k must be a length-1 numeric/integer value (e.g., 7)")
  smooth_k <- as.integer(smooth_k)
  if (is.na(smooth_k) || smooth_k < 3) stop("smooth_k must be an integer >= 3")

  required_cols <- c("calls_per_night", "recording_hours", "night", "habitat", "site", "detector_id")
  assert_columns_exist(df, required_cols, context = "for NB GAMM fitting")

  assert_column_type(df, "recording_hours", "numeric")
  assert_column_type(df, "calls_per_night", "numeric")

  if (any(is.na(df$recording_hours))) {
    stop(sprintf(
      "recording_hours contains %d NA value(s); cannot fit offset(log(recording_hours))",
      sum(is.na(df$recording_hours))
    ))
  }
  if (any(df$recording_hours <= 0)) {
    stop(sprintf(
      "recording_hours contains %d value(s) <= 0; cannot fit offset(log(recording_hours))",
      sum(df$recording_hours <= 0)
    ))
  }
  if (!inherits(df$night, "Date")) stop("night must be of class Date")

  # -------------------------
  # Derive time scaling (canonical)
  # -------------------------
  df_model <- df %>%
    mutate(
      habitat = as.factor(habitat),
      site = as.factor(site),
      detector_id = as.factor(detector_id),
      night_scaled = as.numeric(scale(as.numeric(night)))
    )

  if (!quiet) {
    message("Fitting Negative Binomial GAM (canonical spec)...")
    message(sprintf("  Rows: %s", format(nrow(df_model), big.mark = ",")))
    message(sprintf("  smooth_k: %d", smooth_k))
  }

  # -------------------------
  # Fit model
  # -------------------------
  # IMPORTANT: use mgcv specials as plain s(), not mgcv::s() inside formulas.
  model_formula <- stats::as.formula(sprintf(
    "calls_per_night ~ habitat +
      s(night_scaled, k = %d) +
      s(site, bs = 're') +
      s(detector_id, bs = 're') +
      offset(log(recording_hours))",
    smooth_k
  ))

  model <- mgcv::gam(
    formula = model_formula,
    family = mgcv::nb(),
    data = df_model,
    method = method
  )

  if (!quiet) message("✓ Model fit complete")
  model
}
