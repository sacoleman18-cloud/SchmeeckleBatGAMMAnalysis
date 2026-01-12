# ==============================================================================
# modeling/extract_habitat_effects.R — Habitat Effects Extraction
# ==============================================================================
# PURPOSE
# -------
# Extract habitat fixed-effect contrasts from the fitted NB GAM and present them
# as interpretable rate ratios (IRR) and percent differences (relative to a
# reference habitat).
# =============================================================================

#' Extract habitat effects as IRR and percent difference
#'
#' @description
#' Extracts parametric habitat coefficients from an mgcv::gam fit and converts:
#' - log(IRR) -> IRR = exp(beta)
#' - 95% CI on IRR using beta ± 1.96*SE
#' - percent difference = (IRR - 1) * 100
#'
#' Assumes the model includes `habitat` as a parametric factor term.
#'
#' @param model mgcv::gam object from fit_nb_gamm().
#' @param reference Character. The reference habitat used in factor coding.
#'   Default = "Interior".
#'
#' @return Tibble with habitat contrast rows and columns:
#'   term, estimate_log, se, irr, irr_low, irr_high, pct_diff, p_value
#'
#' @section CONTRACT:
#' - Returns IRR and CI for each non-reference habitat level coefficient
#' - Does not refit the model
#'
#' @section DOES NOT:
#' - Perform overall habitat factor tests (use model comparison separately)
#' - Handle custom contrasts beyond default treatment coding
#'
#' @export
extract_habitat_effects <- function(model, reference = "Interior") {

  if (!inherits(model, "gam")) stop("model must be an mgcv::gam object")

  sm <- summary(model)
  ptab <- as.data.frame(sm$p.table)

  ptab$term <- rownames(ptab)
  rownames(ptab) <- NULL

  habitat_rows <- ptab %>%
    filter(grepl("^habitat", term))

  if (nrow(habitat_rows) == 0) {
    stop("No habitat coefficients found. Ensure habitat is included as a parametric term in the model.")
  }

  out <- habitat_rows %>%
    transmute(
      term = term,
      estimate_log = Estimate,
      se = `Std. Error`,
      p_value = `Pr(>|z|)`,
      irr = exp(estimate_log),
      irr_low = exp(estimate_log - 1.96 * se),
      irr_high = exp(estimate_log + 1.96 * se),
      pct_diff = (irr - 1) * 100
    )

  attr(out, "reference_habitat") <- reference
  out
}
