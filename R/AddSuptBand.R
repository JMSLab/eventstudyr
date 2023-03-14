
#' Produces sup-t bands for the event-study coefficients.
#'
#' @description Source code from Ryan Kessler (2022). [suptCriticalValue](https://github.com/ryanedmundkessler/suptCriticalValue): Computes critical values
#' underlying simultaneous sup-t confidence bands. R package version
#' 0.1.0.
#'
#' @param estimates The first element extracted from [EventStudy()]. Should be a list.
#' @param num_sim The number of simulations used in generating the sup-t bands.
#' Should be a natural number. Defaults to 1000.
#' @param conf_level The confidence level used for obtaining the sup-t bands critical value.
#' Should be a real number between 0 and 1, inclusive. Defaults to .95.
#' @param seed The pseudorandom state used to make drawing "random" numbers reproducible.
#' Should be a natural number.
#' Defaults to 1234.
#' @param eventstudy_coefficients The names of the event-study coefficients. This vector is
#' outputted in the second element of the [EventStudy()] function. Should be a vector of strings.
#'
#' @return A data.frame that contains the upper and lower sup-t band values
#' for each event-study coefficient.
#' @import estimatr
#' @importFrom MASS mvrnorm
#' @keywords internal
#' @noRd
#'
#' @examples
#' eventstudy_estimates <- EventStudy(
#'   estimator = "OLS",
#'   data = df_sample_dynamic,
#'   outcomevar = "y_base",
#'   policyvar = "z",
#'   idvar = "id",
#'   timevar = "t",
#'   controls = "x_r",
#'   FE = TRUE,
#'   TFE = TRUE,
#'   post = 3,
#'   pre = 2,
#'   overidpre = 4,
#'   overidpost = 5,
#'   normalize = - 3,
#'   cluster = TRUE,
#'   anticipation_effects_normalization = TRUE
#' )
#'
#' AddSuptBand(
#'   estimates = eventstudy_estimates$output,
#'   num_sim = 100,
#'   conf_level = .95,
#'   seed = 1234,
#'   eventstudy_coefficients = eventstudy_estimates$arguments$eventstudy_coefficients
#')

AddSuptBand <- function(estimates, num_sim = 1000, conf_level = .95, seed = 1234, eventstudy_coefficients) {

    if (! class(estimates) %in% c("lm_robust", "iv_robust")) {
        stop("estimates is not a data frame with coefficient estimates and standard errors")
    }
    if (! is.numeric(num_sim) | num_sim %% 1 != 0 | num_sim <= 0) {stop("num_sim should be a natural number.")}
    if (! is.numeric(conf_level) | conf_level < 0 | conf_level > 1) {stop("conf_level should be a real number between 0 and 1, inclusive.")}
    if (! is.numeric(seed) | seed %%  1 != 0) {stop("seed should be an integer.")}
    if (! is.character(eventstudy_coefficients)) {stop("eventstudy_coefficients should be a character.")}

    vcov_matrix_all <- estimates$vcov
    v_terms_to_keep <- colnames(vcov_matrix_all) %in% eventstudy_coefficients
    vcov_matrix <- vcov_matrix_all[v_terms_to_keep, v_terms_to_keep]

    v_std_errors <- t(sqrt(diag(vcov_matrix)))
    set.seed(seed)
    draws <- MASS::mvrnorm(n = num_sim, mu = rep(0, nrow(vcov_matrix)), Sigma = vcov_matrix)
    t <- draws / (v_std_errors %x% matrix(rep(1, num_sim)))

    t <- apply(abs(t), 1, FUN = max)
    t <- sort(t)

    conf_level_num_sim <- conf_level * num_sim

    if (round(conf_level_num_sim) == conf_level_num_sim) {
        critical_value = (t[conf_level_num_sim] + t[conf_level_num_sim + 1]) / 2
    } else {
        critical_value = t[floor(conf_level_num_sim) + 1]
    }

    df_estimates_tidy <- estimatr::tidy(estimates)

    df_estimates_tidy["suptband_lower"] <- df_estimates_tidy$estimate - (critical_value * df_estimates_tidy$std.error)
    df_estimates_tidy["suptband_upper"] <- df_estimates_tidy$estimate + (critical_value * df_estimates_tidy$std.error)


    return(df_estimates_tidy)

}
