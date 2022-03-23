
#' Title
#'
#' @param EventStudy_estimate
#' @param num_sim
#' @param conf_level
#' @param seed
#'
#' @return
#' @import MASS
#' @export
#'
#' @examples
#'

AddSuptBand <- function(EventStudy_estimate, num_sim, conf_level, seed = 1234) {

    if ((class(EventStudy_estimate) != "lm_robust") & (typeof(EventStudy_estimate) != "list")) {
    stop("EventStudy_estimate is not a data frame with coefficient estimates and standard errors")
    }
    if (! is.numeric(num_sim) | num_sim %% 1 != 0) {stop("num_sim should be a whole number.")}
    if (! is.numeric(conf_level) | conf_level < 0 | conf_level > 1) {stop("conf_level should be a rational number between 0 and 1, inclusive.")}
    if (! is.numeric(seed) | seed %%  1 != 0) {stop("seed should be an integer.")}

    vcov_matrix <- EventStudy_estimate$vcov
    v_std_errors <- t(sqrt(diag(vcov_matrix)))
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

    data.frame("term" = EventStudy_estimate$term,
               "lower" = (EventStudy_estimate$coefficients) - (critical_value * EventStudy_estimate$std.error),
               "upper" = (EventStudy_estimate$coefficients) + (critical_value * EventStudy_estimate$std.error)
               )
}
