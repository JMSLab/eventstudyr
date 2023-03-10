#' Estimates Equation (2) in [Freyaldenhoven et al. (2021)](https://www.nber.org/system/files/working_papers/w29170/w29170.pdf).
#'
#' @param estimator Accepts one of "OLS" or "FHS". If "FHS" is specified, implements IV estimator in Freyaldenhoven et al. 2019.
#' @param data Data frame that contains the variables of interest.
#' @param outcomevar Character indicating column of outcome variable y. # Instead of "should be a character" change to "Character for variable indicating ..." for concision? - MZW
#' @param policyvar Character indicating column of policy variable z.
#' @param idvar Character indicating column of units.
#' @param timevar Character indicating column of time periods.
#' @param controls Character indicating optional vector of controls q. # What is the q for? - MZW
#' @param proxy Character indicating column of variable that is thought to be affected by the confound but not by the policy.
#' Should be specified if and only if estimator is specified as "FHS".
#' @param proxyIV Character of column to be used as an instrument. Should be specified if and only if estimator is specified as "FHS".
#' If NULL, defaults to the strongest lead of the policy variable based on the first stage.
#' @param FE Specifies if unit fixed-effects should be included. Defaults to TRUE.
#' @param TFE Specifies if time fixed-effects should be included. Defaults to TRUE.
#' @param cluster Specifies whether to use clustered errors by units. If FALSE, will use unclustered
#' heteroskedasticity-robust standard errors. Defaults to TRUE. Must be TRUE if FE is TRUE.
#' @param post Whole number indicating the number of periods in the past before which the past values of the policy
#' are not supposed to affect the value of the outcome. Corresponds to M in equation (2) of
#' [Freyaldenhoven et al. (2021)](https://www.nber.org/system/files/working_papers/w29170/w29170.pdf).
#' @param overidpost Optional whole number indicating the number of event times after "post" to be included in estimation. Defaults to 1.
#' Corresponds to L_M in equation (2) of [Freyaldenhoven et al. (2021)](https://www.nber.org/system/files/working_papers/w29170/w29170.pdf).  # Delete "should be a whole number" and instead say "Optional whole number of event times..." ? - MZW
#' @param pre Whole number indicating the number of periods in the future after which the future values of the policy are
#' not supposed to affect the value of the outcome today. Corresponds to G in equation (2) of
#' [Freyaldenhoven et al. (2021)](https://www.nber.org/system/files/working_papers/w29170/w29170.pdf).
#' @param overidpre Optional whole number indicating the number of event times earlier than -"pre" to be included in estimation. Defaults to "post" + "pre".
#' Corresponds to L_G in equation (2) of [Freyaldenhoven et al. (2021)](https://www.nber.org/system/files/working_papers/w29170/w29170.pdf).
#' @param normalize Specifies the event-time coefficient to be normalized. Defaults to - pre - 1.
#' @param anticipation_effects_normalization If set to TRUE, runs default process and switches coefficient to be normalized to 0
#' when there are anticipation effects. If set to FALSE, does not make switch. Defaults to TRUE  # This variable name is very long - MZW
#'
#' @return A list that contains the estimation output and an object containing the arguments passed to the function
#' @import dplyr
#' @export
#'
#' @examples
#'
#' # A minimal example
#' eventstudy_model <-
#'   EventStudy(
#'     estimator = "OLS",
#'     data = df_sample_dynamic,
#'     outcomevar = "y_base",
#'     policyvar = "z",
#'     idvar = "id",
#'     timevar = "t",
#'     pre = 0, post = 3,
#'     normalize = -1
#'   )
#'
#' ### Access estimates
#' eventstudy_model$output
#'
#' ### Access arguments
#' eventstudy_model$arguments
#'
#' # A dynamic OLS model with anticipation effects and controls
#' eventstudy_model_dyn <-
#'   EventStudy(
#'     estimator = "OLS",
#'     data = df_sample_dynamic,
#'     outcomevar = "y_base",
#'     policyvar = "z",
#'     idvar = "id",
#'     timevar = "t",
#'     controls = "x_r",
#'     FE = TRUE, TFE = TRUE,
#'     post = 3, overidpost = 5,
#'     pre  = 2, overidpre  = 4,
#'     normalize = - 3,
#'     cluster = TRUE,
#'     anticipation_effects_normalization = TRUE
#'   )
#'
#' eventstudy_model_dyn$output
#'
#' # A static model
#' eventstudy_model_static <-
#'   EventStudy(
#'     estimator = "OLS",
#'     data = df_sample_static,
#'     outcomevar = "y_static",
#'     policyvar = "z",
#'     idvar = "id",
#'     timevar = "t",
#'     FE = TRUE, TFE = TRUE,
#'     post = 0, overidpost = 0,
#'     pre  = 0, overidpre  = 0,
#'     cluster = TRUE
#'   )
#'
#' eventstudy_model_static$output
#'
#' # A dynamic model estimated using IV
#' eventstudy_model_iv <-
#'   EventStudy(
#'     estimator = "FHS",
#'     data = df_sample_dynamic,
#'     outcomevar = "y_base",
#'     policyvar = "z",
#'     idvar = "id",
#'     timevar = "t",
#'     controls = "x_r",
#'     proxy = "eta_m",
#'     FE = TRUE, TFE = TRUE,
#'     post = 2, overidpost = 1,
#'     pre  = 0, overidpre  = 3,
#'     normalize = -1,
#'     cluster = TRUE
#'   )
#'
#' eventstudy_model_iv$output
#'
