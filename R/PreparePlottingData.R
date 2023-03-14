
#' Orders the eventstudy coefficients and generates the x-axis labels
#'
#' @param df_tidy_estimates A data.frame created from applying \link[estimatr]{tidy}
#' to the estimation output from [EventStudy()].
#' At a minimum, it contains a column called "term" with the name for the coefficient and a
#' column called "estimate" that contains the corresponding estimate. Should be a data.frame.
#' @param policyvar Character indicating column of policy variable z.
# @param post Whole number indicating the number of periods in the past before which the past values of the policy
#' are not supposed to affect the value of the outcome. Corresponds to M in equation (2) of
#' [Freyaldenhoven et al. (2021)](https://www.nber.org/system/files/working_papers/w29170/w29170.pdf).
#' @param overidpost Optional whole number indicating the number of event times after "post" to be included in estimation. Defaults to 1.
#' Corresponds to L_M in equation (2) of [Freyaldenhoven et al. (2021)](https://www.nber.org/system/files/working_papers/w29170/w29170.pdf).
#' @param pre Whole number indicating the number of periods in the future after which the future values of the policy are
#' not supposed to affect the value of the outcome today. Corresponds to G in equation (2) of
#' [Freyaldenhoven et al. (2021)](https://www.nber.org/system/files/working_papers/w29170/w29170.pdf).
#' @param overidpre Optional whole number indicating the number of event times earlier than -"pre" to be included in estimation. Defaults to "post" + "pre".
#' Corresponds to L_G in equation (2) of [Freyaldenhoven et al. (2021)](https://www.nber.org/system/files/working_papers/w29170/w29170.pdf).
#' @param normalization_column The name of the column containing the coefficient that will
#' be set to 0 in the eventstudy plot. Should be a character.
#' @param proxyIV Character of column to be used as an instrument. Should be specified if and only if estimator is specified as "FHS".
#' If NULL, defaults to the strongest lead of the policy variable based on the first stage.
#'
#' @return A data.frame that contains the x-axis labels, y-axis estimates,
#' and optional plot aesthetics to be used in creating the eventstudy plot
#' @import stringr
#' @rawNamespace import(stats, except=c(lag, filter))
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#'
#' tidy_eventstudy_estimates <- estimatr::tidy(EventStudy(estimator = "OLS",
#'                                        data = df_sample_dynamic,
#'                                        outcomevar = "y_base",
#'                                        policyvar = "z",
#'                                        idvar = "id",
#'                                        timevar = "t",
#'                                        controls = "x_r",
#'                                        FE = TRUE,
#'                                        TFE = TRUE,
#'                                        post = 3, overidpost = 5,
#'                                        pre = 2,  overidpre = 4,
#'                                        normalize = - 3,
#'                                        anticipation_effects_normalization = TRUE)$output)
#'
#' PreparePlottingData(df_tidy_estimates = tidy_eventstudy_estimates,
#'                     policyvar = "z",
#'                     post = 3, overidpost = 5,
#'                     pre = 2,  overidpre = 4,
#'                     normalization_column = "z_fd_lead3",
#'                     proxyIV = NULL)
#'
#' # If you would like to use IV regression:
#' data <- df_sample_dynamic[, c("y_base", "z", "id", "t", "x_r", "eta_m")]
#'
#' tidy_eventstudy_estimates <- estimatr::tidy(EventStudy(estimator = "FHS",
#'                                        data = data,
#'                                        outcomevar = "y_base",
#'                                        policyvar = "z",
#'                                        idvar = "id",
#'                                        timevar = "t",
#'                                        controls = "x_r",
#'                                        proxy = "eta_m",
#'                                        FE = TRUE,
#'                                        TFE = TRUE,
#'                                        post = 1, overidpost = 2,
#'                                        pre = 1,  overidpre = 2,
#'                                        normalize = -1,
#'                                        anticipation_effects_normalization = TRUE)$output)
#'
#' PreparePlottingData(df_tidy_estimates = tidy_eventstudy_estimates,
#'                     policyvar = "z",
#'                     post = 1, overidpost = 2,
#'                     pre = 1,  overidpre = 2,
#'                     normalization_column = "z_fd_lead2",
#'                     proxyIV = "z_fd_lead3")
#'

PreparePlottingData <- function(df_tidy_estimates, policyvar,
                                post, overidpost, pre, overidpre, normalization_column,
                                proxyIV = NULL) {

    if (! is.data.frame(df_tidy_estimates)) {stop("data should be a data frame.")}
    if (! is.character(policyvar)) {stop("policyvar should be a character.")}
    if (! (is.numeric(post) & post >= 0 & post %% 1 == 0)) {stop("post should be a whole number.")}
    if (! (is.numeric(overidpost) & overidpost >= 0 & overidpost %% 1 == 0)) {stop("overidpost should be a whole number.")}
    if (! (is.numeric(pre) & pre >= 0 & pre %% 1 == 0)) {stop("pre should be a whole number.")}
    if (! (is.numeric(overidpre) & overidpre >= 0 & overidpre %% 1 == 0)) {stop("overidpre should be a whole number.")}
    if (! is.character(normalization_column)) {stop("normalization_column should be a character.")}
    if (normalization_column %in% df_tidy_estimates$term) {stop("normalization_column should not be one of the strings in the 'term' column.")}
    if (! (is.null(proxyIV) | is.character(proxyIV))) {stop("proxyIV should be either a character or NULL.")}

    largest_lead <- pre + overidpre
    largest_lag  <- post + overidpost - 1

    integer_regex <- "[0-9]+$"

    first_lead         <- paste0(policyvar, "_lead", largest_lead)
    first_lead_integer <- as.integer(stringr::str_extract(first_lead, integer_regex)) + 1
    first_lead_label   <- paste0("-", first_lead_integer, "+")

    v_leads         <- paste0(policyvar, "_fd_lead", largest_lead:1)
    v_leads_integer <- stringr::str_extract(v_leads, integer_regex)
    v_leads_label   <- paste0("-", v_leads_integer)

    t_0_term       <- paste0(policyvar, "_fd")
    t_0_term_label <- "0"

    v_lags       <- paste0(policyvar, "_fd_lag", 1:largest_lag)
    v_lags_label <- stringr::str_extract(v_lags, integer_regex)

    last_lag         <- paste0(policyvar, "_lag", largest_lag + 1)
    last_lag_integer <- stringr::str_extract(last_lag, integer_regex)
    last_lag_label   <- paste0(last_lag_integer, "+")

    v_terms_to_plot_ordered <- c(first_lead, v_leads, t_0_term, v_lags, last_lag)
    v_terms_to_plot_labels  <- c(first_lead_label, v_leads_label, t_0_term_label, v_lags_label, last_lag_label)

    v_all_terms      <- df_tidy_estimates[["term"]]
    v_plotting_terms <- v_all_terms %in% v_terms_to_plot_ordered

    df_plotting <- df_tidy_estimates[v_plotting_terms, ]

    v_zeroes              <- rep(0, ncol(df_plotting) - 2)
    v_names_for_zeroes    <- names(df_plotting)[3:ncol(df_plotting)]
    v_normalization_other <- stats::setNames(v_zeroes, v_names_for_zeroes)

    supt_or_ci_present <- v_names_for_zeroes %in% c("suptband_lower", "suptband_upper", "ci_lower", "ci_upper")

    if (sum(supt_or_ci_present) > 0) {

        v_normalization_other[which(supt_or_ci_present)] <- NA

    }


    df_normalization_column <- data.frame(
        "term"     = normalization_column,
        "estimate" = 0,
        as.list(v_normalization_other)
    )

    df_plotting <- rbind(df_plotting, df_normalization_column)

    if (!is.null(proxyIV)) {

        proxyIV_integer <- stringr::str_extract(proxyIV, integer_regex)
        proxyIV_lead_or_lag <- stringr::str_extract(proxyIV, "lead|lag")

        proxyIV_sign <- switch (proxyIV_lead_or_lag,
            "lead" = "-",
            "lag" = "+"
        )

        df_proxyIV_column <- data.frame(
            "term"     = proxyIV,
            "estimate" = 0,
            as.list(v_normalization_other)
        )

        proxyIV_label <- paste0(proxyIV_integer, proxyIV_sign)

        df_plotting <- rbind(df_plotting, df_proxyIV_column)

    }


    df_plotting["label"] <- factor(df_plotting$term, levels = v_terms_to_plot_ordered, labels = v_terms_to_plot_labels)

    return(df_plotting)

}
