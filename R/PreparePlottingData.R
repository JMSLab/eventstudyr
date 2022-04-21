

#' Title
#'
#' @param df_tidy_estimates
#' @param policyvar
#' @param post
#' @param overidpost
#' @param pre
#' @param overidpre
#' @param normalize
#'
#' @return
#' @import stringr
#' @export
#'
#' @examples

PreparePlottingData <- function(df_tidy_estimates, policyvar, post, overidpost, pre, overidpre, normalize) {

    largest_lead <- pre + overidpre
    largest_lag <- post + overidpost - 1

    if (normalize < 0) {
        if (normalize == -(largest_lead + 1)) {
            normalization_column <- paste0(policyvar, "_lead", (-1 * (normalize + 1)))
        } else {
            normalization_column <- paste0(policyvar, "_fd_lead", (-1 * normalize))
        }
    } else if (normalize == 0){
        if (normalize == largest_lag + 1) {
            normalization_column <- paste0(policyvar, "_lag", (normalize))
        } else {
            normalization_column <- paste0(policyvar, "_fd")
        }
    } else {
        if (normalize == largest_lag + 1) {
            normalization_column <- paste0(policyvar, "_lag", (normalize))
        } else {
            normalization_column <- paste0(policyvar, "_fd_lag", (normalize))
        }
    }


    first_lead <- paste0(policyvar, "_lead", largest_lead)
    first_lead_integer <- stringr::str_extract(first_lead, "[0-9]+$")
    first_lead_label <- paste0("-", first_lead_integer, "+")

    v_leads <- paste0(policyvar, "_fd_lead", largest_lead:1)
    v_leads_integer <- stringr::str_extract(v_leads, "[0-9]+$")[-1]
    v_leads_label <- paste0("-", v_leads_integer)

    t_0_term <- paste0(policyvar, "_fd")
    t_0_term_label <- "0"

    v_lags <- paste0(policyvar, "_fd_lag", 1:largest_lag)
    v_lags_label <- stringr::str_extract(v_lags, "[0-9]+$")

    last_lag <- paste0(policyvar, "_lag", largest_lag + 1)
    last_lag_integer <- stringr::str_extract(last_lag, "[0-9]+$")
    last_lag_label <- paste0(last_lag_integer, "+")

    v_terms_to_plot_with_normalization <- c(first_lead, v_leads, t_0_term, v_lags, last_lag)
    v_identify_normalization_term <- v_terms_to_plot_with_normalization != normalization_column
    v_terms_to_plot_ordered <- v_terms_to_plot_with_normalization[v_identify_normalization_term]

    v_terms_to_plot_labels <- c(first_lead_label, v_leads_label, t_0_term_label, v_lags_label, last_lag_label)


    v_all_terms <- df_tidy_estimates[["term"]]
    v_plotting_terms <- v_all_terms %in% v_terms_to_plot_ordered

    df_plotting <- df_tidy_estimates[v_plotting_terms, ]
    df_plotting["label"] <- factor(df_plotting$term, levels = v_terms_to_plot_ordered, labels = v_terms_to_plot_labels)

    return(df_plotting)

}
