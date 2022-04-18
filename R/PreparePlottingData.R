

PreparePlottingData <- function(data) {

    df_tidy <- estimatr::tidy(data)

    v_terms_to_plot <- stringr::str_subset(df_tidy[, "term"], "fd|lag|lead")

    df_order <- data.frame(
        term = v_terms_to_plot,
        order = stringr::str_extract(v_terms_to_plot, "\\d$"),
        no_fd = ifelse(stringr::str_detect(v_terms_to_plot, "fd", negate = TRUE), 1, 0),
        lead_or_lag = stringr::str_extract(v_terms_to_plot, "lead|lag")
    )

    first_lead_logic <- df_order["no_fd"] == 1 & df_order["lead_or_lag"] == "lead"
    first_lead <- df_order[first_lead_logic, "term"]
    t_0_term <- stringr::str_subset(v_terms_to_plot, "fd$")

    last_lag_logic <- df_order["no_fd"] == 1 & df_order["lead_or_lag"] == "lag"
    last_lag <- df_order[last_lag_logic, "term"]

    df_leads <- df_order[df_order["no_fd"] == 0 & df_order["lead_or_lag"] == "lead", ]
    df_leads <- df_leads[!is.na(df_leads["no_fd"]), ]
    df_leads <- df_leads[order(df_leads$order, decreasing = TRUE), ]

    v_leads <- df_leads$term

    df_lags <- df_order[df_order["no_fd"] == 0 & df_order["lead_or_lag"] == "lag", ]
    df_lags <- df_lags[!is.na(df_lags["no_fd"]), ]
    df_lags <- df_lags[order(df_lags$order, decreasing = FALSE), ]

    v_lags <- df_lags$term

    v_terms_to_plot_order <- c(first_lead, v_leads, t_0_term, v_lags, last_lag)

    df_plotting <- df_tidy[df_tidy$term %in% v_terms_to_plot, ]

    df_plotting["term"] <- factor(df_plotting$term, levels = v_terms_to_plot_order)

    return(df_plotting)

}
