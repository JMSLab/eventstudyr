
#' Runs OLS with optional fixed effects and clustering
#'
#' @param prepared_model_formula A formula object created in PrepareModelForumla that is passed to EventStudy.
#' @param prepared_data data frame containing all of the parameters required for EventStudy() plus leads and
#' lags of the first differenced policy variable and leads and lags of the policy variable.
#' @param idvar Variable indicating units, should be a character.
#' @param timevar Variable indicating time periods, should be a character.
#' @param FE Specifies if unit fixed-effects should be included. Defaults to TRUE.
#' @param TFE Specifies if time fixed-effects should be included. Defaults to TRUE.
#' @param cluster Specifies whether to use clustered errors by units. If FALSE, will use unclustered
#' heteroskedasticity-robust standard errors. Defaults to TRUE.
#'
#' @return
#' @export
#'
#' @examples

EventStudyOLS <- function(prepared_model_formula, prepared_data, idvar, timevar, FE, TFE, cluster) {

    if (FE & TFE & cluster) {

        estimatr::lm_robust(
            formula = prepared_model_formula,
            data = prepared_data,
            fixed_effects = ~ get(idvar) + get(timevar),
            clusters = get(idvar),
            se_type = "stata"
        )

    } else if ((!FE) & TFE & cluster) {

        estimatr::lm_robust(
            formula = prepared_model_formula,
            data = prepared_data,
            fixed_effects = ~ get(timevar),
            clusters = get(idvar),
            se_type = "stata"
        )


    } else if (FE & (!TFE) & cluster) {

        estimatr::lm_robust(
            formula = prepared_model_formula,
            data = prepared_data,
            fixed_effects = ~ get(idvar),
            clusters = get(idvar),
            se_type = "stata"
        )


    } else if ((!FE) & (!TFE) & cluster) {

        estimatr::lm_robust(
            formula = prepared_model_formula,
            data = prepared_data,
            clusters = get(idvar),
            se_type = "stata"
        )


    } else if (FE & TFE & (!cluster)) {

        estimatr::lm_robust(
            formula = prepared_model_formula,
            data = prepared_data,
            fixed_effects = ~ get(idvar) + get(timevar),
            se_type = "stata"
        )


    } else if ((!FE) & TFE & (!cluster)) {

        estimatr::lm_robust(
            formula = prepared_model_formula,
            data = prepared_data,
            fixed_effects = ~ get(timevar),
            se_type = "stata"
        )


    } else if (FE & (!TFE) & (!cluster)) {

        estimatr::lm_robust(
            formula = prepared_model_formula,
            data = prepared_data,
            fixed_effects = ~ get(idvar),
            se_type = "stata"
        )


    } else if ((!FE) & (!TFE) & (!cluster)) {

        estimatr::lm_robust(
            formula = prepared_model_formula,
            data = prepared_data,
            se_type = "stata"
        )


    }



}
