#' This function can be used to perform tests of linear hypotheses.
#'
#' @param estimates, A list containing estimation results and model information.
#' Should be an output ofEventStudyOLS or EventStudyIV
#' @param test, The hypothesis to be estimated. Accepts inputs that can be passed to hypothesis.matrix
#' argument in linearHypothesis() function from car package.
#' @param pretrends, String containing name of variable
#' @param leveling_off, String containing name of variable
#'
#' @import car
#' @export
#'
#' @examples
#' TestLinear(Estimates, "z_fd_lag1 = z_fd", pretrends = "z_lead6", leveling_off = "z_lag8")
#'


TestLinear <- function(estimates, test, pretrends = NA, leveling_off = NA){
    if ((class(estimates) != "lm_robust") | (typeof(estimates) != "list")) {
        stop("estimates should be a list with coefficient estimates and standard errors")
    }

    estimates$df.residual <- estimates$nclusters - 1

    test_results <- data.frame(row.names = c("Test", "F.statistic", "p.value"))

    hyp_results <- linearHypothesis(estimates, test, test = "F")

    temp <- data.frame("Test" = 'Test', "F" = hyp_results[2, ]$F, "p.value" = hyp_results[2, ]$`Pr(>F)`)
    test_results <- rbind(test_results, temp)

    if (!is.na(pretrends)){
        hyp <- paste0(pretrends, "=0")
        hyp_results <- linearHypothesis(estimates, hyp, test = "F")

        temp <- data.frame("Test" = "Pre-Trends","F" = hyp_results[2, ]$F, "p.value" = hyp_results[2, ]$`Pr(>F)`)
        test_results <- rbind(test_results, temp)
    }

    if (!is.na(leveling_off)){
        hyp <- paste0(leveling_off, "=0")
        hyp_results <- linearHypothesis(estimates, hyp, test = "F")

        temp <- data.frame("Test" = "Leveling-Off","F" = hyp_results[2, ]$F, "p.value" = hyp_results[2, ]$`Pr(>F)`)
        test_results <- rbind(test_results, temp)
    }

    return(test_results)
}

