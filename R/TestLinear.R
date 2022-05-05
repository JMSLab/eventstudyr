#' This function can be used to perform tests of linear hypotheses.
#'
#' @param estimates, A list of length 2 containing estimation results and model information.
#' Should be an output of EventStudyOLS or EventStudyIV.
#' @param test, The hypothesis to be estimated. Accepts inputs that can be passed to hypothesis.matrix
#' argument in linearHypothesis() function from car package.
#' @param pretrends, If TRUE, uses pre and overidpre from estimates to test for pre-trends. Defaults to TRUE.
#' @param leveling_off, If TRUE, uses post and overidpost from estimates to test for leveling-off. Defaults to TRUE.
#'
#' @import car
#' @export
#'
#' @examples
#' TestLinear(Estimates, test = "z_fd_lag1 = z_fd", pretrends = T, leveling_off = T)
#'


TestLinear <- function(estimates, test = NA, pretrends = T, leveling_off = T){
    if (class(estimates) != "list" | length(estimates) != 2){
        stop("estimates should be a list of length two, an output of EventStudy()")}
    if ((class(estimates[[1]]) != "lm_robust") | (typeof(estimates[[1]]) != "list")) {
        stop("The first element of estimates should be a list of class 'lm_robust' with coefficient estimates and standard errors")
    }
    if (class(estimates[[2]]) != "list" | typeof(estimates[[2]]) != "list") {
        stop("The second element of estimates should be a list with argument definitions, an output of EventStudy().")
    }
    if (class(pretrends) != "logical"){stop("pretrends should be a logical. Default value is false.")}
    if (class(leveling_off) != "logical"){stop("leveling_off should be a logical. Default value is false.")}

    estimates[[1]]$df.residual <- estimates[[1]]$nclusters - 1

    test_results <- data.frame(row.names = c("Test", "F.statistic", "p.value"))

    if (!is.na(test)){
        user_results <- car::linearHypothesis(estimates[[1]], test, test = "F")

        temp <- data.frame("Test"    = "UserTest",
                           "F"       = user_results[2, ]$F,
                           "p.value" = user_results[2, ]$`Pr(>F)`)
        test_results <- rbind(test_results, temp)
    }

    if (pretrends == T){
        n_furthest_lead <- estimates[[2]]$pre + estimates[[2]]$overidpre
        furthest_lead    <- paste0(estimates[[2]]$policyvar, "_lead",as.character(n_furthest_lead))
        pretrends_hyp <- paste0(furthest_lead, "=0")

        pretrends_results   <- car::linearHypothesis(estimates[[1]], pretrends_hyp, test = "F")

        temp <- data.frame("Test"        = "Pre-Trends",
                           "F"           = pretrends_results[2, ]$F,
                           "p.value"     = pretrends_results[2, ]$`Pr(>F)`)
        test_results <- rbind(test_results, temp)
    }

    if (leveling_off == T){
        n_furthest_lag <- estimates[[2]]$post + estimates[[2]]$overidpost
        furthest_lag    <- paste0(estimates[[2]]$policyvar, "_lag", as.character(n_furthest_lag))
        leveling_off_hyp <- paste0(furthest_lag, "=0")

        leveling_results <- car::linearHypothesis(estimates[[1]], leveling_off_hyp, test = "F")

        temp <- data.frame("Test"        = "Leveling-Off",
                           "F"           = leveling_results[2, ]$F,
                           "p.value"     = leveling_results[2, ]$`Pr(>F)`)
        test_results <- rbind(test_results, temp)
    }

    return(test_results)
}

