#' This function can be used to perform tests of linear hypotheses.
#' # I believe the commas should be removed due to convention and consistency with other functions - MZW
#' @param estimates, A list of length 2 containing estimation results and model information.
#' Should be an output of EventStudy().
#' @param test, The hypothesis to be estimated. Accepts inputs that can be passed to
#' hypothesis.matrix argument in linearHypothesis() function from car package.
#' @param test_name, Name for test input by user. Defaults to "User Test."
#' @param pretrends, If TRUE, uses pre and overidpre from estimates to test for pre-trends.
#' Defaults to TRUE.
#' @param leveling_off, If TRUE, uses post and overidpost from estimates to test for leveling-off.
#' Defaults to TRUE.
#'
#' @importFrom car linearHypothesis
#' @export
#'
#' @examples
#' estimates <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
#'                         policyvar = "z", idvar = "id", timevar = "t",
#'                         controls = "x_r", FE = TRUE, TFE = TRUE,
#'                         post = 3, pre = 2, overidpre = 4, overidpost = 5,
#'                         normalize = - 3, cluster = TRUE, anticipation_effects_normalization = TRUE)
#'
#' TestLinear(estimates, test = "z_fd_lag1 = z_fd", test_name = "Hypothesis Test",
#'            pretrends = TRUE, leveling_off = TRUE)
#'


TestLinear <- function(estimates, test = NA, test_name = "User Test", pretrends = TRUE, leveling_off = TRUE){
    if (! is.list(estimates) | length(estimates) != 2){
        stop("estimates should be a list of length two, an output of EventStudy()")}
    if ((! class(estimates[[1]]) %in% c("lm_robust", "iv_robust")) | ! is.list(estimates[[1]])) {
        stop("The first element of estimates should be a list of class 'lm_robust' with coefficient estimates and standard errors")
    }
    if (! is.list(estimates[[2]]) | ! is.list(estimates[[2]])) {
        stop("The second element of estimates should be a list with argument definitions, an output of EventStudy().")
    }
    if (! is.character(test_name)) {stop("test_name should be of class character. Defaults to 'User Test'.")}
    if (! is.logical(pretrends)) {stop("pretrends should be a logical. Default value is TRUE")}
    if (! is.logical(leveling_off)) {stop("leveling_off should be a logical. Default value is TRUE")}

    if(estimates[[2]]$cluster == TRUE){

        estimates[[1]]$df.residual <- estimates[[1]]$nclusters - 1

    }

    coefficients <- estimates[[2]]$eventstudy_coefficients

    if (!is.null(estimates[[2]]$proxyIV)){
        coefficients <- coefficients[coefficients != estimates[[2]]$proxyIV]
    }

    test_results <- data.frame(row.names = c("Test", "F.statistic", "p.value"))

    if (!is.na(test)){
        user_results <- car::linearHypothesis(estimates[[1]], test, test = "F")

        temp <- data.frame("Test"    = test_name,
                           "F"       = user_results[2, ]$F,
                           "p.value" = user_results[2, ]$`Pr(>F)`)
        test_results <- rbind(test_results, temp)
    }

    if (pretrends == TRUE){

        G   <- estimates[[2]]$pre
        L_G <- estimates[[2]]$overidpre
        k   <- as.character(seq.int(G+1, (G+L_G)))

        suffix <- paste0("_lead",k)

        suffix_len  <- str_length(suffix)[1]

        delta_k <- coefficients[str_sub(coefficients, start= -suffix_len) %in% suffix]

        pretrends_hyp <- paste0(delta_k, "=0")

        pretrends_results <- car::linearHypothesis(estimates[[1]], pretrends_hyp, test = "F")

        temp <- data.frame("Test"        = "Pre-Trends",
                           "F"           = pretrends_results[2, ]$F,
                           "p.value"     = pretrends_results[2, ]$`Pr(>F)`)

        test_results <- rbind(test_results, temp)
    }

    if (leveling_off == TRUE){

        M   <- estimates[[2]]$post
        L_M <- estimates[[2]]$overidpost
        k   <- as.character(seq.int(M+1, M+L_M))

        suffix_M  <- paste0("_lag",as.character(M))
        suffix_Mk <- paste0("_lag",k)

        suffix_M_len  <- str_length(suffix_M)
        suffix_Mk_len <- str_length(suffix_Mk)[1]

        delta_M   <- coefficients[str_sub(coefficients, start= -suffix_M_len) %in% suffix_M]
        delta_Mk  <- coefficients[str_sub(coefficients, start= -suffix_Mk_len) %in% suffix_Mk]

        leveling_off_hyp <- paste0(delta_Mk, "=", delta_M)

        leveling_results <- car::linearHypothesis(estimates[[1]], leveling_off_hyp, test = "F")

        temp <- data.frame("Test"        = "Leveling-Off",
                           "F"           = leveling_results[2, ]$F,
                           "p.value"     = leveling_results[2, ]$`Pr(>F)`)

        test_results <- rbind(test_results, temp)
    }

    return(test_results)
}

