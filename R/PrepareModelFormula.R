
#' Title
#'
#' @param estimator
#' @param data
#' @param outcomevar
#' @param policyvar
#' @param idvar
#' @param timevar
#' @param controls
#' @param proxy
#' @param proxyIV
#' @param FE
#' @param TFE
#' @param M
#' @param LM
#' @param G
#' @param LG
#' @param normalize
#' @param cluster
#'
#' @return
#' @export
#'
#' @examples


PrepareModelFormula <- function(estimator, data, outcomevar, policyvar, idvar, timevar, controls = NULL,
                                proxy = NULL, proxyIV = NULL, FE = TRUE, TFE = TRUE, M, LM = 1, G, LG = 1,
                                normalize = -1, cluster = TRUE) {

    if (! estimator %in% c("OLS", "FHS")) {stop("estimator should be either 'OLS' or 'FHS'.")}
    if (! is.character(outcomevar)) {stop("outcomevar should be a character.")}
    if (! is.character(policyvar)) {stop("policyvar should be a character.")}
    if (! is.character(idvar)) {stop("idvar should be a character.")}
    if (! is.character(timevar)) {stop("timevar should be a character.")}
    if (! (is.null(controls) | is.character(controls))) {stop("controls should be either NULL or a character.")}
    if ((estimator == "OLS" & ! is.null(proxy))) {stop("proxy should only be specified when estimator = 'FHS'.")}
    if ((estimator == "FHS" & ! is.character(proxy))) {stop("proxy should be a character.")}
    if ((estimator == "OLS" & ! is.null(proxyIV))) {stop("proxyIV should only be specified when estimator = 'FHS'.")}
    if ((estimator == "FHS" & ! is.character(proxyIV))) {stop("proxyIV should be a character.")}
    if (! is.logical(FE)) {stop("FE should be TRUE or FALSE.")}
    if (! is.logical(TFE)) {stop("TFE should be TRUE or FALSE.")}
    if (! (is.numeric(M) & M > 0)) {stop("M should be a positive integer.")}
    # should LM be > 0?
    if (! (is.numeric(LM) & LM > 0)) {stop("LM should be a positive integer.")}
    # should G be > 0?
    if (! (is.numeric(G) & G > 0)) {stop("G should be a positive integer.")}
    # should LG be > 0?
    if (! (is.numeric(LG) & LG > 0)) {stop("LG should be a positive integer.")}
    if (LG < G) {stop("LG should be greater than G")}
    if (!is.numeric(normalize)) {stop("normalize should be numeric.")}
    if (! is.logical(cluster)) {stop("cluster should be TRUE or FALSE")}



}
