#' Compute probability function of a truncated distribution
#' @description This function generates the probability function of a truncated specified distribution.
#' Passing the distribution conventional name as argument, it returns the probability function of the truncated specified distribution.
#' @param dist character string indicating distribution name. It can be set as:
#' \itemize{
#'   \item beta Beta Distribution
#'   \item binom Binomial Distribution
#'   \item cauchy Cauchy Distribution
#'   \item chisq Chi-Square Distribution
#'   \item exp Exponential Distribution
#'   \item f F Distribution
#'   \item gamma Gamma Distribution
#'   \item geom Geometric Distribution
#'   \item hyper Hypergeometric Distribution
#'   \item logis Logistic Distribution
#'   \item lnorm Log Normal Distribution
#'   \item nbinom Negative Binomial Distribution
#'   \item norm Normal Distribution
#'   \item pois Poisson Distribution
#'   \item t Student t Distribution
#'   \item unif Uniform Distribution
#'   \item weibull Weibull Distribution
#'   \item wilcox Wilcoxon Rank Sum Statistic Distribution
#'   \item signrank Wilcoxon Signed Rank Statistic Distribution
#' }
#'
#' @return probability function of the truncated specified distribution
#' @export
#'
#' @examples
#' # Normal distribution
#' ptnorm <-ptruncate("norm")
#' ptnorm(q = 1:10, mean = 3, sd = 2, L = 2, U = 6)
#' ptnorm(q = 1:10, mean = 3, sd = 2, L = 2, U = 6, log.p = T)
#' ptnorm(q = 1:10, mean = 3, sd = 2, L = 2, U = 6, log.p = T, lower.tail = F)
ptruncate <- function(dist){
  # generate probability function name of the specified distribution
  pdist <- paste("p", dist, sep = "")
  # get probability function of the specified distribution and its arguments
  pdist <- get(pdist, mode = "function")
  pargs <- formals(pdist)

  # Output function starts here
  probability <- function(){
    # stop if Lower truncation limit is set as greater than Upper truncation limit
    if (L > U) stop("U must be greater than or equal to L")
    # get probability arguments specified
    call <-  as.list(match.call())[-1]
    # intersect probability function and the call (pdist), giving to probability function arguments the values set to the corresponding arguments of the call (pdist)
    pargs <- intersect_args(x = pargs, y = call)
    # log.p and lower.tail arguments of original probability function don't work for truncated output function
    # We set them as NULL, so these options are not considered and not computed by probability original function
    pargs$log.p <- NULL
    pargs$lower.tail <- NULL
    # get quantile inside truncation limits.
    # Quantile/s greater than Upper specification limit is/are replaced with Upper specification limit;
    # Quantile/s lower than Lower specification limit is/are replaced with Lower specification limit;
    pargs$q <-  pmax(pmin(q,U),L)
    # generate pUargs and pLargs equal to pargs, but setting p argument equal to U and L respectively
    pUargs <- pLargs <- pargs
    pUargs$q <- U
    pLargs$q <- L
    # Compute probabilities of the distribution specified corresponding to the quantiles specified and to Upper and Lower truncation limits
    pq <-  do.call("pdist", as.list(pargs))
    pL <-  do.call("pdist", as.list(pLargs))
    pU <-  do.call("pdist", as.list(pUargs))
    # compute probabilities of the truncated distribution specified
    probability <- (pq-pL)/(pU-pL)

    # lower.tail option: if it is specified as FALSE upper tail probabilities have to be computed
    if(!lower.tail){
      probability <- 1 - probability
    }

    # log.p option: if it is specified as TRUE, the probabilities has to be returned as logarithms
    if(log.p){
      probability <- log(probability)
    }

    return(probability)
  }

  # add to probability function formals L (lower truncation limit) and U (upper truncation limit) with values as passed with pdist
  formals(probability) <- c(pargs, eval(substitute(alist(L=-Inf, U=Inf))))
  # return probability function
  return(probability)

}

