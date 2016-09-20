#' Generate random number from a truncate distribution
#'
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
#' @return random number generation function for the truncated specified distribution
#' @export
#'
#' @examples
#' # t distribution
#' rtt <- rtruncate("t")
#' rtt(n = 10, df = 4, L = 2, U = 5)
rtruncate <- function(dist){

  # generate random number generator function name of the specified distribution dist
  rdist <- paste("r", dist, sep = "")
  # get random number generator function of the specified distribution and its arguments
  rdist <-  get(rdist, mode = "function")
  rargs <-  formals(rdist)

  # generate quantile function name of the specified distribution dist
  qdist <- paste("q", dist, sep = "")
  # get quantile function of the specified distribution and its arguments
  qdist <-  get(qdist, mode = "function")
  qargs <-  formals(qdist)

  # generate probability function name of the specified distribution dist
  pdist <- paste("p", dist, sep = "")
  # get proability function of the specified distribution and its arguments
  pdist <- get(pdist, mode = "function")
  pargs <- formals(pdist)

  # Output function starts here
  random <- function(...){
    # stop if Lower truncation limit is set as greater than Upper truncation limit
    if (L > U) stop("U must be greater than or equal to L")
    # get probability arguments specified
    call <- as.list(match.call())[-1]
    # intersect probability function and the call (rdist), giving to probability function arguments the values set to the corresponding arguments of the call (rdist)
    pargs <- intersect_args(x = pargs, y = call)
    # intersect quantile function and the call (rdist), giving to random function arguments the values set to the corresponding arguments of the call (rdist)
    qargs <- intersect_args(x = qargs, y = call)
    # intersect random generator function and the call (rdist), giving to probability function arguments the values set to the corresponding arguments of the call (rdist)
    rargs <- intersect_args(x = rargs, y = call)
    # generate pUargs and pLargs equal to pargs, but setting p argument equal to U and L respectively
    pUargs <- pLargs <- pargs
    pUargs$q <- U
    pLargs$q <- L
    # Compute probabilities of the distribution specified corresponding to the Upper and Lower truncation limits
    pU <- do.call(pdist, as.list(pUargs))
    pL <- do.call(pdist, as.list(pLargs))
    # Compute probabilities for quantile function by an Uniform distribution with pL and pU support limits
    qargs$p <- runif(n, min = pL, max = pU)
    # Compute random numbers using quantile functions on probabilities previously computed
    random <- do.call(qdist, as.list(qargs))

    return(random)

  }
  # add to random number generator function formals L (lower truncation limit) and U (upper truncation limit) with values as passed with ddist
  formals(random) <-  c(formals(rdist), eval(substitute(alist(L=-Inf, U=Inf))))
  # return random number generator function
  return(random)
}



