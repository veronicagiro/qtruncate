#' Compute quantile function of a truncated distribution
#' @description This function generates the quantile function of a truncated specified distribution.
#' Passing the distribution conventional name as argument, it returns the quantile function of the truncated specified distribution.
#' @param dist character string indicating distribution conventional name. It can be set as:
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
#' @return quantile function of the truncated specified distribution
#' @export
#'
#' @examples
#' # Weibull distribution
#' qtweibull <-qtruncate("weibull")
#' qtweibull(p = 1:9/10, shape = 1, scale = 1.5, L = 3, U = 6)
#' qtweibull(p = 1:9/10, shape = 1, scale = 1.5,  L = 3, U = 6, lower.tail = F, log.p = F)
#' qtweibull(p = log(1:9/10), shape = 1, scale = 1.5, log.p = T, L = 3, U = 6, lower.tail = F)
qtruncate <- function (dist){

    # generate quantile and probability functions name of the specified distribution dist
    qdist <- paste("q", dist, sep = "")
    pdist <- paste("p", dist, sep = "")
    # get quantile function of the specified distribution and its arguments
    qdist <-  get(qdist, mode = "function")
    qargs <-  formals(qdist)
    # get probability function of the specified distribution and its arguments
    pdist <-  get(pdist, mode = "function")
    pargs <- formals(pdist)

    # Output function starts here
    quantile <- function(){
      # stop if Lower truncation limit is set as greater than Upper truncation limit
      if (L > U) stop("U must be greater than or equal to L")
      # get quantile arguments specified
      call <- as.list(match.call())[-1]
      # intersect quantile function and the call (qdist), giving to quantile function arguments the values set to the corresponding arguments of the call (qdist)
      qargs <- intersect_args(x = qargs, y = call)
      # log.p and lower.tail arguments of original quantile function don't work for truncated output function
      # We set them as NULL, so these options are not considered and not computed by quantile original function
      qargs$log.p <- NULL
      qargs$lower.tail <- NULL
      # generation of probability function arguments
      pargs <- c(pargs[!is.element(names(pargs), names(call))], call[is.element(names(call), names(pargs))])
      # intersect probability function and the call (qdist), giving to probability function arguments the values set to the corresponding arguments of the call (qdist)
      pargs <- intersect_args(x = pargs, y = call)
      # log.p and lower.tail arguments of original probability function don't work for truncated output function
      # We set them as NULL so these options are not computed by probability original function
      pargs$log.p <- NULL
      pargs$lower.tail <- NULL
      # generate pUargs and pLargs equal to pargs, but setting p argument equal to U and L respectively
      pUargs <- pLargs <- pargs
      pUargs$q <- U
      pLargs$q <- L
      # control if Upper and Lower limits are set inside function support
      if(do.call("pdist", as.list(pUargs)) == 0) stop("U below lower support limit")
      if(do.call("pdist", as.list(pLargs)) == 1) stop("L above upper support limit")

      # log.p option: if it is specified as TRUE, the values passed as p have to be interpreted as logarithms
      if(log.p){
        p <- exp(p)
      }

      # lower.tail option: if it is specified as FALSE upper tail probabilities have to be computed
      if(!lower.tail){
        p <- 1 - p
      }

      # computation of probabilities for truncated distribution
      qargs$p <- do.call("pdist", as.list(pLargs)) + p * (do.call("pdist", as.list(pUargs)) - do.call("pdist", as.list(pLargs)))
      qp <- do.call("qdist", as.list(qargs))
      quantile <- pmin(pmax(L, qp), U)

      return(quantile)
    }

    # add to quantile function formals L (lower truncation limit) and U (upper truncation limit) with values as passed with qdist
    formals(quantile) <-  c(formals(qdist), eval(substitute(alist(L=-Inf, U=Inf))))
    # return quantile function
    return(quantile)
}

