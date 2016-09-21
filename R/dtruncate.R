#' Compute density function of a truncated distribution
#' @description This function generates the density function of a truncated specified distribution.
#' Passing the distribution conventional name as argument, it returns the density function of the truncated specified distribution.
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
#' @return density function of the truncated specified distribution
#' @export
#'
#' @examples
#' # Normal distribution
#' dtnorm <-dtruncate("norm")
#' dtnorm(x = 1:10, mean = 3, sd = 2, L = 2, U = 6)
#' dtnorm(x = 1:10, mean = 3, sd = 2, L = 2, U = 6, log = T)
dtruncate <-
  function (dist){
    # generate density function name of the specified distribution
    ddist <- paste("d", dist, sep = "")
    # generate probability function name of the specified distribution
    pdist <- paste("p", dist, sep = "")

    # get density function and its arguments
    ddist <-  get(ddist, mode = "function")
    dargs <- formals(ddist)

    # get probability function and its arguments
    pdist <- get(pdist, mode = "function")
    pargs <- formals(pdist)

    #Output function starts here
    density <- function(){
      # stop if Lower truncation limit is set as greater than Upper truncation limit
      if (L > U) stop("U must be greater than or equal to L")
      # get density arguments specified
      call <- as.list(match.call())[-1]
      # intersect density function and the call (ddist), giving to density function arguments the values set to the corresponding arguments of the call (ddist)
      dargs <- intersect_args(x = dargs, y = call)
      # log.p argument of original density function don't work for truncated output function
      # We set them as NULL, so this option is not considered and not computed by density original function
      dargs$log <- NULL  # comment if you choose to use log diff for compute density

      # intersect probability function and the call (ddist), giving to probability function arguments the values set to the corresponding arguments of the call (ddist)
      pargs <- intersect_args(x = pargs, y = call)
      pargs$log <- NULL

      #select x only where inside truncation limits
      dargs$x <- x[x > L & x <= U]

      # generate pUargs and pLargs equal to pargs, but setting p argument equal to U and L respectively
      pUargs <-  pLargs <- pargs
      pUargs$q <- U
      pLargs$q <- L

      #initialize output
      density <- numeric(length(x))

      # Compute density of the distribution specified corresponding to the quantiles specified and the probabilities specified equal to Upper and Lower truncation limits
      dx <- do.call("ddist", as.list(dargs))
      pU <- do.call("pdist", as.list(pUargs))
      pL <- do.call("pdist", as.list(pLargs))

      # compute densities of the truncated distribution specified
      density[x > L & x <= U] <-  dx /(pU - pL)


      # density[x > L & x <= U] <-if(log){
      #    dx - log(pU - pL)
      # }else{
      #       dx /(pU - pL)
      # }

      # log option: if it is specified as TRUE, the densities has to be returned as logarithms
       if(log){
         density[x > L & x <= U] <- log(density[x > L & x <= U])
       }

      #returns density values for truncated distributions
      return(density)

    }

    # add to density function formals L (lower truncation limit) and U (upper truncation limit) with values as passed with ddist
    formals(density) <-  c(formals(ddist), eval(substitute(alist(L=-Inf, U=Inf))))
    # return density function
    return(density)
  }
