dtruncate <-
  function (dist){

    ddist=paste("d", dist, sep = "")
    pdist=paste("p", dist, sep = "")

    #gets density function
    ddist <-  get(ddist, mode = "function")
    #gets argument of density function
    dargs <- formals(ddist)


    #gets probability function
    pdist <- get(pdist, mode = "function")
    #gets argument of probability function
    pargs <- formals(pdist)

    #Output function starts here
    density <- function ()
    {
      #check L U
      if (L > U) stop("U must be greater than or equal to L")

      #gets density arguments

      call <- as.list(match.call())[-1]

      dargs <- intersect_args(x = dargs, y = call)
      dargs$log <- NULL

      #all unique arguments belonging to probability and pdist
      pargs <- intersect_args(x = pargs, y = call)
      pargs$log <- NULL

      #select x only where defined by L and U
      dargs$x <- x[x > L & x <= U]

      #define arguments for pdist in L and U
      pUargs <-  pLargs <- pargs
      pUargs$q <- U
      pLargs$q <- L

      #initialize output
      density <- numeric(length(x))

      #this is standard method for computing density values for truncated distributions

      dx <- do.call("ddist", as.list(dargs))
      pU <- do.call("pdist", as.list(pUargs))
      pL <- do.call("pdist", as.list(pLargs))

      density[x > L & x <= U] <-  dx /(pU - pL)

      #returns density values for truncated distributions

      if(log){
        density <- log(density)
      }

      return(density)

    }

    #add to density function formals L and U with values as passed with dtruncate
    formals(density) <-  c(formals(ddist), eval(substitute(alist(L=-Inf, U=Inf))))
    #return density function
    return(density)
  }
