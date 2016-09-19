qtruncate <-
  function (dist){
      
    #dist <- deparse(substitute(dist))
    
    qdist=paste("q", dist, sep = "") 
    pdist=paste("p", dist, sep = "")
    
    qdist <-  get(qdist, mode = "function")
    qargs <-  formals(qdist)
    
    pdist <-  get(pdist, mode = "function")
    pargs <- formals(pdist)
    
    quantile <- function () 
    {
      if (L > U) stop("U must be greater than or equal to L")
      
      call <- as.list(match.call())[-1]
      #qargs <- c(qargs[!is.element(names(qargs), names(call))], call[is.element(names(call), names(qargs))])
      qargs <- intersect_args(x = qargs, y = call)
      
      
      pargs <- c(pargs[!is.element(names(pargs), names(call))], call[is.element(names(call), names(pargs))])
      pargs <- intersect_args(x = pargs, y = call)
      
      pUargs <- pLargs <- pargs 
      pUargs$q <- U
      pLargs$q <- L
      
      if ( do.call("pdist", as.list(pUargs)) == 0) stop("U below lower support limit") 
      if ( do.call("pdist", as.list(pLargs)) == 1) stop("L above upper support limit")
      
      qargs$p <- do.call("pdist", as.list(pLargs)) + p * (do.call("pdist", as.list(pUargs)) - do.call("pdist", as.list(pLargs))) 
      
      qp <- do.call("qdist", as.list(qargs))
      #quantile <- pmin(pmax(L,do.call("qdist", as.list(qargs))),U)
      quantile <- pmin(pmax(L, qp ), U)
      
      return(quantile)
      
    }
    
    formals(quantile) <-  c(formals(qdist), eval(substitute(alist(L=-Inf, U=Inf))))
    
    return(quantile)
    
  }

