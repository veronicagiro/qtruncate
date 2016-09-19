rtruncate <-
  function (dist){
    
  
  #dist <- deparse(substitute(dist))
  
  rdist <- paste("r", dist, sep = "") 
  rdist <-  get(rdist, mode = "function")
  rargs <-  formals(rdist)
  
  
  qdist <- paste("q", dist, sep = "") 
  qdist <-  get(qdist, mode = "function")
  qargs <-  formals(qdist)
  
  pdist <- paste("p", dist, sep = "") 
  pdist <-  get(pdist, mode = "function")
  pargs <-  formals(pdist)
  
  
  random <-  function (...) 
  {
    if (L > U) stop("U must be greater than or equal to L")
    
    call <- as.list(match.call())[-1]
    #pargs <-  c(pargs[!is.element(names(pargs), names(call))], call[is.element(names(call), names(pargs))])
    pargs <- intersect_args(x = pargs, y = call)
    
    #qargs <- c(qargs[!is.element(names(qargs), names(call))], call[is.element(names(call), names(qargs))])
    qargs <- intersect_args(x = qargs, y = call)
    
    #rargs <- c(rargs[!is.element(names(rargs), names(call))], call[is.element(names(call), names(rargs))])
    rargs <- intersect_args(x = rargs, y = call)
    
    pUargs <- pLargs <- pargs 
    pUargs$q <- U
    pLargs$q <- L
    
    pU <- do.call(pdist, as.list(pUargs))
    pL <- do.call(pdist, as.list(pLargs))
    
    qargs$p <- runif(n, min = pL, max = pU)
    
    random <- do.call(qdist, as.list(qargs))

    return(random)
    
  }
  
  formals(random) <-  c(formals(rdist), eval(substitute(alist(L=-Inf, U=Inf))))
  return(random)
}



