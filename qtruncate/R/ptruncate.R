ptruncate <-
  function (dist){
    
    #if(!is.character(dist)) dist <- as.character(substitute(dist))
    #envir <- as.environment(paste("pacqqnkage", as.character(substitute(pkg)), sep = ":"))
    
    pdist=paste("p", dist, sep = "")
    pdist <-  get(pdist, mode = "function")
    pargs <-  formals(pdist)
    
    probability <- function () 
    {
      if (L > U) stop("U must be greater than or equal to L")
      
    call <-  as.list(match.call())[-1]
    
    pargs <- intersect_args(x = pargs, y = call)
    #pargs[intersect(names(pargs), names(call))]  <-  call[intersect(names(pargs), names(call))]
    #pargs <- c(pargs[!is.element(names(pargs), names(call))], call[is.element(names(call), names(pargs))])
      
     pargs$q <-  pmax(pmin(q,U),L)
     pUargs <- pLargs <-  pargs 
     pUargs$q <- U
     pLargs$q <- L
     
     pq <-  do.call("pdist", as.list(pargs))
     pL <-  do.call("pdist", as.list(pLargs))
     pU <-  do.call("pdist", as.list(pUargs))
      
     probability <- (pq-pL)/(pU-pL)
     
      #probability <-  (do.call("pdist", as.list(pargs)) - do.call("pdist", as.list(pLargs)))/(do.call("pdist", as.list(pUargs))-do.call("pdist", as.list(pLargs)))   
        
      return(probability)
    }
    
    formals(probability) <- c(pargs, eval(substitute(alist(L=-Inf, U=Inf)))) #formals(pdist)
    return(probability)
  
  }

