## ---------------------------------------------------------------------------------------------
#' Simulating an LRE with an exogenous shock
#'
#' @param g Decision rule
#' @param h Motion rule
#' @param x0 vector of predetermined variables that includes the AR parameter
#' @param t length of the simulation
#' @param e vector of exogenous shocks, input is a list which is then transformed into an appropriate matrix
#'
#' @return A matrix showing the dynamic path of variables over the simulation time
#' @export
#'
#' @examples
simulate_shock <- function(g, h, x0, t, e) {
  e <- as.matrix(e, nrow = length(e))
  

  n1 <- length(x0)    
  n2 <- length(g(x0)) 
  pre <- 1:n1
  npr <- (n1 + 1):(n1 + n2)
  
  out <- matrix(0, t, n1 + n2) 
  out[1, pre] <- x0 
  out[1, npr] <- g(x0)
  
  for (i in 1:(t - 1)) {
    if(i <= nrow(e)) {
      out[i + 1, pre] <- h(out[i, pre]) + e[i,]
      out[i + 1, npr] <- g(out[i + 1, pre])
    } else {
      out[i + 1, pre] <- h(out[i, pre])
      out[i + 1, npr] <- g(out[i + 1, pre])
  }}
  out
}

## -------------------------------------------------------------------------------------

#' Simulation for an LRE without exogenous shocks. Takes either the autonomous case or the AR case specified g and h.
#'
#' @param g Decision rule
#' @param h Motion rule
#' @param x0 vector of predetermined variables
#' @param t length of the simulation
#'
#' @return A matrix showing the dynamic paths of the variables in the specification
#' @export
#'
#' @examples
simulate_no_shock <- function(g, h, x0, t) {
  n1 <- length(x0)
  n2 <- length(g(x0))
  
  pre <- 1:n1
  npr <- (n1 + 1):(n1 + n2)
  
  out <- matrix(0, t, n1 + n2)
  
  out[1,pre] <- x0
  out[1, npr] <- g(x0)
  
  for (i in 1:(t-1)) {
    out[i + 1, pre] <- h(out[i, pre])
    out[i + 1, npr] <- g(out[i + 1, pre])
  }
  out
}

## ------------------------------------------------------------------------------------

#' Wrapper function for the simulations.
#'
#' @param g First output of a given lre function
#' @param h Second output of the lre function
#' @param x0 vector of initial predetermined variables. If dealing with an AR process, this needs to include the Phi elements
#' @param t length of the simulation. If not given, assumed to be the length of the exogenous shocks + 1
#' @param e exogenous shocks
#'
#' @return A matrix showing the dynamic paths of the variables in the specification
#' @export
#'
#' @examples
simulate <- function(g, h, x0, t = NULL, e = NULL) {
  if (is.null(t)) t <- length(e) + 1
  if (is.null(e)) {
    return(simulate_no_shock(g, h, x0, t))
  } else {
    return(simulate_shock(g, h, x0, t, e))
  }
}
