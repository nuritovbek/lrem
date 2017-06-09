
#' Diagonal bind for a matrix, source: Patrick Blisle, http://www.medicine.mcgill.ca/epidemiology/joseph/pbelisle/dbind.html
#'
#' @param m Upper left matrix(or element)
#' @param a Bottom right matrix
#' @param rev reverces the order if necessary
#'
#' @return A new matrix with the two inputs in its block diagonal.
#' @export
#'
#' @examples
dbind <- function(m, a, rev=F)
{
  
  matrix.index <- function(m, row, col) {(col-1)*nrow(m)+row}
  
  if (!is.matrix(m)) m <- matrix(m, 1, 1)
  if (!is.matrix(a)) a <- matrix(a, 1, 1)
  
  m.dim <- dim(m)
  a.dim <- dim(a)
  
  if (all(m.dim==0))
  {
    m <- a
  }
  else if (any(a.dim>0))
  {
    new.dim <- m.dim + a.dim
    
    new <- matrix(0, nrow=new.dim[1], ncol=new.dim[2])
    j <- as.vector(matrix.index(new, row(m), col(m)))
    new[j] <- m
    j <- as.vector(matrix.index(new, m.dim[1] + row(a), m.dim[2] + col(a)))
    new[j] <- a
    m <- new
  }
  
  m
} # end of dbind

