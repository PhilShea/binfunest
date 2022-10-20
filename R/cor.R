#' cor.mle returns the correlation matrix of an mle fit.
#'
#' This simple function will return the correlation matrix for `mle` fits of
#' more than one variable.  Note that this function is *not* an S3 generic.
#'
#' @param m an `mle` object with a fit of more than one variable.
#'
#' @return a symmetric matrix with ones on the diagonal.
#'
#' @seealso [stats4::mle()]
#'
#' @export
#'
#' @examples
#' Q_ab <- function( gamma, a, b)
#` Q_(sqrt( a * gamma / (1 + b * gamma)))
#` nobs <- 6 # This is the Mathematica test data.
#` s <- c( sqrt(10), 10, 10 * sqrt(10), 100, 100 * sqrt(10), 1000)
#` r <- c( 246, 211, 220, 23, 9, 6)
#` N <- c( 20000, 500000, 20000000, 20000000, 20000000, 20000000)
#` mle1 <-  mleB2B( Errors=r, N=N, f=Q_ab, fparms=list( gamma=s),
#`               start=c(a=2, b=undB(-11)), method="L-BFGS-B", lower=list(b=0))
#' cor.mle( mle1)
#'
cor.mle <- function( m) {
   stopifnot( isa( m, "mle"))
   c <- stats4::vcov( m)
   v<- diag(1 / sqrt( diag( c)))
   v %*% c %*% v
}
