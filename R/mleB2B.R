#' mleB2B  Estimates Back-to-Back "Q" and Offsets to a bit error rate function.
#'
#' Bit error counts modeled as independent binary decisions result in a
#' log-likelihood dependent on the bit error probability.  This function
#' inserts the supplied bit error probability function into the binomial
#' log-likelihood function, and passes that to [stats4::mle], which ultimately
#' calls [stats::optim].  The function will optimize a binomial probability
#' of the form $r = N * P( x_1, x_2, ..., x_n, a_1, a_2, ... a_k)$, where the
#' $x_i$ are variables from `data`, and the $a_j$ are parameters to be
#' estimated.
#'
#' The function estimates the parameters identified in `start` in the
#' constructed call to `f`. For a function `f` of the form
#' `fun( SNR, x2, x3, B2B, offset)`
#' A call of the form
#'
#' \code{
#' mleB2B( data=df, Errors="r", N="trials", f=fun,
#'         fparm=list( SNR="s", x2=1, x3="noise"), start=list(B2B=1, offset=2))
#' }
#'
#'  will construct a call to `mle` of the form:
#'
#' \code{
#' mle( minuslogl=ll, start=start, nobs=length( Errors), method=method)}
#'
#' where the function `ll` is defined as
#'
#' \code{
#'   ll <- function( a, b) -sum( dbinom( df$r, df$n,
#'                 fun( SNR=df$s, x2=1, x3=df$noise, B2B=B2B, offset=offset),
#'                 log=TRUE))
#' }
#'
#' @param data a data frame or list with named components. If a list, each
#' component must be the same length (just like a data frame). This is not
#' checked, so usual rules of recycling will apply.  Partial matching not
#' performed, so you must use full column names.
#' @param Errors A vector of error counts, or a string identifying a column of
#'     `data` from which to draw the error counts
#' @param N A single number, or a vector of the same length as
#'    `data`, or a string identifying a column of `data` specifying the number
#'    of trials used to measure the error counts
#'    in `Errors`.  If a single number, then that number is used as the number
#'    of trials for all error counts.
#' @param f A function that predicts the probability of errors.
#' @param fparms a list of named components that are the arguments of `f`.
#'     Each component can be a string, a single number, or a vector. If a
#'     string that names a column of `data`, that column will be used,
#'     otherwise the string will be passed to `f`.  Note the potential for
#'     errors if a column name was misspelled. A single number or vector
#'     will be passed to `f`. Between `fparms`, `start`, and function defaults,
#'     all parameters that need to be supplied to `f` should be specified, and
#'     (except for defaults) not duplicated.
#' @param start Named list of initial values for the parameters of `f` to be
#'     estimated.
#' @param method Optimization method.  See [stats::optim()].
#' @param ... Optional arguments to be passed to [mle].
#'
#' @return An object of class [stats4::mle-class] with the parameters
#' identified in `start` estimated.
#'
#' @seealso [stats4::mle()], [stats::optim()]
#' @export
#'
#' @examples
#' QPSKdB.B2B <- B2BConvert( QPSKdB)
#' O1 <- 3
#' B1 <- 16
#' s <- 0:20
#' N <- 1000000
#' r <- rbinom( length( s), N, QPSKdB.B2B( s, B1, O1))
#' df <- data.frame( Errors=r, SNR=s, N=N)
#' llsb2 <- function( b2b, offset)
#'          -sum( dbinom( r, N, QPSKdB.B2B( s, b2b, offset), log=TRUE))
#' mle1 <- stats4::mle( llsb2, start=c( b2b=20, offset=0), nobs=length(s),
#'                      method="Nelder-Mead")
#' est1 <-  mleB2B( data=df, Errors="Errors", N=N, f=QPSKdB.B2B,
#'                  fparms=list( x="SNR"), start=c(b2b=20, offset=0))
#'
mleB2B <- function( data=NULL, Errors, N, f, fparms, start,
                    method="Nelder-Mead", ...) { #, lower, upper, control, ...) {
   cll <- match.call( expand.dots=FALSE)
   if (!is.null( data)) { # a data frame has been supplied.
      dnames <- names( data)
      # first, we need a helper function
      helper <- function( x, strict=FALSE)
         # if in dnames it will pull from data.
         if (is.character( x))
             if (x %in% dnames)  data[[ x]] else
                if (strict) stop( paste( x, "is not in data")) else x
         else x # if (is.character( x))
      Errors <- helper( Errors, strict=TRUE)
      N <- helper( N, strict=TRUE)
      fparms <- lapply( fparms, helper) # expand each item in fparms.
   }
   # start has the names of variables of f to optimize.
   opt_names <- lapply( names( start), as.name)
   # create the call to f
   fun.txt <- c( quote( f), fparms, opt_names)
   fun <- as.call( fun.txt)
   le <- length( Errors)
   if (length( N) == 1) N <- rep( N, le) # normalize N to length of Errors
   ll <- function() -sum( stats::dbinom( Errors, N, eval(fun), log=TRUE))
   formals( ll) <- start # must use alist to make this work.
   # constructing the call to mle.
   sublst <- list( minuslogl=ll, start=start, nobs=le, method=method)
   if (!is.null( cll$...)) sublst <- append( sublst, cll$...)
   #if ( !missing( lower) || !missing( upper))
   #   sublst <- append( sublst, alist(lower=lower, upper=upper))
   #if ( !missing( control)) sublst <- append( sublst, alist(control=control))
   # This idiom will allow the call to be retained my mle.
   call <- as.call( c( quote( stats4::mle), sublst))
   eval( call)
}
