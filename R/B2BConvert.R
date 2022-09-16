#' B2BConvert Converts a function of SNR into one of SNR, B2B, and Offset.
#'
#' Creates a function `f( -dB( undB( -s) + undB( -B2B)) - offset)`
#'
#' Note that all quantities are assumed to be in Decibels.
#'
#' @param f A function of a single argument `f( s)`. `f` can be a symbol, a
#'     string, or an expression.
#'
#' @return A function of three arguments `f( s, B2B, offset)`.
#' @export
#'
#' @examples
#' QPSKdB.B2B <- B2BConvert( QPSKdB)
#'
B2BConvert <- function( f) {
   fs <- substitute(f) # Try to retain original function name
   if( is.character( fs)) fs <- match.fun( fs)
   removeSource( eval( bquote( function( x, B2B = Inf, offset = 0) {
      b <- undB( -B2B)
      s <- undB( -x)
      .(fs)( -dB((s + b)) - offset)
   })))
}
