#' B2BConvert Converts a function of SNR into one of SNR, B2B, and Offset.
#'
#' Creates a function `f( -dB( undB( -s) + undB( -B2B)) - offset)`
#'
#' Note that all quantities are assumed to be in Decibels.
#'
#' @param f A function of a single argument `f( s)`.
#'
#' @return A function of three arguments `f( s, B2B, offset)`..
#' @export
#'
#' @examples
#' QPSKdB.B2B <- B2BConvert( QPSKdB)
#'
B2BConvert <- function( f) {
   force(f)
   function( x, B2B = Inf, offset = 0) {
      b <- undB( -B2B)
      s <- undB( -x)
      do.call( f, list(-dB((s + b)) - offset))
   }
}
