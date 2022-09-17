#' B2BConvert Converts a function of SNR into one of SNR, B2B, and Offset.
#'
#' Creates a function `f( -dB( undB( -s) + undB( -B2B)) - offset)`
#'
#' Note that all quantities are assumed to be in Decibels.
#'
#' @param f A function of a single argument `f( s)`. `f` can be a symbol, a
#'     string, or an anonymous function.  If a symbol, the symbol name will
#'     be remembered.  If a string, it will be passed to `match.fun` which
#'     will return the function form that is the value of the named symbol
#'     (i.e., not the symbol the string named).
#'
#' @return A function of three arguments `function( x, B2B, offset){...}`
#'     where `x` is the symbol SNR, `B2B` is the back-to-back SNR (i.e. the
#'     equivalent SNR when the input SNR (`x`) is infinite), and `offset` is
#'     the offset to the function `f` (i.e., if `B2B` where infinite).
#' @export
#'
#' @examples
#' QPSKdB.B2B <- B2BConvert( QPSKdB)
#'
B2BConvert <- function( f) {
   fs <- substitute(f) # Try to retain original function name
   if( is.character( fs)) fs <- match.fun( fs)
   # bquote returns a call, and call's remember their source,
   # that is, the source *before* bquote is applied.
   # eval will return a function, but it will attach the call's
   # source.  removeSource clears this, and the source can be
   # recovered from the tokenized code.
   removeSource( eval( bquote( function( x, B2B = Inf, offset = 0) {
      b <- undB( -B2B)
      s <- undB( -x)
      .(fs)( -dB((s + b)) - offset)
   })))
}
