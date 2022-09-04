#' An example `BERDF` dataframe created by `simsigs()`, a function in a
#' forthcoming package `coherent`.
#'
#' `BERDF` is a standard `R` data frame created by the `simsigs()` function in
#' the forthcoming `coherent` package.  The observations have been `condense`d
#'
#' @format A dataframe with the following fields:
#' \describe{
#'     \item{Name}{Name of constellation used to create the record.}
#'     \item{SNR}{The SNR in Decibles of the observation.}
#'     \item{Bps}{The number of bits per symbol.  The number of bits in a
#'     simulation run is \code{Bps * N}}
#'     \item{NoisePower}{The actual noise power in the simulation run.  Since
#'     the noise is randomly generated, this is a stochastic item.}
#'     \item{N}{The number of symbols in the simulation run.}
#'     \item{SER}{The number of symbols errors observed in the simulation run.}
#'     \item{BER}{The number of bit errors observed in the simulation run.}
#' }
"BERDFc"
