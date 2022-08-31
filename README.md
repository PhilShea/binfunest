README.rmd
================
Phil Shea
2022-08-30

# `binfunest`

<!-- badges: start -->

[![R-CMD-check](https://github.com/PhilShea/binfunest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PhilShea/binfunest/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of B2BQ is to simplify the estimation of offsets and
Back-to-Back “Q” for communications systems. Also eases maximum
likelihood estimation of any function generating binomial probabilities.

## Installation

You can install the development version of B2BQ from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PhilShea/binfunest")
```

## Example

This is a basic example which shows you how to solve a common problem of
estimating the <B@B> Q and offset. First we create a function which will
add offset and B2B parameters to a standard communications modulation
(Quadrature Phase Shift Keying, or QPSK). The package provides
theoretical performance curves for many common modulations. Then
`rbinom` is called to simulate measurements made at a range of signal to
noise ratios (SNR). All SNRs, offsets, and B2BQs are in Decibels. Note
that the generated samples include zeros, and that the first of these is
usefule in the estimate.

``` r
library(B2BQ, stats4)
QPSKdB.B2B <- B2BConvert( QPSKdB)
O1 <- 3 # offset
B1 <- 16 # B2BQ
s <- 0:20 # SNR Range 
N <- 1000000 # Number of samples
(r <- rbinom( length( s), N, QPSKdB.B2B( s, B1, O1)))
#>  [1] 161245 134497 108463  83285  61327  43263  28457  17424   9857   4936
#> [11]   2296    971    426    126     48     16      3      1      0      0
#> [21]      0
df <- data.frame( Errors=r, SNR=s, N=N) # place data in data frame

## This shows how you could do the work by hand
llsb2 <- function( b2b, offset)
       -sum( dbinom( r, N, QPSKdB.B2B( s, b2b, offset), log=TRUE))
mle1 <- stats4::mle( llsb2, start=c( b2b=20, offset=0), nobs=length(s),
                   method="Nelder-Mead")
stats4::coef( mle1)
#>       b2b    offset 
#> 15.922649  2.991044
# Below is the new function
est1 <-  mleB2B( data=df, Errors="Errors", N=N, f=QPSKdB.B2B,
                 fparms=list( x="SNR"), start=c(b2b=20, offset=0))

(est1coef <- stats4::coef( est1))
#>       b2b    offset 
#> 15.922649  2.991044
```

The plot below compares the theoretical curve to the curve with B2B and
offset, and the estimated curve.

``` r
plot( s, y=r/N, log='y', type='p', panel.first = grid())
#> Warning in xy.coords(x, y, xlabel, ylabel, log): 3 y values <= 0 omitted from
#> logarithmic plot
lines( s, QPSKdB( s))
lines( s, QPSKdB.B2B( s, B1, O1), col='red')
lines( s, y=QPSKdB.B2B( s, est1coef[1],  est1coef[2]), col="green")
legend( "bottomleft",
        legend=c( "Data",  "Theory", "3 dB Offset + 16 dB B2B", 
                  "Estimated"),
        lty=c( NA, 1, 1, 1), col=c( 'black', 'black', 'red', 'green'),
        pch=c( 1, NA, NA, NA))
```

<img src="man/figures/README-plotQab-1.png" width="100%" />
