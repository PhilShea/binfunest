test_that("cor.mle works", {
   require("stats4", quietly = TRUE)
   Q_ab <- function( gamma, a, b)
      Q_(sqrt( a * gamma / (1 + b * gamma)))
   nobs <- 6 # This is the Mathematica test data.
   s <- c( sqrt(10), 10, 10 * sqrt(10), 100, 100 * sqrt(10), 1000)
   r <- c( 246, 211, 220, 23, 9, 6)
   N <- c( 20000, 500000, 20000000, 20000000, 20000000, 20000000)
   m <-  mleB2B( Errors=r, N=N, f=Q_ab, fparms=list( gamma=s),
                 start=c(a=2, b=undB(-11)), method="L-BFGS-B", lower=list(b=0))
  expect_equal( cor.mle( m), array( c(1., 0.9608065, 0.9608065, 1.), c(2,2)))
})

test_that("cor.mle failos when it should", {
   x <- 1:10 # create a simple vector to pass
   expect_error( cor.mle( x))
})
