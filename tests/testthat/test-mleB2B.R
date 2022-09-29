require( stats4)


test_that("mleB2B main function works", {
   # Set up test data.
   QPSKdB.B2B <- B2BConvert( QPSKdB)
   O1 <- 3
   B1 <- 16
   s  <- 0:20
   N  <- 1000000
   r  <- rbinom( length( s), N, QPSKdB.B2B( s, B1, O1))
   df <- data.frame( Errors=r, SNR=s, N=N)

   llsb2 <- function( b2b, offset)
      -sum( dbinom( r, N, QPSKdB.B2B( s, b2b, offset), log=TRUE))

   mle1 <- stats4::mle( llsb2, start=c( b2b=20, offset=0), nobs=length(s),
                        method="Nelder-Mead")

   # tests using dataframe for errors and snr, singular N.
   est1 <-  mleB2B( data=df, Errors="Errors", N=N, f=QPSKdB.B2B,
                    fparms=list( x="SNR"), start=c(b2b=20, offset=0))
   expect_type( est1, typeof( mle1))
   expect_equal( coef( mle1), coef( est1))

   #tests vector errors, snr and N.
   est2 <-  mleB2B( Errors=r, N=df$N, f=QPSKdB.B2B, fparms=list( x=s),
                    start=c(b2b=20, offset=0))
   expect_equal( coef( mle1),  coef( est2))

   # test N as a name.
   est3 <-  mleB2B( data=df, Errors="Errors", N="N", f=QPSKdB.B2B,
                    fparms=list( x="SNR"), start=c(b2b=20, offset=0))
   expect_equal( coef( mle1),  coef( est3))

   est4 <- mleB2B( data=df, Errors="Errors", N="N",
                   f=function(x, b2b = 20, offset = 0)
                      QPSKdB(-dB(( undB(-s) + undB( -b2b))) - offset),
                   fparms=list( x="SNR"), start=c(b2b=20, offset=0))
   expect_equal( coef( mle1),  coef( est4))
})

test_that( "mleB2B fails when it should", {
   # Set up test data.
   QPSKdB.B2B <- B2BConvert( QPSKdB)
   O1 <- 3
   B1 <- 16
   s <- 0:20
   N <- 1000000
   r <- rbinom( length( s), N, QPSKdB.B2B( s, B1, O1))
   df <- data.frame( Errors=r, SNR=s, N=N)

   expect_error( mleB2B( data=df, Errors="Err", N=N, f=QPSKdB.B2B,
                         fparms=list( x="SNR"), start=c(b2b=20, offset=0)),
                 "Err is not in data")
   expect_error( mleB2B( data=df, Errors="Errors", N="n", f=QPSKdB.B2B,
                         fparms=list( x="SNR"), start=c(b2b=20, offset=0)),
                 "n is not in data")
   #errors in fparm will generate odd messages.
   expect_error( mleB2B( data=df, Errors="Errors", N="N", f=QPSKdB.B2B,
                         fparms=list( x="S"), start=c(b2b=20, offset=0)))
})

test_that( "mleB2B method, control, and '...' work", {
   # Set up test data different from above.
   QPSKdB.B2B <- B2BConvert( QPSKdB)
   O2 <- 3
   B2 <- 80
   s <- 0:20
   N <- 1000000
   r2 <- rbinom( length( s), N, QPSKdB.B2B( s, B2, O2))
   df2 <- data.frame( Errors=r2, SNR=s, N=N)

   llsb2 <- function( b2b, offset)
      -sum( dbinom( r2, N, QPSKdB.B2B( s, b2b, offset), log=TRUE))
   mle1 <- stats4::mle( llsb2, start=c( b2b=20, offset=0), nobs=length(s),
                        method="Brent", fixed=list(b2b=80), lower=c( 0, -6),
                        upper=c( 100, 10))
   est1 <-  mleB2B( data=df2, Errors="Errors", N=N, f=QPSKdB.B2B,
                    fparms=list( x=s), start=c(b2b=20, offset=0),
                    method="Brent", fixed=list(b2b=80), lower=c(0, -6),
                    upper=c( 100, 10))
   expect_equal( coef( mle1), coef( est1))
})
