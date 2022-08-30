test_that("B2BConvert works", {
   f <- B2BConvert( QPSKdB)
   b2b <- 15
   off <- 3
   sdB <- 1:15
   s <- undB( -sdB) + undB( -b2b)
  expect_equal( QPSKdB( -dB( s) - off), f( sdB, b2b, off))
})
