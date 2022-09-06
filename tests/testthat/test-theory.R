test_that("dB work well", {
  expect_equal(dB(10.0), 10.0)
})

test_that( "undB works", {
   expect_equal( undB( 10.0), 10.0)
})

test_that( "Q_ works", {
   expect_equal( Q_( c(0.0, 3.090232)), c(0.5, 0.00100000103))
})

test_that( "Q_Inv works", {
   expect_equal( Q_Inv( c( 0.1, 0.001)), c( 2.15472171, 9.7998225))
})

test_that( "Q_ is invertable", {
   expect_equal( Q_Inv( Q_( undB( 10.0/2))), 10.0)
})

test_that( "QPSKdB works", {
   expect_equal( QPSKdB( 10.0), 3.872108216e-06)
})

test_that( "DQPSKdB works)", {
   expect_equal( DQPSKdB( 10), 0.000022699965)
})

test_that( "DQPSKDDdB works", {
   expect_equal( DQPSKDDdB( 10), 7.744186445e-06)
})

test_that( "PSQPSKdB works", {
   expect_equal( PSQPSKdB( 10), 5.04054018e-08)
})

test_that( "MPSKdB works",{
   expect_equal( c( MPSKdB( 10, 7), MPSKdB( 10, 8)),
                 c(0.0004094214076, 0.0010113953207))
})

test_that( "MPSKdB throws error if M is not whole number", {
   expect_error( MPSKdB( 10, 12.5))
})

test_that( "MPSKdB throws error if M four or less", {
   expect_error( MPSKdB( 10, 4))
})

test_that( "MPSKdB.8 works",{
   expect_equal( MPSKdB.8( 10), MPSKdB( 10, 8))
})

test_that( "QAMdB.8.star works", {
   expect_equal( QAMdB.8.star( 10), 0.000231055007)
})

test_that( "QAMdB works", {
   expect_equal( QAMdB( 10, 16), 0.002338867491)
})

test_that( "QAMdB throws error if M not whole number", {
   expect_error( QAMdB( 10, 12.5))
})

test_that( "QAMdB throws error if M 4 or less", {
   expect_error( QAMdB( 10, 3))
} )

test_that( "QAMdB.16 works",{
   expect_equal( QAMdB.16( 10),  0.001751073574)
})

test_that( "mod_Inv for perr > 0 works", {
   expect_equal( mod_Inv( QPSKdB, QPSKdB( 7))$x, 7)
})

test_that( " mod_Inv for perr < 0 works", {
   expect_identical( mod_Inv(QPSKdB, -.1)$x, Inf)
})

test_that( "mod_InvV works", {
   expect_equal( mod_InvV( QPSKdB, QPSKdB(c(6,7))), c(6,7))
})
