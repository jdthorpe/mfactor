
context('Coersion from character')
test_that("repeated values don't mess up the levels attribute",{

	x = mfactor(c('a','a;a','a;b'),split = ';')
	expect_identical(levels(x),c('a','b'))
	expect_identical(length(attr(x,'mlevels')),1L)
	expect_identical(attr(x,'mlevels'),'1,2')

	x = mfactor(c('a','a;a'),c('a;a;a'),split = ';')
	expect_identical(levels(x),c('a'))
	expect_identical(length(attr(x,'mlevels')),0L)
	expect_identical(attr(x,'mlevels'),character(0))

})


