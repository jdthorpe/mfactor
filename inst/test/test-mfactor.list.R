



context('Coersion from list')
test_that("repeated values don't mess up the levels attribute",{

	x = mfactor(list('a',c('a','a'),c('a','b')))
	expect_identical(levels(x),c('a','b'))
	expect_identical(length(attr(x,'mlevels')),1L)
	expect_identical(attr(x,'mlevels'),'1,2')

	x = mfactor(list('a',c('a','a'),c('a','a','a')))
	expect_identical(levels(x),c('a'))
	expect_identical(length(attr(x,'mlevels')),0L)
	expect_identical(attr(x,'mlevels'),character(0))

})


