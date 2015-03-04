

context('Coersion to factor')
test_that("Exclusions don't exclude explicit levels",{

	x <- factor(mfactor(letters[1:5]), exclude = c('d','e'), levels = letters,none=NA)
	expect_identical(levels(x), c(letters))
	expect_identical(x, factor(c(letters[1:3],NA,NA), letters))

	x <- factor(mfactor(letters[1:5]), exclude = c('d','e'), levels = letters)
	expect_identical(levels(x), 
					c(getOption('mfactor.none','<None>'),letters))
	expect_identical(x, factor(c(letters[1:3],
								 rep(getOption('mfactor.none','<None>'),2)), 
							   c(getOption('mfactor.none','<None>'),letters)))

	x <- factor(mfactor(letters[1:5]), exclude = c('d','e'), levels = letters, labels = LETTERS,none=NA)
	expect_identical(levels(x), LETTERS)
	expect_identical(x, factor(c(LETTERS[1:3],NA,NA), LETTERS))

})

test_that("Exclusions do exclude inherited levels",{
	x = factor(mfactor(letters[1:5]), exclude = c('d','e'),none=NA)
	expect_identical(levels(x), letters[1:3])
	expect_identical(x, factor(c(letters[1:3],NA,NA), letters[1:3]))

	x = factor(mfactor(letters[1:5]), exclude = c('d','e'),labels=LETTERS[1:3],none=NA)
	expect_identical(levels(x), LETTERS[1:3])
	expect_identical(x, factor(c(LETTERS[1:3],NA,NA), LETTERS[1:3]))
})
	




