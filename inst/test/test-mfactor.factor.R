

context('Coersion from factor')
test_that("levels and labels are handeled properly",{

	x <- factor(letters[1:5])
	# expanded levels 
	expect_identical(levels(mfactor(x)), levels(x))
	expect_identical(mfactor(x), mfactor(letters[1:5]))

	# expanded levels 
	expect_identical(levels(mfactor(x,levels=letters)), letters)
	expect_identical(mfactor(x,levels=letters), mfactor(letters[1:5], levels=letters))

	# reduced levels 
	expect_identical(levels(mfactor(x,levels=letters[1:3])), letters[1:3])
	expect_identical(mfactor(x,levels=letters[1:3]), mfactor(c(letters[1:3],NA,NA)))

	# expanded levels, with excluded values
	expect_identical(levels(mfactor(x,levels=letters)), letters)
	expect_identical(mfactor(x,levels=letters), 
					 mfactor(letters[1:5], levels=letters))

	# excluseions with labels
	expect_identical(levels(mfactor(x,exclude=c('c','d'),labels=LETTERS[1:3])),
					 LETTERS[1:3])
	expect_identical(mfactor(x,exclude=c('c','d'),labels=LETTERS[1:3]),
					 mfactor(c('A','B','','','C'),split = ','))
	# excluseions with expanded levels
	# excluseions with expanded levels and lables
	expect_identical(mfactor(x,exclude=c('c','d'),levels=letters,labels=LETTERS),
					 mfactor(c('A','B','','','E'),split = ',',levels = LETTERS))

	mfactor(x,exclude=c('c','d'),levels=letters)

})

#-- test_that("Exclusions do exclude inherited levels",{
#-- 	x = factor(mfactor(letters[1:5]), exclude = c('d','e'),none=NA)
#-- 	expect_identical(levels(x), letters[1:3])
#-- 	expect_identical(x, factor(c(letters[1:3],NA,NA), letters[1:3]))
#-- 
#-- 	x = factor(mfactor(letters[1:5]), exclude = c('d','e'),labels=LETTERS[1:3],none=NA)
#-- 	expect_identical(levels(x), LETTERS[1:3])
#-- 	expect_identical(x, factor(c(LETTERS[1:3],NA,NA), LETTERS[1:3]))
#-- })
	




