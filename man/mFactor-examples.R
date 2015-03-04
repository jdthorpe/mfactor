(mf <- mfactor(c(NA,"",'1','2','3','1,2','2,3','1,2,3')
	,split=','
	,levels = c('1','2','3','4')
	,labels = c('Never','Rarely','Sometimes','Often')
	,ordered = TRUE))

# subsets
mf[[2]] 
mf[2:4]

# subset assignment
mf[[8]] <- list(c('Sometimes','Often')) # a new multi factor level
mf
# if a list is assigned, recycling happens at the list-element level
mf[1:2] <- list(c('Never','Often'))
mf
# if a vector is assigned, recycling happens at the vector-element level
mf[6:10] <- c('Sometimes','Often')
mf
# invalid factor level assignment creates missing values and issues a warning
mf[9:10] <- c('hello','world')

#conversion to factor types
as.factor(mf)
as.factor(mf,sep = '|')
# this median value provided (missing if even number of values provided)
as.factor(mf,median) #CAREFUL - median rerurned a new value for mf[1]
# the median value in the absence of ties, the upper median if even number provided
as.factor(mf,upper.median)
# the median value in the absence of ties, the upper median if even number provided
as.factor(mf,lower.median)
# the lowest value in the absence of ties, the upper median if even number provided
as.factor(mf,min)
# the highest value in the absence of ties, the upper median if even number provided
as.factor(mf,max)
# a custom function which returns exactly 1 argument
as.factor(mf,collapse=function(x)floor(median(x)[1]))
# a string with which to replace the multi-valued entries
as.factor(mf,collapse='<Multiple-Values>')

# IMPORTANT!!
# did you notice that in the above conversions to factor, 
# median() returned a value not among the initial values of mf[1]? 
mf[1]
as.factor(mf[1],median) 
# if the middle value is desired, upper.median or lower.median may be more appropriate
as.factor(mf[1],lower.median) 
as.factor(mf[1],upper.median) 

#conversion to other types
as.list(mf)
as.list(mf,levels=TRUE)

identical(mf,mfactor(as.list(mf),ordered=T,levels=levels(mf)))
as.character(mf,sep='|')
as.matrix(mf)

#comparisons are available for ordered multi-factors
mf < 'Sometimes'
mf <= 'Sometimes'
mf > 'Sometimes'
mf >= 'Sometimes'
mf == 'Sometimes'
mf != 'Sometimes'

# multifactors can be added (element wise union) and 
# subtracted  (element wise setdiff)
(x <- mfactor(letters[1:6],levels = letters[1:10]))
(y <- mfactor(letters[8:3],levels = letters[1:10]))
(z <- mfactor(letters[1:5],levels = letters[1:10]))

# you can subtract things that can be coerced and whose length is a divisor of 
# length of x (via the usual R rules) and which can be coerced to an mfactor

x - 'a'
x + list('a','b') # notice the recycling rule is used because list('a','b') has length > 1
z + list(c('a','b')) # now 'a' and 'b' are added to each element...

# you can also simplify ordered factors by min / max value
(x <- mfactor(x,ordered=TRUE))
(y <- mfactor(y,ordered=TRUE))

max(x + y)
min(x + y)
min(x +'d')

# comparisons are available for ordered multi-factors
mf2 <- mf
mf < mf2 # implemented as max(mf) < min(mf2)
mf <= mf2 # implemented as max(mf) <= min(mf2)
mf > mf2 # implemented as min(mf) > max(mf2)
mf >= mf2 # implemented as min(mf) >= max(mf2)

# TODO: these two are still not intuitive
options( mfactor.strict.compare=TRUE)
mf == mf2 # roughly any(max(mf) %in%  min(mf2))
mf != mf2 # roughly !any(max(mf) %in%  min(mf2))
options( mfactor.strict.compare=FALSE)
mf == mf2 # roughly any(max(mf) %in%  min(mf2))
mf != mf2 # roughly !any(max(mf) %in%  min(mf2))

# extension or truncation
length(mf)
length(mf) <- 20
length(mf)
length(mf) <- 5
length(mf)

# setting and identifying missing values
is.na(mf)
is.na(mf) <- 5
is.na(mf)

# setting and identifying missing values
is.none(mf)
is.none(mf) <- 5
is.none(mf)

# re-assignment of the levels
levels(mf) <- LETTERS[15:18]

# element names
names(mf) <- paste('x-',1:5,sep = '')

# conversion to indicator matrix
(mx <- as.matrix(mfactor(mf2[1:5])))
dimnames(mx)[[1]] <- c('foo','bar','baz','ball','bat')
mx[2,2] <- TRUE
mx[5,2:3] <- TRUE
mfactor(mx)

# INCLUSION IN DATA FRAMES
(dd <- data.frame(a = LETTERS[1:6]
			,b = mfactor(c(NA,'1','a','b','a,1','1,b'),levels = c('1','a','b','c'),split=',',ordered = TRUE)
			,c = 1:6 + 1000)
			)
dd$b

# EXTRACTING A COLUMN OF THE UNDERLYING MATRIX (syntactic sugar)
mf2$Rarely
mf2$'Rarely'

# THE STRING USED TO INDICATE 'NONE' VALUES CAN BE SET
as.character(mf2[1:3])
options('mfactor.none'='<hello>') # glabally
as.character(mf2[1:3])
as.character(mf2[1:3],none='<<world>>') # and locally

# ------------------------------
# because the empty string is interpreted as the emtpy value 
# then some SOME THINGS TO NOTE: 
# ------------------------------
#-- # note that this is not currently true, but would be true 
#-- # THESE ARE NOT THE SAME
#-- levels(mfactor(c('','a'),none=""))
#-- levels(factor(c('','a')))
#-- 
#-- # THESE ARE THE SAME
#-- levels(mfactor(c('','a')))
#-- levels(factor(c(NA,'a')))
#-- 
#-- # BUT THESE ARE NOT
#-- as.character(mfactor(c('','a')))
#-- as.character(factor(c(NA,'a')))


