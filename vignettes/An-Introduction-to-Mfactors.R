## ----, echo = FALSE------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
# A ragged array
(response_list <- list('a','b',c('b','c')))

# an indicator matirx
(response_matrix <- matrix(c(T,F,F,
						     F,T,F,
						     F,T,T),
						     nrow=3,
						     byrow=T,
						     dimnames = list(1:3,
											 c('a','b','c'))))

# a data.frame with indicator variables. 
(response_df <- data.frame(a = c(T,F,F),
						   b = c(F,T,F),
						   c = c(F,T,T)))

# A comma delimited set of strings
(response_delim <- c('a','b','b,c'))


## ------------------------------------------------------------------------
library(mfactor)
mf <- mfactor(x =c(NA,"",'1','2','3','1,2','2,3','1,2,3')
	,split=','
	,levels = c('1','2','3','4')
	,labels = c('Never','Rarely','Sometimes','Often')
	,ordered = TRUE)
mf

## ------------------------------------------------------------------------
# A ragged array
mfactor(response_delim,
		split = ',',
		levels=c('a','b','c'),
		labels=c("Love'em","Hate'em","Who?"))
mfactor(response_list,
		levels=c('a','b','c'),
		labels=c("Love'em","Hate'em","Who?"))
# Note that by default the levels are taken from the column names
# for data.frames and matricies (if they exist)
mfactor(response_matrix,
		labels=c("Love'em","Hate'em","Who?"))
mfactor(response_df,
		labels=c("Love'em","Hate'em","Who?"))

## ------------------------------------------------------------------------
#conversion to factor types
as.factor(mf)
as.factor(mf,sep = '|')

## ------------------------------------------------------------------------
# The 'smallest' value from each element:
as.factor(mf,min) # or equivelently: min(mf)
# The 'largest' value from each element:
as.factor(mf,max) # or equivelently: max(mf)
# this median value provided (missing if even number of values provided)
as.factor(mf,median) 

## ------------------------------------------------------------------------
# the median value in the absence of ties, and the upper value otherwise
as.factor(mf,upper.median)

# the median value in the absence of ties, and the lower median otherwise
as.factor(mf,lower.median)

# the median value in the absence of ties, otherwise a random selection from 
# the values closest to the arethmetic median.
as.factor(mf,median.random)

## ------------------------------------------------------------------------
AGG <- function(x)floor(median(x)[1])
as.factor(mf,AGG)

## ------------------------------------------------------------------------
as.factor(mf,collapse='<Multiple-Values>')

## ------------------------------------------------------------------------
options(mfactor.sep="::")
as.factor(mf)
options(mfactor.none= "<<<EMPTY>>>")
as.factor(mf)
options(mfactor.collapse="<<<Various>>>")
as.factor(mf)
options(mfactor.collapse=median.random)
as.factor(mf)

## ----,echo=FALSE---------------------------------------------------------
options(mfactor.sep=NULL)
options(mfactor.none= NULL)
options(mfactor.collapse=NULL)

## ------------------------------------------------------------------------
mf == NONE

## ------------------------------------------------------------------------
is.none(mf)
x <- mf
is.none(x[1:5]) <- T
x

## ------------------------------------------------------------------------
options('mfactor.none'='<Hello World>') 
x

## ----,echo=F-------------------------------------------------------------
options('mfactor.none'='<None>') # glabally
rm(x)

## ------------------------------------------------------------------------
# Ragged Arrays:
as.list(mf)
(mf_ragged_array <- as.list(mf))

# Character Vectors
(mf_character <- as.character(mf,sep='|'))

# Indicator Matricies.
(mf_matrix <- as.matrix(mf))

## ------------------------------------------------------------------------
mf$Sometimes

## ------------------------------------------------------------------------

(x <- mfactor(letters[1:6],levels = letters[1:10])) #length = 6
(y <- mfactor(letters[8:3],levels = letters[1:10])) #length = 6
(q <- mfactor(letters[1:3],levels = letters[1:10])) #length = 3
(p <- mfactor(letters[1:5],levels = letters[1:10])) #length = 5

x + y

# note that the usual recycling rules apply
x + q 

x + p # Warning: longer object length is not a multiple of shorter object length

## ------------------------------------------------------------------------
x - 'a'
x + list('a','b') # notice the recycling rule is used because list('a','b') has length > 1
x + list(c('a','b')) # now 'a' and 'b' are added to each element...

## ----,echo=FALSE---------------------------------------------------------
rm(x,y,p,q)

## ------------------------------------------------------------------------
mf2 <- mf
mf <  mf2 # implemented as max(mf) <  min(mf2)
mf <= mf2 # implemented as max(mf) <= min(mf2)
mf >  mf2 # implemented as min(mf) >  max(mf2)
mf >= mf2 # implemented as min(mf) >= max(mf2)

## ------------------------------------------------------------------------
# the default behavior
options( mfactor.strict.compare=TRUE) 
mf == mf2 # essentially a vectorized version of `identical(a,b)`
mf != mf2 # essentially a vectorized version of `!identical(a,b)`

# the alternate behavior
options( mfactor.strict.compare=FALSE)
mf == mf2 # essentially a vectorized version of `any(a %in%  b)`
mf != mf2 # essentially a vectorized version of`!any(a %in%  b)`

## ------------------------------------------------------------------------
mf < 'Sometimes'
mf <= 'Sometimes'
mf > 'Sometimes'
mf >= 'Sometimes'

## ------------------------------------------------------------------------
mf[[2]] 
mf[2:4]
mf[3] <- 'Sometimes'
mf

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
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

# element names
names(mf) <- paste0('item',seq_along(mf))
mf

## ------------------------------------------------------------------------
levels(mf)
levels(mf) <- LETTERS[15:18]
levels(mf)

