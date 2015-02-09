## ------------------------------------------------------------------------
# A 'ragged array'
ARRAY <- list('a','b',c('b','c'))
ARRAY

# an indicator matirx
MATRIX <- matrix(c(1,0,0,
				   0,1,0,
				   0,1,1),nrow=3,byrow=T,dimnames = list(1:3,letters[1:3]))
MATRIX

# a data.frame with indicator variables. 
DATA.FRAME <- data.frame(a = c(1,0,0),
						 b = c(0,1,0),
						 c = c(0,1,1))
DATA.FRAME

## ------------------------------------------------------------------------
mf <- mfactor:::mfactor.list(strsplit(c(NA,"",'1','2','3','1,2','2,3','1,2,3'),split=',')
	,levels = c('1','2','3','4')
	,labels = c('Never','Rarely','Sometimes','Often')
	,ordered = TRUE)

mf <- mfactor(x =c(NA,"",'1','2','3','1,2','2,3','1,2,3')
	,split=','
	,levels = c('1','2','3','4')
	,labels = c('Never','Rarely','Sometimes','Often')
	,ordered = TRUE)

