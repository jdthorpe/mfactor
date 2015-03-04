
# a little QC
#-- foo <- data.frame(a=1:2,b=mfactor(c('a','a:b'),':'))
#-- bar <- data.frame(a=3:4,b=mfactor(c('c','c:b'),':'))
#-- foobar <- data.frame(a=1:4,b=mfactor(c('a','a:b','c','c:b'),':'))
#-- stopifnot(identical(rbind(foo,bar),foobar))
#-- rm(foo,bar,foobar)

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

