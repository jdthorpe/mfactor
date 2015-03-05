---
title: "An Introduction to multi-factors"
author: "Jason Thorpe [jdthorpe_at_gmail_dot_com]"
date: "2015-03-05"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{An Introduction to multi-factors}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---


# Why mfactors?

Multi-factors came about in order to deal with character vectors whose
contents are delimited fields, whose entries *should* be limited to a fixed
set of values (levels), and whose values *should* be limited to a single
value per entry.   If we we remove the words 'delimited' and '*should*' from
the prevous sentance, we would have a good definition of a `factor`
variable.  

However, the process that generates such data may not be constrained.
Specifically the data that motivated this class were derives from paper
surveys in which individuals were asked to pick 'the single best response'
to dozens of questions, but often indicted more than one response.   If this
same questionnaire were administered electronically, the responses could
have been been constrained, but sadly, this was not the case.  

## Alternatives.  

Before describing what multi-factors are and how they work, there are
several obvious alternatives that may suffice in some cases, including (1) a
ragged array, (2) an indicator matrix, (3) a data.frame with indicator
variables, or a character vector with delimited.  

To begin with an example, lets say we ask 3 individuals, to answer the
quesiton "How do you feel about the Seattle Seahawks?  (a) I don't like
them, (b) I love them, or (c) I've never heard of the Seattle Seahawks".
furthermore lets assume that the first respondent replied 'a', the second
respondent replied 'b', and the third respondent replid 'b' and 'c'.

The following are three solutions to modeling these responses, each of which
may be preferred in some situations:



```r
# A ragged array
(response_list <- list('a','b',c('b','c')))
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "b"
#> 
#> [[3]]
#> [1] "b" "c"

# an indicator matirx
(response_matrix <- matrix(c(T,F,F,
						     F,T,F,
						     F,T,T),
						     nrow=3,
						     byrow=T,
						     dimnames = list(1:3,
											 c('a','b','c'))))
#>       a     b     c
#> 1  TRUE FALSE FALSE
#> 2 FALSE  TRUE FALSE
#> 3 FALSE  TRUE  TRUE

# a data.frame with indicator variables. 
(response_df <- data.frame(a = c(T,F,F),
						   b = c(F,T,F),
						   c = c(F,T,T)))
#>       a     b     c
#> 1  TRUE FALSE FALSE
#> 2 FALSE  TRUE  TRUE
#> 3 FALSE FALSE  TRUE

# A comma delimited set of strings
(response_delim <- c('a','b','b,c'))
#> [1] "a"   "b"   "b,c"
```

## About Multi-Factors (`mfactors`)

### Creating `mfactor` variables

Creating `mfactor` variables is similar to creating ordinary factors from a
character vector, except that the the argument `split` may be supplied
(which is passed to `base::strsplit`) and is used to split each string into
it's individual elements, as follows:


```r
library(mfactor)
mf <- mfactor(x =c(NA,"",'1','2','3','1,2','2,3','1,2,3')
	,split=','
	,levels = c('1','2','3','4')
	,labels = c('Never','Rarely','Sometimes','Often')
	,ordered = TRUE)
mf
#> [1] <NA>                   <None>                 Never                 
#> [4] Rarely                 Sometimes              Never;Rarely          
#> [7] Rarely;Sometimes       Never;Rarely;Sometimes
#> Levels: Never < Rarely < Sometimes < Often
```

Notice that several things happend in this call to mfactor. 

- The elements of the argument `x` were separated using the regular
  expression supplied to the argument `split`
- The resulting elements were checked against the expected values supplied
  in the `levels` argument, and the user would be warned if additonal values
  were observed in `x` (after splitting `x` of course)
- The values were re-labeled with labels supplied to the argument `labels`

At each step many common errors (e.g. invalid values in the argumen `x`) are
checked, and warnings or error issued in response, which can help with the
data cleaning process.  

Many other types of objects can also be  coerced to multi-factors via
mfactors, as in:


```r
# A ragged array
mfactor(response_delim,
		split = ',',
		levels=c('a','b','c'),
		labels=c("Love'em","Hate'em","Who?"))
#> [1] Love'em      Hate'em      Hate'em;Who?
#> Levels: Love'em Hate'em Who?
mfactor(response_list,
		levels=c('a','b','c'),
		labels=c("Love'em","Hate'em","Who?"))
#> [1] Love'em      Hate'em      Hate'em;Who?
#> Levels: Love'em Hate'em Who?
# Note that by default the levels are taken from the column names
# for data.frames and matricies (if they exist)
mfactor(response_matrix,
		labels=c("Love'em","Hate'em","Who?"))
#>            1            2            3 
#>      Love'em      Hate'em Hate'em;Who? 
#> Levels: Love'em Hate'em Who?
mfactor(response_df,
		labels=c("Love'em","Hate'em","Who?"))
#> [1] Love'em      Hate'em;Who? Who?        
#> Levels: Love'em Hate'em Who?
```

### Coersion to Ordinary Factors

Typically the goal of data cleaning is to remove anamalies like the ones
previously discucssed, so mfactrs are able to be converted to orinary
factors using 'as.factor'. 


```r
#conversion to factor types
as.factor(mf)
#> [1] <NA>                   <None>                 Never                 
#> [4] Rarely                 Sometimes              Never;Rarely          
#> [7] Rarely;Sometimes       Never;Rarely;Sometimes
#> 8 Levels: <None> < Never < Rarely < Sometimes < ... < Never;Rarely;Sometimes
as.factor(mf,sep = '|')
#> [1] <NA>                   <None>                 Never                 
#> [4] Rarely                 Sometimes              Never|Rarely          
#> [7] Rarely|Sometimes       Never|Rarely|Sometimes
#> 8 Levels: <None> < Never < Rarely < Sometimes < ... < Never|Rarely|Sometimes
```

The above conversions were not very useful, since, except for the fact that
the codes were exchanged for labels, the data are in the same shape they
were at the beining of this exercise.

The real power of multi-factors comes from the fact that we can supply an
*aggregating function*, which returns a single value when supplied a vector
input. Examples of aggregating functions include `min`,`max`, and `median`.  


```r
# The 'smallest' value from each element:
as.factor(mf,min) # or equivelently: min(mf)
#> [1] <NA>      <None>    Never     Rarely    Sometimes Never     Rarely   
#> [8] Never    
#> Levels: <None> < Never < Rarely < Sometimes < Often
# The 'largest' value from each element:
as.factor(mf,max) # or equivelently: max(mf)
#> [1] <NA>      <None>    Never     Rarely    Sometimes Rarely    Sometimes
#> [8] Sometimes
#> Levels: <None> < Never < Rarely < Sometimes < Often
# this median value provided (missing if even number of values provided)
as.factor(mf,median) 
#> Warning in as.factor.mfactor(mf, median): NAs introduced by coersion from
#> levels:"Never;Rarely", "Rarely;Sometimes"
#> [1] <NA>      <None>    Never     Rarely    Sometimes <NA>      <NA>     
#> [8] Rarely   
#> Levels: <None> < Never < Rarely < Sometimes < Often
```

Did you notice that in the last example, supplying `median` as the
aggregaing function introduced meissing values in `mf` and a warning stating
`Warning in as.factor.mfactor(mf, median): NAs introduced by coersion from
levels:"Never;Rarely", "Rarely;Sometimes"`?  This happed  because
aggregaging functions *should* return an integer between zero and
`length(levels(mf))` or a mising value), but in this case median returned a
non integer value.  Because the `median` of the selected values is often
desirable, the `mfactor` package includes 3 aggregating functions which
avoid this problem.


```r
# the median value in the absence of ties, and the upper value otherwise
as.factor(mf,upper.median)
#> [1] <NA>      <None>    Never     Rarely    Sometimes Rarely    Sometimes
#> [8] Rarely   
#> Levels: <None> < Never < Rarely < Sometimes < Often

# the median value in the absence of ties, and the lower median otherwise
as.factor(mf,lower.median)
#> [1] <NA>      <None>    Never     Rarely    Sometimes Never     Rarely   
#> [8] Rarely   
#> Levels: <None> < Never < Rarely < Sometimes < Often

# the median value in the absence of ties, otherwise a random selection from 
# the values closest to the arethmetic median.
as.factor(mf,median.random)
#> [1] <NA>      <None>    Never     Rarely    Sometimes Never     Sometimes
#> [8] Rarely   
#> Levels: <None> < Never < Rarely < Sometimes < Often
```

User defined aggregating functions can also be used


```r
AGG <- function(x)floor(median(x)[1])
as.factor(mf,AGG)
#> [1] <NA>      <None>    Never     Rarely    Sometimes Never     Rarely   
#> [8] Rarely   
#> Levels: <None> < Never < Rarely < Sometimes < Often
```

or alternatively, a string indicating the value to be assigned to each
mutipliy-valued element can be specified


```r
as.factor(mf,collapse='<Multiple-Values>')
#> [1] <NA>              <None>            Never             Rarely           
#> [5] Sometimes         <Multiple-Values> <Multiple-Values> <Multiple-Values>
#> 6 Levels: <None> < Never < Rarely < Sometimes < ... < <Multiple-Values>
```

### Setting Coersion Options Globally 

Default values for the arguments `collapse`, `sep`, and `none` in calls to
`as.factor` and `as.ordered` can also be set golbally as follows:


```r
options(mfactor.sep="::")
as.factor(mf)
#> [1] <NA>                     <None>                  
#> [3] Never                    Rarely                  
#> [5] Sometimes                Never::Rarely           
#> [7] Rarely::Sometimes        Never::Rarely::Sometimes
#> 8 Levels: <None> < Never < Rarely < Sometimes < ... < Never::Rarely::Sometimes
options(mfactor.none= "<<<EMPTY>>>")
as.factor(mf)
#> [1] <NA>                     <<<EMPTY>>>             
#> [3] Never                    Rarely                  
#> [5] Sometimes                Never::Rarely           
#> [7] Rarely::Sometimes        Never::Rarely::Sometimes
#> 8 Levels: <<<EMPTY>>> < Never < Rarely < Sometimes < ... < Never::Rarely::Sometimes
options(mfactor.collapse="<<<Various>>>")
as.factor(mf)
#> [1] <NA>          <<<EMPTY>>>   Never         Rarely        Sometimes    
#> [6] <<<Various>>> <<<Various>>> <<<Various>>>
#> 6 Levels: <<<EMPTY>>> < Never < Rarely < Sometimes < ... < <<<Various>>>
options(mfactor.collapse=median.random)
as.factor(mf)
#> [1] <NA>        <<<EMPTY>>> Never       Rarely      Sometimes   Never      
#> [7] Sometimes   Rarely     
#> Levels: <<<EMPTY>>> < Never < Rarely < Sometimes < Often
```

Note that arguments supplied to `as.factor()` will always override global
options.



## The 'NONE" type

As mentioned earlier, each element of an multi-factor can be
thought of as the set of responses to a question indicating "Check all that
apply'.  In the case that none of the options apply (and there is no option
for 'None'), an element containing no values ("None of these apply to me")
is distinct from a missing value ("User was not asked this question"). 

There is a special `NONE` object which represents the former concept("None 
of these apply to me"), which can be used to test for elements in which no
response is selected:


```r
mf == NONE
#> Warning: Incompatible methods ("Ops.ord_mfactor", "Ops.mfactor") for "=="
#> [1]    NA  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
```

`is.none` and `is.none<-` can also be used to query and assign `NONE`
values:


```r
is.none(mf)
#> [1]    NA  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
x <- mf
is.none(x[1:5]) <- T
x
#> [1] <None>                 <None>                 <None>                
#> [4] <None>                 <None>                 Never;Rarely          
#> [7] Rarely;Sometimes       Never;Rarely;Sometimes
#> Levels: Never < Rarely < Sometimes < Often
```
and the string used to indicate 'NONE' values can be set globally 


```r
options('mfactor.none'='<Hello World>') 
x
#> [1] <Hello World>          <Hello World>          <Hello World>         
#> [4] <Hello World>          <Hello World>          Never;Rarely          
#> [7] Rarely;Sometimes       Never;Rarely;Sometimes
#> Levels: Never < Rarely < Sometimes < Often
```



## Conversion to other types

Multi-factors can be coerced to a variety of other types as follows:


```r
# Ragged Arrays:
as.list(mf)
#> [[1]]
#> [1] NA
#> 
#> [[2]]
#> integer(0)
#> 
#> [[3]]
#> [1] "Never"
#> 
#> [[4]]
#> [1] "Rarely"
#> 
#> [[5]]
#> [1] "Sometimes"
#> 
#> [[6]]
#> [1] "Never"  "Rarely"
#> 
#> [[7]]
#> [1] "Rarely"    "Sometimes"
#> 
#> [[8]]
#> [1] "Never"     "Rarely"    "Sometimes"
(mf_ragged_array <- as.list(mf))
#> [[1]]
#> [1] NA
#> 
#> [[2]]
#> integer(0)
#> 
#> [[3]]
#> [1] "Never"
#> 
#> [[4]]
#> [1] "Rarely"
#> 
#> [[5]]
#> [1] "Sometimes"
#> 
#> [[6]]
#> [1] "Never"  "Rarely"
#> 
#> [[7]]
#> [1] "Rarely"    "Sometimes"
#> 
#> [[8]]
#> [1] "Never"     "Rarely"    "Sometimes"

# Character Vectors
(mf_character <- as.character(mf,sep='|'))
#> [1] NA                       "<None>"                
#> [3] "Never"                  "Rarely"                
#> [5] "Sometimes"              "Never|Rarely"          
#> [7] "Rarely|Sometimes"       "Never|Rarely|Sometimes"

# Indicator Matricies.
(mf_matrix <- as.matrix(mf))
#>      Never Rarely Sometimes Often
#> [1,]    NA     NA        NA    NA
#> [2,] FALSE  FALSE     FALSE FALSE
#> [3,]  TRUE  FALSE     FALSE FALSE
#> [4,] FALSE   TRUE     FALSE FALSE
#> [5,] FALSE  FALSE      TRUE FALSE
#> [6,]  TRUE   TRUE     FALSE FALSE
#> [7,] FALSE   TRUE      TRUE FALSE
#> [8,]  TRUE   TRUE      TRUE FALSE
```

As a bit of syntactic sugar, a single column of the indicator matrix can be
extracted using the `$` operator 


```r
mf$Sometimes
#> [1]    NA FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE
```

## Algebraic Operators

The operators `+` and `-` are defined for multifactors that share a common
set of `levels()`, which can be thought of as `Vectorize`d versions `union`
and `setdiff`:


```r

(x <- mfactor(letters[1:6],levels = letters[1:10])) #length = 6
#> [1] a b c d e f
#> Levels: a b c d e f g h i j
(y <- mfactor(letters[8:3],levels = letters[1:10])) #length = 6
#> [1] h g f e d c
#> Levels: a b c d e f g h i j
(q <- mfactor(letters[1:3],levels = letters[1:10])) #length = 3
#> [1] a b c
#> Levels: a b c d e f g h i j
(p <- mfactor(letters[1:5],levels = letters[1:10])) #length = 5
#> [1] a b c d e
#> Levels: a b c d e f g h i j

x + y
#> [1] a;h b;g c;f d;e d;e c;f
#> Levels: a b c d e f g h i j

# note that the usual recycling rules apply
x + q 
#> [1] a   b   c   a;d b;e c;f
#> Levels: a b c d e f g h i j

x + p # Warning: longer object length is not a multiple of shorter object length
#> Warning in Ops.mfactor(x, p): longer object length is not a multiple of
#> shorter object length
#> [1] a   b   c   d   e   a;f
#> Levels: a b c d e f g h i j
```

Type coercion is automatically applied by the  `+` and `-` operators:


```r
x - 'a'
#> [1] <None> b      c      d      e      f     
#> Levels: a b c d e f g h i j
x + list('a','b') # notice the recycling rule is used because list('a','b') has length > 1
#> [1] a   b   a;c b;d a;e b;f
#> Levels: a b c d e f g h i j
x + list(c('a','b')) # now 'a' and 'b' are added to each element...
#> [1] a;b   a;b   a;b;c a;b;d a;b;e a;b;f
#> Levels: a b c d e f g h i j
```




## Comparisons operators

Comparisons are available for ordered multi-factors however, these can be
somewhat non-intuitive.  There equality 


```r
mf2 <- mf
mf <  mf2 # implemented as max(mf) <  min(mf2)
#> [1]    NA    NA FALSE FALSE FALSE FALSE FALSE FALSE
mf <= mf2 # implemented as max(mf) <= min(mf2)
#> [1]    NA    NA  TRUE  TRUE  TRUE FALSE FALSE FALSE
mf >  mf2 # implemented as min(mf) >  max(mf2)
#> [1]    NA    NA FALSE FALSE FALSE FALSE FALSE FALSE
mf >= mf2 # implemented as min(mf) >= max(mf2)
#> [1]    NA    NA  TRUE  TRUE  TRUE FALSE FALSE FALSE
```

Note that the options `mfactor.strict.compare` affects how the equaliti
operators evalute the comparison:


```r
# the default behavior
options( mfactor.strict.compare=TRUE) 
mf == mf2 # essentially a vectorized version of `identical(a,b)`
#> [1]   NA TRUE TRUE TRUE TRUE TRUE TRUE TRUE
mf != mf2 # essentially a vectorized version of `!identical(a,b)`
#> [1]    NA FALSE FALSE FALSE FALSE FALSE FALSE FALSE

# the alternate behavior
options( mfactor.strict.compare=FALSE)
mf == mf2 # essentially a vectorized version of `any(a %in%  b)`
#> [1]    NA FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
mf != mf2 # essentially a vectorized version of`!any(a %in%  b)`
#> [1]    NA  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
```

Lastly, there is implied type coercion for the comparison operators

```r
mf < 'Sometimes'
#> [1]    NA    NA  TRUE FALSE FALSE FALSE FALSE FALSE
mf <= 'Sometimes'
#> [1]    NA    NA  TRUE  TRUE FALSE  TRUE FALSE FALSE
mf > 'Sometimes'
#> [1]    NA    NA FALSE FALSE  TRUE FALSE FALSE FALSE
mf >= 'Sometimes'
#> [1]    NA    NA FALSE  TRUE  TRUE FALSE  TRUE FALSE
```

### Subsets and Subset Assignment 

Multi-factors behave in a similar way to ordinary factors in terms of
subsets and subset assingnment


```r
mf[[2]] 
#> [1] <None>
#> Levels: Never < Rarely < Sometimes < Often
mf[2:4]
#> [1] <None> Never  Rarely
#> Levels: Never < Rarely < Sometimes < Often
mf[3] <- 'Sometimes'
mf
#> [1] <NA>                   <None>                 Sometimes             
#> [4] Rarely                 Sometimes              Never;Rarely          
#> [7] Rarely;Sometimes       Never;Rarely;Sometimes
#> Levels: Never < Rarely < Sometimes < Often
```
except that we can assign a list containing subsets of `levels(x)` when
assiging values to a subset of a multi-factor.


```r
# subset assignment
mf[[8]] <- list(c('Sometimes','Often')) # a new multi factor level
mf
#> [1] <NA>             <None>           Sometimes        Rarely          
#> [5] Sometimes        Never;Rarely     Rarely;Sometimes Sometimes;Often 
#> Levels: Never < Rarely < Sometimes < Often

# if a list is assigned, recycling happens at the list-element level
mf[1:2] <- list(c('Never','Often'))
mf
#> [1] Never;Often      Never;Often      Sometimes        Rarely          
#> [5] Sometimes        Never;Rarely     Rarely;Sometimes Sometimes;Often 
#> Levels: Never < Rarely < Sometimes < Often

# if a vector is assigned, recycling happens at the vector-element level
mf[6:10] <- c('Sometimes','Often')
#> Warning in x[...] <- m: number of items to replace is not a multiple of
#> replacement length
mf
#>  [1] Never;Often Never;Often Sometimes   Rarely      Sometimes  
#>  [6] Sometimes   Often       Sometimes   Often       Sometimes  
#> Levels: Never < Rarely < Sometimes < Often

# invalid factor level assignment creates missing values and issues a warning
mf[9:10] <- c('hello','world')
#> Warning in `[<-.mfactor`(`*tmp*`, 9:10, value = c("hello", "world")):
#> invalid factor level, NAs generated
```

## Miscelaneous

Like other vectors, multi-factors can be extended or truncated using the
`length<-`, missing values can be set queried and set via `is.na` and
`is.na<-`, and the names can be set and queried via `names` and `names<-`:


```r
# extension or truncation
length(mf)
#> [1] 10
length(mf) <- 20
length(mf)
#> [1] 20
length(mf) <- 5
length(mf)
#> [1] 5

# setting and identifying missing values
is.na(mf)
#> [1] FALSE FALSE FALSE FALSE FALSE
is.na(mf) <- 5
is.na(mf)
#> [1] FALSE FALSE FALSE FALSE  TRUE

# element names
names(mf) <- paste0('item',seq_along(mf))
mf
#>       item1       item2       item3       item4       item5 
#> Never;Often Never;Often   Sometimes      Rarely        <NA> 
#> Levels: Never < Rarely < Sometimes < Often
```

And like factors, the levels can be queried and reassigned via `levels` and
`levels<-`


```r
levels(mf)
#> [1] "Never"     "Rarely"    "Sometimes" "Often"
levels(mf) <- LETTERS[15:18]
levels(mf)
#> [1] "O" "P" "Q" "R"
```



