
## Overview

The multi-factor (mfactor) package handles "Ragged Enum" data, in which each observation takes zero or more of a fixed set of values.  
In R, such data are typically handled with one of these types  

 - `data.frame`s or `list`s of indicator vectors
 - an indicator matirx
 - lists-of-lists 
 - delimited character vectors 

The mfactor package serves as an intermediate to each of these types and includes S3 methods for `table()`, `as.factor()`, and `as.ordered()` 
for summarizing ragged enums and/or coercing them to ordinary factors.

## Getting Started

1. Install the release version of `devtools` from CRAN with `install.packages("devtools")`.

2. To install mfactor from github use: .


    ```R
    devtools::install_github("jdthorpe/mfactor")
    ```

    note that in windows, you may have to build the github tools using: 


    ```R
    library(devtools)
    build_github_devtools()
    #### Restart R before continuing ####
    ```

    * Alternatively, downoload the github repository to a zip file and use:

        ```R
        install.packages("mfactor.zip", repos = NULL)

        # Remove the package after installation
        unlink("mfactor.zip")
        ```

3. View the introduction vignette via:

	```R
	library(mfactor)
	vignette("An-Introduction-to-Mfactors")
	```
	or browse the HTML version of the vignette (*sans* it's CSS resources) [here](http://htmlpreview.github.io/?https://github.com/jdthorpe/mfactor/blob/master/inst/doc/An-Introduction-to-Mfactors.html).

