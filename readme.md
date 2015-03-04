
(This project is currently being re-implemented with a more efficient back end, so some features are currently unavailable)

## Installation Instructions

1. Install the release version of `devtools` from CRAN with `install.packages("devtools")`.

2. To install mfactor from github use: .


        ```R
        devtools::install_github("jdthorpe/mfactor")
        ```

    note that in windows, you may have to build the github toolws


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

