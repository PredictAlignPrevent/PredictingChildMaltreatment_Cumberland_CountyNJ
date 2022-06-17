# Predict-Align-Prevent + TACC Training Materials

## Environment Setup (Windows)

1. Install or upgrade to R version 4.1.2

2. Install Rtools

4. Add Rtools to the system search path by running the following in an R console:

        write('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', file = "~/.Renviron", append = TRUE)

5. Test Rtools installation in an R console:

        sys.which("make")

   This command should output path to the Rtools executable if Rtools is correctly installed and added to PATH.

6. Restore the project environment with the following commands in an R console:

        install.packages("renv")
        library(renv)
        setwd("~/data_trainings/PAP-TACC-2022")
        renv::restore()
