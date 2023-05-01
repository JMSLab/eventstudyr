# eventstudyr <img src='man/figures/logo.png' align="right" height="139" />

## Overview

The **eventstudyr** package implements tools for estimating linear panel event study models, following the recommendations in [Freyaldenhoven et al. (2021)](https://www.nber.org/papers/w29170).
Includes sup-t bands, testing for key hypotheses, least wiggly path through the Wald region.
Allows instrumental variables estimation following [Freyaldenhoven et al. (2019)](https://www.aeaweb.org/articles?id=10.1257/aer.20180609).

## Installation

```R
# Install from CRAN
install.packages("eventstudyr")

# Install latest version from GitHub
install.packages("devtools")
devtools::install_github("JMSLab/eventstudyr")
```

## Usage

Find a minimal example below. 
For more examples see the package [documentation](https://cran.r-project.org/web/packages/eventstudyr/eventstudyr.pdf) and [vignette](https://cran.r-project.org/web/packages/eventstudyr/vignettes/documentation.html).

```R
library(eventstudyr)

estimates_ols <- EventStudy(
   estimator = "OLS",
   data = example_data,   # Use package sample data
   outcomevar = "y_smooth_m",
   policyvar = "z",
   idvar = "id",
   timevar = "t",
   controls = "x_r",
   pre = 0,  post = 4
)

plt <- EventStudyPlot(estimates = estimates_ols)
plt
```

## Citation

Simon Freyaldenhoven, Christian Hansen, Jorge Pérez Pérez, and Jesse M. Shapiro. "Visualization, Identification, and Estimation in the Panel Event-Study Design." [NBER Working Paper No. 29170](https://www.nber.org/papers/w29170),
August 2021.

Simon Freyaldenhoven, Christian Hansen, Jorge Pérez Pérez, Jesse M. Shapiro, Veli M. Andirin, Richard Calvo, Santiago Hermo, Nathan Schor, Emily Wang. "`eventstudyr` package." Code and data repository at https://github.com/JMSLab/eventstudyr, March 2023.

## Acknowledgments

Thank you to Eliana Sena Sarmiento and Melissa Wu for their excellent work testing and reviewing `eventstudyr`.
