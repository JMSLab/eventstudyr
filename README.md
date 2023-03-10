# eventstudyr

## Overview

The **eventstudyr** package implements tools for estimating linear panel event study models, following the recommendations in Freyaldenhoven et al. (2021).
Includes sup-t bands, testing for key hypotheses, least wiggly path through the Wald region.
Allows instrumental variables estimation following Freyaldenhoven et al. (2019).

## Installation

```R
# Install from GitHub
install.packages("devtools")
devtools::install_github("JMSLab/eventstudyr")
```

## Usage

Find a minimal example below. 
For more examples see the package documentation.

```R
library(eventstudyr)

estimates_ols <- EventStudy(
   estimator = "OLS",
   data = df_sample_dynamic,   # Use package sample data
   outcomevar = "y_smooth_m",
   policyvar = "z",
   idvar = "id",
   timevar = "t",
   controls = "x_r",
   pre = 0,  overidpost = 4
)

plt_ols <- EventStudyPlot(estimates = estimates_ols)
plt_ols
```

## Citation

Simon Freyaldenhoven, Christian Hansen, Jorge Pérez Pérez, and Jesse M. Shapiro. "Visualization, Identification, and Estimation in the Panel Event-Study Design." [NBER Working Paper No. 29170](https://www.nber.org/papers/w29170),
August 2021.

Simon Freyaldenhoven, Christian Hansen, Jorge Pérez Pérez, Jesse M. Shapiro, Veli M. Andirin, Richard Calvo, Santiago Hermo, Nathan Schor, Emily Wang. "`eventstudyr` package." Code and data repository at https://github.com/JMSLab/eventstudyr, March 2023.

