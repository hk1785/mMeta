# R package: mMeta

Title: Multi-marker meta analysis (mMeta) & Adaptive multi-marker meta-analysis (aMeta)

Version: 1.0

Date: 2021-2-11

Author: Hyunwook Koh

Maintainer: Hyunwook Koh <hyunwook.koh@stonybrook.edu>

Description: This R package provides facilities for multi-marker meta analysis (mMeta) and adaptive multi-marker meta-analysis (aMeta) which conduct meta-analyses to combine multiple studies throughout multiple related markers.

NeedsCompilation: No

Depends: R(>= 4.0.3)

Imports: forestplot, MASS, vegan

License: GPL-2

URL: https://github.com/hk1785/mMeta

## Reference

* Koh H, Zhao N. Meta-analysis methods for multiple related markers: applications to microbiome studies with the results on multiple α-diversity indices. **_Statistics in Medicine_** (_Accepted_)

## Troubleshooting Tips

If you have any problems for using this R package, please report in Issues (https://github.com/hk1785/mMeta/issues) or email Hyunwook Koh (hyunwook.koh@stonybrook.edu).

## Prerequites

devtools
```
install.packages("devtools")
```
forestplot
```
install.packages("forestplot")
```
MASS
```
install.packages("MASS")
```
vegan
```
install.packages("vegan")
```

## Installation

```
library(devtools)
install_github("hk1785/mMeta", force=T)
```

---------------------------------------------------------------------------------------------------------------------------------------

# Manual
This R package includes two core functions, mMeta.aMeta and mMeta.aMeta.plot, and one summary data set, HIV.Sum.Data. Please find the details below.

## :mag: mMeta.aMeta

### Description
This function conducts the meta-analysis using mMeta and aMeta to combine multiple studies throughout multiple related markers.

### Usage
```
mMeta.aMeta(est, std.err, tau0=FALSE, n.perm=5000, seed=NULL)
```

### Arguments
* _est_ - A data.frame (or matrix) of study-specific estimates (e.g., regression coefficients). Rows are studies (row names are study names) and columns are markers (column names are marker names). 'est' can contain missing elements (NAs), but the locations of NAs should be matched between 'est' and 'std.err'. The number of studies (i.e., the number of rows) should be greater than or equal to 10.
* _std.err_ - A data.frame (or matrix) of study-specific standard errors. Rows are studies (row names are study names) and columns are markers (column names are marker names). 'std.err' can contain missing elements (NAs), but the locations of NAs should be matched between 'est' and 'std.err'. The number of studies (i.e., the number of rows) should be greater than or equal to 10.
* _tau0_ - An indicator to set up the heterogeneous variance exists under the null (TRUE), which is the traditional random effects meta-analysis, or the heterogeneous variance does not exist under the null (FALSE), which is the Han and Eskin’s modified random effects meta-analysis. Default is TRUE.
* _n.perm_ - A number of permutations. Default is n.perm=5000. If the number of studies < 15, all possible permutations are implemented and 'n.perm' is ignored.
* _seed_ - A seed number to obtain the same outcomes repeatedly. Default is NULL for no seed number. If the number of studies < 15, 'seed' is ignored as all possible permutations are implemented.

### Values
Pooled estimates, 95% confidence intervals, p-values for each marker and across markers using mMeta, and a p-value across markers using aMeta

### References
* Koh H, Zhao N. Meta-analysis methods for multiple related markers: applications to microbiome studies with the results on multiple α-diversity indices. **_Statistics in Medicine_** (_Accepted_)

### Example
Import requisite R packages
```
library(forestplot)
library(MASS)
library(vegan)
library(mMeta)
```
Import example summary data
```
data(Sum.Data)
est <- Sum.Data$est
std.err <- Sum.Data$std.err
```
Fit mMeta and aMeta
```
out1 <- mMeta.aMeta(est, std.err, seed=123)
out1
```
Import summary data for 15 HIV studies and 6 alpha-diversity indices 
```
data(HIV.Sum.Data)
est <- HIV.Sum.Data$est
std.err <- HIV.Sum.Data$std.err
```
Fit mMeta and aMeta
```
out2 <- mMeta.aMeta(est, std.err, seed=123)
out2
```

## :mag: mMeta.aMeta.plot

### Description
This function draws a forest plot for each marker, and across markers using mMeta and aMeta.

### Usage
```
mMeta.aMeta.plot(mMeta.aMeta.out)
```

### Arguments
* _mMeta.aMeta.out_ - An output obtained using the mMeta.aMeta function.

### Values
A forest plot for each marker, and across markers using mMeta and aMeta

### References
* Koh H, Zhao N. Meta-analysis methods for multiple related markers: applications to microbiome studies with the results on multiple α-diversity indices. **_Statistics in Medicine_** (_Accepted_)

### Example
Import requisite R packages
```
library(forestplot)
library(MASS)
library(vegan)
library(mMeta)
```
Import example summary data
```
data(Sum.Data)
est <- Sum.Data$est
std.err <- Sum.Data$std.err
```
Fit mMeta and aMeta
```
out1 <- mMeta.aMeta(est, std.err, seed=123)
out1
```
Draw a forest plot
```
mMeta.aMeta.plot(mMeta.aMeta.out=out1)
```
Import summary data for 15 HIV studies and 6 alpha-diversity indices 
```
data(HIV.Sum.Data)
est <- HIV.Sum.Data$est
std.err <- HIV.Sum.Data$std.err
```
Fit mMeta and aMeta
```
out2 <- mMeta.aMeta(est, std.err, seed=123)
out2
```
Draw a forest plot
```
mMeta.aMeta.plot(mMeta.aMeta.out=out2)
```

## :mag: HIV.Sum.Data

### Description
A list of regression coefficients and their standard errors for the 15 human microbiome studies for the association between HIV status and each of the 6 alpha-diversity indices, Richness (Observed), Shannon, Simpson, PD, PE and PQE. 

### References
* Koh H, Zhao N. Meta-analysis methods for multiple related markers: applications to microbiome studies with the results on multiple α-diversity indices. **_Statistics in Medicine_** (_Accepted_)

### Example
Import requisite R packages
```
library(mMeta)
```
Import summary data for 15 HIV studies and 6 alpha-diversity indices 
```
data(HIV.Sum.Data)
est <- HIV.Sum.Data$est # Regression coefficients
std.err <- HIV.Sum.Data$std.err # Standard errors
```
