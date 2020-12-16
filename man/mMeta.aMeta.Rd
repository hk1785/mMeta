\name{mMeta.aMeta}
\alias{mMeta.aMeta}
\title{
Multi-marker meta analysis (mMeta) and Adaptive multi-marker meta-analysis (aMeta)
}
\description{
This function conducts the meta-analysis using mMeta and aMeta to combine multiple studies throughout multiple related markers.
}
\usage{
mMeta.aMeta(est, std.err, tau0=FALSE, n.perm=5000, seed=NULL)
}
\arguments{
  \item{est}{
A data.frame (or matrix) of study-specific estimates (e.g., regression coefficients). Rows are studies (row names are study names) and columns are markers (column names are marker names). 'est' can contain missing elements (NAs), but the locations of NAs should be matched between 'est' and 'std.err'. The number of studies (i.e., the number of rows) should be greater than or equal to 10.
}
  \item{std.err}{
A data.frame (or matrix) of study-specific standard errors. Rows are studies (row names are study names) and columns are markers (column names are marker names). 'std.err' can contain missing elements (NAs), but the locations of NAs should be matched between 'est' and 'std.err'. The number of studies (i.e., the number of rows) should be greater than or equal to 10.
}
  \item{tau0}{
An indicator to set up the heterogeneous variance exists under the null (TRUE), which is the traditional random effects meta-analysis, or the heterogeneous variance does not exist under the null (FALSE), which is the Han and Eskin’s modified random effects meta-analysis. Default is TRUE (the Han and Eskin’s modified random effects meta-analysis).
}
  \item{n.perm}{
A number of permutations. Default is n.perm=5000. If the number of studies is smaller than 15, all possible permutations are implemented and 'n.perm' is ignored.
}
  \item{seed}{
A seed number to obtain the same outcomes repeatedly. Default is NULL for no seed. If the number of studies is smaller than 15, 'seed' is ignored as all possible permutations are implemented.
}
}
\value{
Pooled estimates, 95% confidence intervals, p-values for each marker and across markers using mMeta, and a p-value across markers using aMeta
}
\references{
Koh H, Zhao N. Meta-analysis methods for multiple related markers: applications to microbiome studies with the results on multiple α-diversity indices (Under review)
}
\author{
Hyunwook Koh
}

\examples{
# Import requisite R packages
require(forestplot)
require(MASS)
require(vegan)
require(mMeta)

# Import example summary data
data(Sum.Data)
est <- Sum.Data$est
std.err <- Sum.Data$std.err

# Fit mMeta and aMeta
out1 <- mMeta.aMeta(est, std.err, seed=123)
out1

# Import summary data for 15 studies with 6 alpha-diversity indices.
data(HIV.Sum.Data)
est <- Sum.Data$est
std.err <- Sum.Data$std.err

# Fit mMeta and aMeta
out2 <- mMeta.aMeta(est, std.err, seed=123)
out2

}