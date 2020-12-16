\name{mMeta.aMeta.plot}
\alias{mMeta.aMeta.plot}
\title{
A forest plot for mMeta and aMeta
}
\description{
This function draws a forest plot for each marker, and across markers using mMeta and aMeta.
}
\usage{
mMeta.aMeta.plot(mMeta.aMeta.out)
}
\arguments{
  \item{mMeta.aMeta.out}{
An output obtained using the mMeta.aMeta function.
}
}

\value{
A forest plot for each marker, and across markers using mMeta and aMeta
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

# Draw a forest plot
mMeta.aMeta.plot(mMeta.aMeta.out=out1)

# Import summary data for 15 studies with 6 alpha-diversity indices.
data(HIV.Sum.Data)
est <- HIV.Sum.Data$est
std.err <- HIV.Sum.Data$std.err

# Fit mMeta and aMeta
out2 <- mMeta.aMeta(est, std.err, seed=123)

# Draw a forest plot
mMeta.aMeta.plot(mMeta.aMeta.out=out2)

}