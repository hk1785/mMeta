\name{Sum.Data}
\alias{Sum.Data}
\docType{data}
\title{
Simulated summary data (i.e., regression coefficients and their standard errors) for 15 studies with 6 markers
}
\description{
A list of regression coefficients and their standard errors for the 15 studies and 6 markers.
}
\usage{data("Sum.Data")}
\references{
Koh H, Zhao N. Meta-analysis methods for multiple related markers: applications to microbiome studies with the results on multiple α-diversity indices (Under review)
}

\examples{
# Import example summary data for 15 studies and 6 markers.
data(Sum.Data)
est <- Sum.Data$est # Regression coefficients
std.err <- Sum.Data$std.err # Standard errors

}
