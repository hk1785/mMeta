\name{HIV.Sum.Data}
\alias{HIV.Sum.Data}
\docType{data}
\title{
Summary data (i.e., regression coefficients and their standard errors) for 15 HIV studies with 6 alpha-diversity indices
}
\description{
A list of regression coefficients and their standard errors for the 15 human microbiome studies for the association between HIV status and each of the 6 alpha-diversity indices, Richness (Observed), Shannon, Simpson, PD, PE and PQE. 
}
\usage{data("HIV.Sum.Data")}
\references{
Koh H, Zhao N. Meta-analysis methods for multiple related markers: applications to microbiome studies with the results on multiple α-diversity indices (Under review)
}

\examples{
# Import summary data for 15 HIV studies with 6 alpha-diversity indices.
data(HIV.Sum.Data)
est <- HIV.Sum.Data$est # Regression coefficients
std.err <- HIV.Sum.Data$std.err # Standard errors

}
