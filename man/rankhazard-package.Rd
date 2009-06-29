\name{rankhazard-package}
\alias{rankhazard-package}
\alias{rankhazard}
\docType{package}
\title{Rank-hazard plots}
\description{Rank-hazard plots visualize the relative importance of covariates in a proportional hazards model. The key idea is to rank the covariate values and plot the relative hazard as a function of ranks scaled to interval [0,1]. The relative hazard is plotted with respect to the reference hazard, which can be e.g. the hazard related to the median of the covariate. 
}
\details{
\tabular{ll}{
Package: \tab rankhazard\cr
Type: \tab Package\cr
Version: \tab 0.8\cr
Date: \tab 2009-06-29\cr
License: \tab GPL 2\cr
LazyLoad: \tab yes\cr
}
The function rankhazardplot receives a coxph (package survival) object or a cph (package Design) object as an argument and creates a rank-hazard plot of the 
covariates. The reference values for the relative hazards and legend texts can be provided as optional arguments. Plotting parameters such as, lwd, lty, col and pch
are passed to the plotting commands.
}
\author{
Juha Karvanen <juha.karvanen@iki.fi> 
}
\references{
J. Karvanen, F. E. Harrell Jr., Visualizing covariates in proportional hazards model. Statistics in Medicine, 28:1957--1966, 2009.
}
\keyword{ package }
\keyword{ hplot }
\keyword{ survival }

\examples{
library(survival)
model <- coxph(Surv(time,status) ~ age + sex + ph.karno, data=cancer, x=TRUE)
rankhazardplot(model)
}
