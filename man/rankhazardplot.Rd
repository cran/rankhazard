\name{rankhazardplot}
\alias{rankhazardplot}
\alias{rankhazardplot.cph}
\alias{rankhazardplot.coxph}
\alias{rankhazardplot.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create a rank-hazard plot }
\description{
\code{rankhazardplot} creates a rank-hazard plot of the covariates of a proportional hazards model.
}
\usage{
rankhazardplot(...)
\method{rankhazardplot}{coxph}(coxphobj,refvalues=NULL,x=NULL,legendtext=NULL,axistext=NULL,plottype="hazard",...)
\method{rankhazardplot}{cph}(cphobj,refvalues=NULL,x=NULL,legendtext=NULL,axistext=NULL,plottype="hazard",...)
\method{rankhazardplot}{default}(x,coefs=NULL,xp=NULL,refvalues=NULL,legendtext=NULL,axistext=NULL,plottype="hazard",col=NULL,pch=NULL,lwd=1,...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coxphobj}{An object of class 'coxph' (survival).}
  \item{cphobj}{An object of class 'cph' (rms).}
  \item{refvalues}{A vector of reference values given in the same order as the covariates in the model. If \code{NULL}, the medians of each covariate are used.  }
  \item{x}{A matrix or data frame of the covariate data. Needed if the \code{coxphobj$x} or \code{cphobj$x} does not exist.}  
  \item{legendtext}{A vector of covariate names for the legend box. If \code{NULL}, the column names are from \code{x}, \code{coxphobj$x} or \code{cphobj$x}.}
  \item{axistext}{A vector of covariate names and units for the x-axis. If \code{NULL}, the column names are from \code{x}, \code{coxphobj$x} or \code{cphobj$x}.}
  \item{plottype}{Scaling for the y-axis. Either \code{"hazard"} for the relative hazard with log-scale or \code{"loghazard"} for the logarithm of the relative hazard with linear scale.} 
  \item{coefs}{A vector of regression coefficients for the covariates. If \code{NULL}, \code{xp} is required.}
  \item{xp}{Predictions ("terms") for the covariates. If \code{NULL}, \code{coefs} is required and \code{xp} is calculated a product of \code{coefs} and \code{x}.}
\item{col}{See documentation for \code{\link{par}}.} 
\item{pch}{See documentation for \code{\link{points}}.} 
\item{lwd}{See documentation for \code{\link{par}}.} 
\item{\dots}{ Other arguments to passed to the plotting commands. }
}
\details{
  The function receives a coxph (package survival) object or a cph (package rms) object as an argument and creates a rank-hazard plot of the 
covariates. The reference values for the relative hazards and legend texts can be provided as optional arguments. Plotting parameters such as, lwd, lty, col and pch
are passed to the plotting commands.
}

\references{J. Karvanen, F. E. Harrell Jr., Visualizing covariates in proportional hazards model. Statistics in Medicine, 28:1957--1966, 2009.}
\author{ Juha Karvanen}
\seealso{ \code{\link{coxph}} and \code{cph} }
\examples{
library(survival)
model <- coxph(Surv(time,status) ~ age + sex + ph.karno, data=cancer, x=TRUE)
rankhazardplot(model)

%function(...) UseMethod("rankhazardplot")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{survival}
