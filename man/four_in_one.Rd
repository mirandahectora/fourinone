% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/four_in_one.R
\name{four_in_one}
\alias{four_in_one}
\title{Create a Four-in-One Residuals Plot}
\usage{
four_in_one(model, plotcolor, npplinecolor, yintcolor)
}
\arguments{
\item{model}{A given fitted model object, such as, but not limited to,
those generated using lm() and aov().}

\item{plotcolor}{An optional value for the color of points shown on
each plot.}

\item{yintcolor}{optional value for the color of the y-intercept
line shown in the residual plots.}

\item{nppcolor}{An optional value for the color of the line shown
in the normal quantile plot.}
}
\value{
A plot containing a normal quantile plot, a histogram, and two
redisual plots.
}
\description{
This function creates a set of residual plots which can
be used to ascertain the applicability of a given model
to a data set. The function's output and the included plots
are based off of Minitab®'s "Four in One" residual plots, 
which each contain a normal quantile plot, a residual plot 
comparing predicted residuals to their actual value, a residual 
plot comparing predicted residuals to the order in which they 
are observed, and a histogram.
}
