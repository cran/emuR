% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/locus.R
\name{locus}
\alias{locus}
\title{Calculate locus equations for two-dimensional data}
\usage{
locus(
  target,
  onset,
  labels.vow = NULL,
  yxline = TRUE,
  plotgraph = TRUE,
  axes = TRUE,
  ...
)
}
\arguments{
\item{target}{a numerical vector typically of F2 values at the vowel target}

\item{onset}{a numerical vector typically of the same length as target of
F2 values at the vowel onset}

\item{labels.vow}{an optionally character vector for plotting labels at the
points (target, onset) of the same length as target}

\item{yxline}{optionally plot the line target = onset.  Defaults to True.}

\item{plotgraph}{a logical vector for specifying whether the data should be
plotted. Defaults to True.}

\item{axes}{A logical vector indicating whether the axes should be plotted}

\item{...}{graphical options \link{par}}
}
\value{
A list containing regression diagnostics of the function lm() that
can be accessed with summary() and the estimated locus frequency in
$locus. A plot of values in the onset x target plane with superimposed
locus equation and line onset=target.
}
\description{
The function plots a locus equation and returns associated statistical
information.
}
\details{
A locus equation is a straight line regression fitted with lm() in which
the F2- values typically at the vowel onset are regressed on those of the
target. The slope can be used to give an indication of target-on-onset
coarticulatory influences.

The best estimate of the locus frequency is where the locus equation
bisects the line target = onset.
}
\examples{


 # calculate an F2-locus equation for initial [d] 
# preceding lax vowels produced by female speaker "68".
# the onset is taken at the vowel onset; the
# vowel target is taken at the vowel's temporal midpoint.

# identify initial "d" of speaker "68"
temp <- vowlax.left == "d" & vowlax.spkr == "68"
# get the F2 value at the vowel's temporal midpoint
targ <- dcut(vowlax.fdat[temp,2], .5, prop=TRUE)
# F2 value at the vowel's acoustic onset.
on <- dcut(vowlax.fdat[temp,2], 0, prop=TRUE)

# locus equation plot
result <- locus(targ, on, vowlax.l[temp])
# statistical diagnostics of the regression line (locus equation)
summary(result)
# intercept and slope
result$coeff
# best estimate of the locus frequency, i.e. the
# point of bisection of on = TRUEarg with the regression line
result$locus


}
\author{
Jonathan Harrington
}
\keyword{math}
