% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epg.class.R
\name{epgplot}
\alias{epgplot}
\title{Plot palatographic data}
\usage{
epgplot(
  epgdata,
  select = NULL,
  numbering = "times",
  gridlines = TRUE,
  mfrow = NULL,
  col = 1,
  mar = c(0.8, 0.1, 0.8, 0.1),
  xlim = NULL
)
}
\arguments{
\item{epgdata}{An eight-columned EPG-compressed trackdata object, or an
eight columned matrix of EPG-compressed trackdata, or a 3D palatographic
array that is the output of palate()}

\item{select}{A vector of times. Palatograms are plotted at these times
only. Note: this argument should only be used if epgdata is temporally
contiguous, i.e. the entire trackdata object contains palatograms at
successive multiple times of the EPG sampling frequency. (as in
coutts.epg$ftime). Defaults to NULL, in which case palatograms are plotted
for all times available in epgdata.}

\item{numbering}{Either "times" (default), or logical TRUE, or a character
vector of the same length as the number of segments in epgdata.  In the
default case, the times at which the palatograms occur are printed above
the palatograms. If logical TRUE, then the palatograms are numbered 1, 2, ...
number of segments and this value is printed above the palatograms. If a
character vector, then this must be the same length as the number of
segments in epgdata.}

\item{gridlines}{if TRUE (default) grid lines over the palatogram are drawn.}

\item{mfrow}{By default, the function tries to work out a sensible number
of rows and columns for plotting the palatograms. Otherwise, this can be
user-specified, in which case mfrow is a vector of two integer numeric
values.}

\item{col}{specify a colour for plotting the filled EPG cells.}

\item{mar}{A numerical vector of the form 'c(bottom, left, top, right)'
which gives the number of lines of margin to be specified on the four sides
of the plot. The default in this function is c(0.8, 0.1, 0.8, 0.1). (The
default in the R plot() function is c(5, 4, 4, 2) + 0.1.}

\item{xlim}{A numeric vector of two time values over which the epgdata
should be plotted.  Note: this argument should only be used if epgdata is
temporally contiguous, i.e. the entire trackdata object contains
palatograms at successive multiple times of the EPG sampling frequency. (as
in coutts.epg$ftime). Defaults to NULL (plot all time values).}
}
\description{
Function to plot palatograms from EPG compressed objects or from a
3D-palatographic array that is output from palate().
}
\details{
The function plots 62 values arranged over an 8 x 8 grid with columns 1 and
8 unfilled for row 1.  When there is a contact (1), the corresponding
rectangle of the grid is filled otherwise the rectangle is empty.
}
\examples{

epgplot(polhom.epg[10,])

# as above but between times 1295 ms and 1330 ms
epgplot(polhom.epg[10,], xlim=c(1295, 1330))

# the same as above, but the data is first
# converted to a 3D palatographic array
p <- palate(polhom.epg[10,])
epgplot(p, xlim=c(1295, 1330))

# plot palatograms 2 and 8
epgplot(p[,,c(2, 8)])

# as above but
# no gridlines, different colour, numbering rather than times
epgplot(p[,,c(2, 8)], gridlines=FALSE, col="pink", numbering=TRUE)

# as above but with a user-specified title

epgplot(p[,,c(2, 8)], gridlines=FALSE, col="pink", numbering=c("s1", "s2"))

# plot the palatograms in the second
# segment of coutts.epg that are closest in time
# to 16377 ms and 16633 ms
epgplot(coutts.epg[2,], c(16377, 16633))


}
\seealso{
\code{\link{epgai}} \code{\link{epgcog}} \code{\link{epggs}}
\code{\link{palate}}
}
\author{
Jonathan Harrington
}
\keyword{dplot}
