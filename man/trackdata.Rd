% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emuR-objDocs.R
\name{trackdata}
\alias{trackdata}
\alias{Math.trackdata}
\alias{Math2.trackdata}
\alias{Ops.trackdata}
\alias{Summary.trackdata}
\title{Track data object}
\format{
\describe{ \item{$index}{a two columned matrix, each row keeps the
first and last index of the $data rows that belong to one segment}
\item{$ftime}{a two columned matrix, each row keeps the times marks of one
segment} \item{$data}{a multi-columned matrix with the real track values
for each segment} }
}
\description{
A track data object is the result of get_trackdata().
}
\note{
The entire data track is retrieved for each segment in the segment
list. The amount of data returned will depend on the sample rate and number
of columns in the track requested.
}
\section{Methods}{
 The following generic methods are implemented for
trackdata objects.  \describe{ \item{list("Arith")}{\code{"+"}, \code{"-"},
\code{"*"}, \code{"^"}, \code{"\%\%"}, \code{"\%/\%"}, \code{"/"}}
\item{list("Compare")}{\code{"=="}, \code{">"}, \code{"<"}, \code{"!="},
\code{"<="}, \code{">="}} \item{list("Logic")}{\code{"&"}, \code{"|"}.  }
\item{list("Ops")}{\code{"Arith"}, \code{"Compare"}, \code{"Logic"}}
\item{list("Math")}{\code{"abs"}, \code{"sign"}, \code{"sqrt"},
\code{"ceiling"}, \code{"floor"}, \code{"trunc"}, X \code{"cummax"},
\code{"cummin"}, \code{"cumprod"}, \code{"cumsum"}, \code{"log"},
\code{"log10"}, \code{"log2"}, \code{"log1p"}, \code{"acos"},
\code{"acosh"}, \code{"asin"}, \code{"asinh"}, \code{"atan"},
\code{"atanh"}, \code{"exp"}, \code{"expm1"}, \code{"cos"}, \code{"cosh"},
\code{"sin"}, \code{"sinh"}, \code{"tan"}, \code{"tanh"}, \code{"gamma"},
\code{"lgamma"}, \code{"digamma"}, \code{"trigamma"} }
\item{list("Math2")}{\code{"round"}, \code{"signif"}}
\item{list("Summary")}{\code{"max"}, \code{"min"}, \code{"range"},
\code{"prod"}, \code{"sum"}, \code{"any"}, \code{"all"}} }
}

\examples{

   data(demo.vowels.fm)
   data(demo.vowels)
   
   #Formant track data for the first segment of the segment list demo.vowels
   demo.vowels.fm[1]
  

}
\seealso{
\code{\link{get_trackdata}}, \code{\link{demo.vowels.fm}}
\code{\link{demo.all.rms}}
}
\keyword{classes}
