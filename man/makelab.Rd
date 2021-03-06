% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makelab.R
\name{makelab}
\alias{makelab}
\title{Write out ESPS-style label files}
\usage{
makelab(vectimes, uttname, dir, extn = "xlab", labels = NULL)
}
\arguments{
\item{vectimes}{a vector of times}

\item{uttname}{a character vector of the same length as vectimes giving the
utterance name associated with each element of vectimes}

\item{dir}{a character specifying the directory}

\item{extn}{a character specifying the extension of the resulting files.
Defaults to xlab}

\item{labels}{either a single character vector or a character vector the
same length as vectimes.  Defaults to "T"}
}
\value{
ESPS-style label files are written out to the directory of the
user's choice. One ESPS-label file is created for each utterance containing
all time values for that utterance.
}
\description{
Writes out separate ESPS-label files for each utterance to a specified
directory.
}
\examples{

   #first two segments (for the whole example) of segmentlist vowlax 
   vowlax[1:2,]

   #format track of vowlax
   vowlax.fdat[1:2,]

   #Formant values of the midpoint of the segment
   vowlax.fdat.5 = dcut(vowlax.fdat,0.5,prop=TRUE)

   #the time marks of the midpoint of the segment
   times = vowlax.fdat.5[1:2,1]
   times

   #utterance names to the segments
   utts = utt(vowlax[1:2,])
   utts 

   #the path to save the label files to "." is the RHOME Directory
   path = "."

   #write the label files to path
   \dontrun{makelab(times, utts, path, labels="T")}

   #the first two segments are from the same utterance,
   #thus one label file was created in the R_HOME directory

}
\author{
Jonathan Harrington
}
\keyword{IO}
