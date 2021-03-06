% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emuR-requery.database.R
\name{requery_hier}
\alias{requery_hier}
\title{Requery hierarchical context of a segment list in an emuDB}
\usage{
requery_hier(
  emuDBhandle,
  seglist,
  level,
  collapse = TRUE,
  resultType = "tibble",
  calcTimes = TRUE,
  timeRefSegmentLevel = NULL,
  verbose = FALSE
)
}
\arguments{
\item{emuDBhandle}{emuDB handle as returned by \code{\link{load_emuDB}}}

\item{seglist}{segment list to requery on (type: \link{emuRsegs})}

\item{level}{character string: name of target level}

\item{collapse}{collapse the found items in the requested level to a sequence (concatenated with ->). 
If set to \code{FALSE} separate items as new entries in the emuRsegs object are returned.}

\item{resultType}{type of result (either 'tibble' == default or 'emuRsegs')}

\item{calcTimes}{calculate times for resulting segments (results in \code{NA} values for start and end 
times in emuseg/emuRsegs). As it can be very computationally expensive to 
calculate the times for large nested hierarchies, it can be turned off via this boolean parameter.}

\item{timeRefSegmentLevel}{set time segment level from which to derive time information. It is only 
necessary to set this parameter if more than one child level contains time information and the queried 
parent level is of type ITEM.}

\item{verbose}{be verbose. Set this to \code{TRUE} if you wish to choose which path to traverse on intersecting 
hierarchies. If set to \code{FALSE} (the default) all paths will be traversed (= legacy EMU behaviour).}
}
\value{
result set object of class \link{emuRsegs} or \link{tibble}
}
\description{
Function to requery the hierarchical context of a segment list queried from an emuDB
}
\details{
A segment is defined as a single item or a chain of items from the respective level, e.g. 
if a level in a bundle instance has labels 'a', 'b' and 'c' in that order, 'a' or 'a->b' or 'a->b->c' 
are all valid segments, 'a->c' is not. For each segment of the input segment list \code{seglist} 
the function checks the start and end item for hierarchically linked items in the given target 
level, and based on them constructs segments in the target level. As the start item in the resulting 
segment the item with the lowest sequence index is chosen; for the end item that with the highest 
sequence index. If the parameter \code{collapse} is set to \code{TRUE} (the default), it is guaranteed 
that result and input segment list have the same length (for each input 
segment one or multiple segments on the target level was found). If multiple linked segments where found
they are collapsed into a sequence of segments ('a->b->c') and if no linked items where found an NA row 
is inserted.
}
\examples{
\dontrun{

##################################
# prerequisite: loaded ae emuDB 
# (see ?load_emuDB for more information)

## Downward requery: find 'Phoneme' sequences of all words 'beautiful' (of level 'Text')
## Note that the resulting segments consists of phoneme sequences and have therefore 
## the same length as the word segments.

sl1 = query(ae, "Text == beautiful")
requery_hier(ae, sl1, level = "Phoneme")

## Upward requery: find all word segments that dominate a 'p' on level 'Phoneme'
## Note that the resulting segments are larger than the input segments,
## because they contain the complete words.

sl1 = query(ae, "Phonetic == p")
requery_hier(ae, sl1, level = 'Text')

## Why is there a 'p' the word 'emphazised'? Requery the whole words back down to 'Phoneme' level:

requery_hier(ae, sl1, level = 'Phoneme')

## ... because of 'stop epenthesis' a 'p' is inserted between 'm' and 'f'

## Combined requery: last phonemes of all words beginning with 'an'.
## Note that we use a regular expression 'an.*' (EQL operator '=~') in the query.

sl1=query(ae, "Text =~ an.*")
requery_seq(ae, requery_hier(ae, sl1, level = 'Phoneme'), offsetRef = 'END')

}
}
\seealso{
\code{\link{query}} \code{\link{requery_seq}} \code{\link{emuRsegs}}
}
\keyword{database}
\keyword{emuDB}
\keyword{requery}
