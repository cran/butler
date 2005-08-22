\name{stopwatch}
\alias{stopwatch}
\title{Stop watch}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Profile the performance of function call.
}
\usage{stopwatch(f, reps = 2, interval = 0.02)}
\arguments{
\item{f}{function to profile}
\item{reps}{number of times to run}
\item{interval}{interval between samples (in seconds)}
}
\value{call tree}
\details{Results can be display nicely using either plot or print.}
\seealso{\code{\link{print.call.tree}}, \code{\link{plot.call.tree}}}
\examples{}
\keyword{debugging}
