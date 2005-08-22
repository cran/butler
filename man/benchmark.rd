\name{benchmark}
\alias{benchmark}
\title{Benchmark}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Compare the performance of different functions
}
\usage{benchmark(..., reps = 10)}
\arguments{
\item{...}{functions to compare, make sure to name them (see example)}
\item{reps}{number of reps of each function to run}
}

\details{@arguments functions to compare, make sure to name them (see example)
@arguments number of reps of each function to run
@keyword debugging}

\examples{mean1 = function() {x <- rnorm(1000); mean(x);}
mean2 = function() {x <- rnorm(1000); sum(x) / length(x);}
mean3 = function() {x <- rnorm(1000); total=0;for(i in 1:length(x)) {total <- total + x[i]}; total/length(x);}
mt = benchmark(mean=mean1,sum=mean2,loop=mean3, reps=1000)
print(mt)}
\keyword{debugging}
