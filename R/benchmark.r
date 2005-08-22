# Benchmark
# Compare the performance of different functions
#
# @arguments functions to compare, make sure to name them (see example)
# @arguments number of reps of each function to run
# @keyword debugging
#
#X mean1 = function() {x <- rnorm(1000); mean(x);}
#X mean2 = function() {x <- rnorm(1000); sum(x) / length(x);}
#X mean3 = function() {x <- rnorm(1000); total=0;for(i in 1:length(x)) {total <- total + x[i]}; total/length(x);}
#X mt = benchmark(mean=mean1,sum=mean2,loop=mean3, reps=1000)
#X print(mt)
benchmark <- function(..., reps = 10) {
	args <- list(...)

	#Preconditions
	assert(is.positive.integer(reps), "Number of repetitions (reps) must be a positive integer")	
	assert(length(list) > 0, "Supply functions to test in ...")
	assert(all(as.logical(lapply(args, is.function))),"Arguments to ... expected to be functions")
	assert(!is.null(names(args)), "Functions should be named")

	times <- list()

	for (i in 1:length(args)) {
		functionName <- names(args)[i]
		times[[functionName]] <- system.time(for(j in 1:reps){args[[i]]()}, gcFirst=TRUE)[1:3]
	}
	
	times <- as.data.frame(times)[1:3]
	attr(times, "reps") <- reps
	rownames(times) <- c("system", "user", "total")
	class(times) <- c("benchmark", class(times))

	times
}

# Print benchmark
# Print nicely formatted benchmark results
#
# @arguments benchmark object to display
# @arguments time to display (one of system, user or total)
# @arguments required to match generic
# @keyword debugging
print.benchmark <- function(x, type="total", ...) {
	assert(type %in% c("system", "user", "total"), "Type must be one of: system, user, total")

	reps <- attr(x, "reps")

	times <- as.numeric(x[type,])
	comp <- outer(times, times, "/")
	dimnames(comp) <- list(colnames(x), colnames(x))
	
	cat("Repetitions: ", reps, "\n")
	cat("Timing: ", type, "\n")
	
	display <- matrix(c(
		"", colnames(x),
		"Time (s)", format(times / reps, width=5), 
		"Frequency (per s)", format(reps / times, digits=1)
	), nrow=3, byrow=TRUE)
	dimnames(display) <- list(rep("",dim(display)[1]),rep("",dim(display)[2]))
	print(display, quote=FALSE, right=TRUE)

	cat("\nRelative speeds\n")
	print(comp, digits=2)
}

# Benchmark2
# An extension to benchmark which allows you to specify a (minimum) time for the functions to run
#
# @arguments functions to compare, make sure to name them (see example)
# @arguments (minimum) time, in seconds, to run for
# @arguments number of reps to run between checking time
# @keyword debugging
benchmark2 <- function(..., time = 3, reps = 10) {
	args <- list(...)

	#Preconditions
	assert(mode(reps) == "numeric" && reps > 0 && floor(reps) == reps, "Number of repetitions (reps) must be a positive integer")	
	assert(mode(time) == "numeric" && time > 0, "Amount of time (time) must be a positive number")	
	assert(length(list) > 0, "Supply functions to test in ...")
	assert(all(as.logical(lapply(args, is.function))),"Arguments to ... expected to be functions")
	assert(!is.null(names(args)), "Functions should be named")

	times <- list()
	class(times) <- c("benchmark")
	cur.time <- 0
	totalreps <- 0
	
	while(cur.time < time) {
		for(i in 1:length(args)) {
			functionName <- names(args)[i]
			timing <- system.time(for(j in 1:reps){args[[i]]()}, gcFirst = TRUE)
			if (is.null(times[[functionName]])) {
				times[[functionName]] <- timing
			} else {
				times[[functionName]] <- timing + times[[functionName]]
			}
			totalreps <- totalreps + reps
			cur.time <- cur.time + timing[3]
			print(cur.time)
		}
		 
	}

	attr(times, "reps") <- totalreps
	times
}
