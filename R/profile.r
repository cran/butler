# Stop watch
# Profile the performance of function call.
#
# Results can be display nicely using either plot or print.
#
# @seealso \code{\link{print.call.tree}}, \code{\link{plot.call.tree}}
# @arguments function to profile
# @arguments number of times to run
# @arguments interval between samples (in seconds)
# @value call tree
# @keyword debugging
stopwatch <- function(f, reps = 2, interval = 0.02) {
	assert(is.positive.integer(reps), "Repetitions (reps) must be a positive integer");
	assert(is.function(f), "f must be a function");
	
	tmp <- tempfile()
	on.exit(unlink(tmp))
	
	for(i in 1:reps) {
		Rprof(tmp, append=TRUE)
		f()
		Rprof()
	}

	lines <- scan(tmp, what="character", sep="\n")
	clean.lines <- lines[-grep("sample\\.interval=",lines)]
	calls <- sapply(clean.lines, strsplit, split=" ", USE.NAMES = FALSE)
	calls <- sapply(calls, rev)
	
	class(calls) <- "call.tree"
	attr(calls, "interval") <- interval
	attr(calls, "reps") <- reps
	
	calls
} 

# Get calls
# Get all calls at a given level of the call stack
#
# @arguments list of calls
# @arguments level of call stack
# @arguments function name
# @keyword debugging
getCalls <- function(calls, level, value) {
	sapply(calls, function(x) { if (!is.na(x[level]) && x[level] == value) x[(level + 1):length(x)]})
}

# Get first call
# @keyword debugging
getFirstCalls <- function(calls, level) {
	sort(table(sapply(calls, function(x) {x[level]})), decreasing=TRUE)
}

# Get next call
# @keyword debugging
nextCalls <- function(calls, level, value) {
	cur.stack <- getCalls(calls, level, value)
	cur.next.call <- sapply(cur.stack, function(x) {if (!is.null(x)) x[1]})
	cur.next.time <- table(unlist(cur.next.call))
	sort(cur.next.time, decreasing=TRUE)
}

# Print call tree
# Attractively print call tree
# @keyword debugging
print.call.tree <- function(x, startlevel = 3, depth = 8, mintime = 2, ...) {
	assert(is.positive.integer(startlevel), "Start level must be a positive integer");
	assert(is.positive.integer(depth), "Depth must be a positive integer");
	assert(is.positive.integer(mintime), "Minimum time (mintime) level must be a positive integer");
	
	depth <- startlevel + depth
	
	first.x <- getFirstCalls(x,startlevel)
	for(call in names(first.x)) {
		displayCallNode(x, startlevel, call, depth, mintime)
	}
}

# Display call node
# @keyword debugging
displayCallNode <- function(calls, level, value, depth, mintime) {
	next.calls <- nextCalls(calls, level, value)
	if (sum(next.calls) >= mintime) {
		cat(rep("  ", level - 1), value, " (", sum(next.calls),")", "\n", sep="")
		if(sum(next.calls) > 0 && level < depth) {
			for(i in 1:length(next.calls)) {
				displayCallNode(calls, level + 1, names(next.calls)[i], depth, mintime)
			}
		}
	}
}

# Plot call tree
# Attractively plot call tree
# @keyword debugging
plot.call.tree <- function(x, startlevel = 1, depth = 5, mintime = 2, ...) {
	plot(x=0, y=0, ylim=c(0,depth+1), xlim=c(0,100),type="n", xlab="", ylab="", mai=0)
	depth <- startlevel + depth
	
	first.x <- getFirstCalls(x,startlevel)
	for(call in names(first.x)) {
		plotCallNode(x, startlevel, call, 0, 100, mintime, startlevel, depth)
	}
}

# Plot call node
# Recursively function that powers plot.call.tree
# @keyword debugging
plotCallNode <- function(calls, level, value, start, end, mintime, startlevel, depth) {
	if (level > depth) return()
	if (end - start < mintime) return()
	next.calls <- nextCalls(calls, level, value)
	
	displevel <- level - startlevel + 1
	
	if (sum(next.calls)  >= mintime) {
		rect(start,displevel-1,end,displevel)
		text(start, displevel-0.5, pos=4,value, cex=0.8)
	
		breakdown <- start + c(0,cumsum(next.calls) / sum(next.calls)) * (end-start)
		for(i in 1:length(next.calls)) {
			plotCallNode(calls, level+1, names(next.calls)[i], breakdown[i], breakdown[i + 1], mintime, startlevel, depth)
		}
	}
}
