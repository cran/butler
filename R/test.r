# Test.
# Run all tests in given file and return results
#
# Displays status for each test as run and then reports particular errors
# ...F..E.. 8/10 successful, then report errors
#
# parse file, & for each expression:
#  * if contains assert, increment test count, then try to run test
#		store result + warning + expression in results[[i]]
#		cat ., E or W as appropriate
#  * eval other lines as usual
#  * output results to list failures 
# @arguments path to file
# @keyword debugging
test <- function(path, print=TRUE) {	
	expressions <- parse(path)

	results <- lapply(expressions, expression.test, print)
	if (print) cat("\n")
	
	sapply(results, print.test.result)
	invisible(results)
}

# Print test result
# 
# @keyword debugging
print.test.result <- function(x, ...) {
	if (is.assertError(x$error)) {
		cat(paste("\nAssert failure in ", x$test, ":\n", x$message, "\n", sep=""))
	} else if (is.error(x$error)) {
		cat(paste("\nError in ", x$test, ":\n", x$message, "\n", sep=""))
	}
}

# Is this expression a test?
# Determine whether expression is a test and should be run when "test"-ing a file
#
# An expression is a test if it is a function whose name begins with test.
# @arguments expression to test
# @keyword debugging
is.test <- function(expression) {
	if (length(expression) < 2) return(FALSE)
	text <- as.character(expression[[2]])
	isTRUE(grep("^(test)", text) >= 0)	
}

# Expression test.
# Run test and display status
#
# @arguments expression to test
# @arguments print results?
# @keyword debugging
expression.test <- function(expression, print=TRUE) {
	if (!is.test(expression)) return()
	result <- test.result(expression)
	if (!print) return(result)

	if (is.assertError(result$error)) {
		cat("F")
	} else if (is.error(result$error)) {
		cat("E")
	} else {
		cat(".")
	}
	result
}

# Eval expression and return list with errors, warnings and the deparsed expression
# @keyword debugging
test.result <- function(expression) {
	error <- get_error(eval(expression, list())())
	list(
		test = as.character(expression[[2]]),
		error = error, #, enclos=NULL,
		message = error$message
	)
}