# Assert
# Basic assert function that powers all others.
#
# @arguments condition to evaluation for truth
# @arguments message to display if condition is false
# @arguments should warnings be displayed 
# @keyword debugging
assert <- function(condition, msg = "") {
	if (!isTRUE(condition)) {
		if (length(msg) == 1 && msg == "") msg <- paste(deparse(substitute(condition)), "not true")
		throw_assert_error(msg)
	}
}

# is.positive.integer
# Converience function for determining if x is a positive integer
#
# @arguments object to test
# @keyword debugging
is.positive.integer <- function(x) {
	mode(x) == "numeric" && x > 0 && floor(x) == x
}

# Assert warning
# Assert that a warning occured
# @keyword debugging
assert.warning <- function(f, warning = "", msg = "") {
	w <- get_warning(f)
	assert.warning_error(w, warning, msg)
}

# Assert error
# Assert an error occured
# @keyword debugging
assert.error <- function(f, warning = "", msg = "") {
	w <- get_error(f)
	assert.warning_error(w, warning, msg)
}

# Assert warning or error
# Assert a warning of error occured
# @keyword debugging
assert.warning_error <- function(w, warning = "", msg = "") {
	expected <- !(is.na(w) || ((warning != "") && (w$message != warning)))
	if (!expected) {
		if (missing(warning)) {
			msg <- paste(deparse(substitute(condition)), "did not raise a warning")
		} else { 
			msg <- paste(deparse(substitute(condition)), "raised", w$message, "not", warning)
		}
		warning(msg)
	}
	expected
}

# Throw assert error
# Equivalent of stop(msg) but classed so that other methods can recognise it
# @arguments error message to display
# @keyword debugging
throw_assert_error <- function(msg) {
	err <- simpleError(msg)
	class(err) <- c("assertError", class(err))
	stop(err)
}

# Get warning
# Collect any warnings created when running function
# @keyword debugging
get_warning <- function(x) { err <- NA	
	tryCatch(force(x), warning = function(e){err <<- e})
	err
}

# Get error
# Collect any errors created when running function
# @keyword debugging
get_error <- function(x) { err <- NA	
	tryCatch(force(x), error = function(e){err <<- e})
	err
}

# Assert is S3 class
# Assert argument is of given S3 class
# @keyword debugging
assert.isS3class <- function(obj, class, msg = "") {
	if (missing(msg)) msg <- paste(deparse(substitute(obj)), "not S3 class", class)
	assert(class %in% class(obj), msg)	
}

# Is all equal
# Are x and y equal
# @keyword debugging
is.all.equal <- function(x,y) isTRUE(all.equal(x,y))

# Assert equal
# Assert expected and actual are equal
# @keyword debugging
assert.equal <- function(expected, actual, msg = "") {
	if (msg == "") msg <- paste(deparse(substitute(expected)), "does not equal", deparse(substitute(actual)), "because\n:", paste(all.equal(expected, actual), collapse="\n"))
	assert(is.all.equal(expected, actual), msg)	
}
# Is assert error?
# Is x an assertion error?
# @keyword debugging
is.assertError <- function(x) "assertError" %in% class(x)

# Is error?
# Is x an assertion error?
# @keyword debugging
is.error <- function(x) "error" %in% class(x)

