#' Change from long to short format, using "Value"
#' 
#' @param x
#' 
#' @export

convertShorttoLong <- function(x) {
	x %<>% dcast(modelVars~Descr, value.var="Value")
}


#' Change fit statistics from long to short format, changing cell value to either "X" or "."
#' 
#' @param x
#' 
#' @export

convertShorttoLongX <- function(x) {
	x %<>% dcast(modelVars~Effect, value.var="Estimate", fun=X_function)
}