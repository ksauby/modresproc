#' Change fit statistics from long to short format, changing cell value to either "X" or "."
#' 
#' @param x
#' 
#' @export

short_to_long_format_X_function <- function(x) {
	x %<>% dcast(modelVars~Effect, value.var="Estimate", fun=X_function)
}