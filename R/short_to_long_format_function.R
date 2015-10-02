#' Change from long to short format, using "Value"
#' @param x

short_to_long_format_function <- function(x) {
	x %<>% dcast(modelVars~Descr, value.var="Value")
}