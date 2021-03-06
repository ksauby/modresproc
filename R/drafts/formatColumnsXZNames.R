#' Rename model dimensions
#' 
#' @param y
#' 
#' @export

formatColumnsXZNames <- function(y) {
	if ("Columns in X" %in% names(y)) 
		{setnames(y, "Columns in X", "ColumnsX")}
	if (length(grep("Columns in Z", names(y), fixed=T)) > 0) 
		{setnames(y, names(y)[grep("Columns in Z", names(y), fixed=T)], 
		"ColumnsZ")}
	return(y)
}