#' Process model results for a model selection table using data from SAS GLIMMIX models
#' 
#' @param y
#' @param select_list
#' 
#' @export

model_results_processing_function <- function(y, select_list) {
	#y[, replace_list][y[, replace_list] > 0] <- "X"
	#y[, replace_list][y[, replace_list] == 0] <- "."
	# change column names
	if ("CA" %in% names(y)) {setnames(y, "CA", "Invasive Moth")}
	if ("CH" %in% names(y)) {setnames(y, "CH", "Native Bug")}
	if ("DA" %in% names(y)) {setnames(y, "DA", "Native Scale")}
	if ("ME" %in% names(y)) {setnames(y, "ME", "Native Moth")}
	if ("Ln_Size_min_st" %in% names(y)) 
		{setnames(y, "Ln_Size_min_st", "Ln(Minimum Plant Size), standardized")}
	#y = y[, select_list]
	return(y)			
}