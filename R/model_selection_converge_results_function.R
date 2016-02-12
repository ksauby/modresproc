#' Model selection converge results function
#' 
#' @param model_selection_table
#' @param select_list
#' 
#' @export

	
model_selection_converge_results_function <- function(model_selection_table, select_list) {
	 model_selection_table %>%
		mutate(`Positive Definite G Matrix?` = ifelse(pdG==1, "Yes", "No")) %>%
		select(-pdG)
}

