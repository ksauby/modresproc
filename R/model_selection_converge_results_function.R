#' Model selection converge results function
#' 
#' @param model_selection_table
#' @param select_list
#' @export

model_selection_converge_results_function <- function(model_selection_table, select_list) {
	model_selection_converge_status <- model_selection_table %>% 
		names_processing_function %>%
		.[, select_list] %>%
		dplyr::mutate(	`Positive Definite G Matrix?` = ifelse(pdG==1, "Yes", "No")) %>%
		dplyr::select(-pdG)
	model_selection_converge_status[which(model_selection_converge_status$`Insect x Weather`=="NA"), ]$`Insect x Weather` <- ""
	model_selection_converge_status[which(model_selection_converge_status$`P x T`=="NA"), ]$`P x T` <- ""
	return(model_selection_converge_status)
}
