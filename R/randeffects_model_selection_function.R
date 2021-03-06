#' Return table comparing SAS GLIMMIX random effects models.
#' 
#' @description DOES NOT LOAD RIGHT
#' @param modelresults
#' 
#' @export

randeffects_model_selection_function <- function(modelresults){
	x = modelresults %>%
		dplyr::filter(`Positive Definite G-Matrix?`=="Yes") %>%
		dplyr::select(-Label, -Objective) %>%
		dplyr::select(-(Note:`Positive Definite G-Matrix?`)) %>%
		dplyr::group_by(`Random Effects`) %>%
		dplyr::filter(row_number(DF)==1) %>%
		as.data.frame %>%
		dplyr::arrange(AIC)
		x %<>% AIC_function		
	return(x)
}
