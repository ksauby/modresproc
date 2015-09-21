#' Return table comparing SAS GLIMMIX random effects models.
#' 
#' @param modelresults

randeffects_model_selection_function <- function(modelresults){
	x = modelresults %>%
		filter(`Positive Definite G-Matrix?`=="Yes") %>%
		dplyr::select(-Label, -Objective) %>%
		dplyr::select(-(Note:Subject)) %>%
		group_by(`Random Effects`) %>%
		filter(row_number(DF)==1) %>%
		as.data.frame %>%
		arrange(AIC)
		x %<>% AIC_function		
	return(x)
}
