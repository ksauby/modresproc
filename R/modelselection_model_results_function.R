#' Format parameters and their estimates for a model selection table using data from SAS GLIMMIX
#' 
#' @param models.dimensions
#' @param convergence.status
#' @param parameter.estimates
#' @param conditional.fit.statistics
#' @param select_list
#' 
#' @export

model_selection_results_function <- function(
	model_selection_table,
	select_list,
	round.n=2
)
{
  	model_selection_table %<>% filter(pdG==1) %>% select(-pdG)
	model_selection_table %<>% cAIC_function
	model_selection_table <- model_selection_table[, select_list]
	model_selection_table %<>% names_processing_function
	# take modelVars from top of list
	topmodels = model_selection_table[which(model_selection_table$`delta cAIC` <= 2), "modelVars"]
	top.estimates = 
	parameter.estimates[which(parameter.estimates$modelVars %in% 
		topmodels),]	
	# top.estimates = 
	#	parameter.estimates[which(parameter.estimates$modelVars %in% topmodels$modelVars),]	
	# modelVars = model_selection_table[1:nmodels, "modelVars"]
	
	
	# how do I convert ln standardized back to regular numbers?
	for (i in 1:length(topmodels)) {
		Data = top.estimates[which(top.estimates$modelVars == model_selection_table$modelVars[i]), ]
		model_selection_table[i  ] %<>% constructConfInt
	
	#model_selection_table %<>% dplyr::select(-modelVars)
	model_selection_table[, "P x T"][model_selection_table[, "P x T"] == "NA"] <- ""
	model_selection_table[, "Insect x Weather"][model_selection_table[, "Insect x Weather"] == "NA"] <- ""
	model_selection_table %<>% select(-modelVars)
	return(model_selection_table)
}
