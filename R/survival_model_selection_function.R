#' Create table of survival model results
#' 
#' @param convergence.status
#' @param parameter.estimates
#' @param fit.statistics
#' @param replace_list
#' @param select_list

survival_model_selection_function <- function(
	convergence.status, 
	parameter.estimates, 
	fit.statistics, 
	replace_list, 
	select_list
)
{
	y <- survival_model_results_function(
		convergence.status, 
		parameter.estimates, 
		fit.statistics, 
	)
	Logit = y %>% 
		filter(`Link Function`=="Logit") %>%
		AIC_function %>%
		model_results_processing_function(select_list)
	CLogLog = y %>% 
		filter(`Link Function`=="C-Log-Log") %>%
		AIC_function %>%
		model_results_processing_function(select_list)
	return(list(Logit,CLogLog))
}
