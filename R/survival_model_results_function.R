#' Create table of survival model results
#' 
#' @param convergence.status
#' @param parameter.estimates
#' @param fit.statistics
#' @param replace_list
#' @param select_list

survival_model_results_function <- function(
	convergence.status, 
	parameter.estimates, 
	fit.statistics,
	select_list
)
{
	# filter out models that didn't converge
	y = merge(convergence.status, parameter.estimates) %>%
		filter(Reason=="Algorithm converged.") %>%
		# change effects to columns
		dcast(modelVars~Parameter, value.var="Estimate", fun=X_function)
		# fit statistics
	z = fit.statistics %>%
		filter(Criterion=="AIC (smaller is better)" | Criterion=="Scaled Deviance") %>%
		dcast(modelVars~Criterion, value.var="Value") %>%
		setnames("AIC (smaller is better)", "AIC") 
	y %<>% merge(z, by="modelVars") %>% 
		arrange(modelVars) %>%
		mutate(`Link Function` = c(
			rep("Logit", dim(convergence.status)[1]/2), 
			rep("C-Log-Log", dim(convergence.status)[1]/2))
		) %>%
		arrange(`Link Function`, `Scaled Deviance`) %>% 
		dplyr::select(-c(modelVars, Scale))
		# select columns
	y <- y[, select_list]
	y %<>% names_processing_function
	return(y)
}
