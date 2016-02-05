#' Build table of candidate models
#' 
#' @param covariance.parameter.estimates
#' @param models.dimensions
#' @param convergence.status
#' @param parameter.estimates
#' @param conditional.fit.statistics
#' 
#' @importFrom reshape2 dcast
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace
#' @export

model_selection_table_function <- function(covariance.parameter.estimates, models.dimensions, convergence.status, parameter.estimates, 
	conditional.fit.statistics) {
	# to verify that all models have the same covariance parameters included
	if ("Subject" %in% names(covariance.parameter.estimates)) {
		covariance.parameter.estimates %<>% 
			mutate(CovParm = paste(CovParm, Subject)) %>%
			dcast(modelVars~CovParm, value.var="Estimate")
	} else {
		covariance.parameter.estimates %<>% 
			mutate(CovParm = paste(CovParm)) %>%
			dcast(modelVars~CovParm, value.var="Estimate")
	}
	names(covariance.parameter.estimates) <- str_replace_all(names(covariance.parameter.estimates), fixed(" "), "")
	conditional.fit.statistics %<>% 
		short_to_long_format_function %>%
		.[, -3]
	models.dimensions %<>%
		short_to_long_format_function %>%
		select(modelVars, `Columns in X`, starts_with("Columns in Z"))
	parameter.estimates %<>% constructConfInt
	if ("CH_t_1" %in% parameter.estimates$Effect) {
		y = parameter.estimates %>%
			filter(CH_t_1!=0)
	}
	if ("DA_t_1" %in% y$Effect) {
		y %<>% filter(DA_t_1!=0)
	}
	if ("ME_t_1" %in% y$Effect) {
		y %<>% filter(ME_t_1!=0)
	}
	if ("NatInsect_t_1" %in% y$Effect) {
		y %<>% filter(NatInsect_t_1!=0)	
	}
	y %<>% dcast(modelVars~Effect, value.var="Estimate.CF") %>%
		# fit statistics
		merge(conditional.fit.statistics) %>%
		merge(models.dimensions) %>%
		merge(covariance.parameter.estimates) %>%
		model_dimensions_name_processing_function %>%
		merge(convergence.status %>% dplyr::select(modelVars, pdG))
	return(y)
}

