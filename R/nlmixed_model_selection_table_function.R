#' @title Build table of candidate models
#' 
#' @param covariance.parameter.estimates
#' @param models.dimensions
#' @param convergence.status
#' @param parameter.estimates
#' @param conditional.fit.statistics
#' 
#' @importFrom reshape2 dcast
#' @importFrom dplyr mutate
#' @export

constructNLMIXEDConfInt

nlmixed_table_function <- function(models.dimensions, convergence.status, parameter.estimates) {
	models.dimensions %<>%
		dcast(modelVars~Descr, value.var="Value") %>%
		dplyr::select(modelVars, Parameters)
	y = parameter.estimates %>%
		mutate(
			ModelType = "Count Model",
			ModelType = replace(
				ModelType, 
				grep("a", parameter.estimates$Parameter), 
				"Zero Model"
			),
			ModelType = replace(
				ModelType, 
				grep("sigma2_Loc", parameter.estimates$Parameter), 
				"Zero Model"
			),
			ModelType = replace(
				ModelType, 
				grep("sigma2_Yr", parameter.estimates$Parameter), 
				"Count Model"
			)
		) %>%
		# change effects to columns
		dcast(modelVars ~ Parameter, value.var="Estimate")
		
		z = as.data.frame(
			NA, 
			nrow=dim(parameter.estimates)[1] / 
				length(unique(parameter.estimates$modelVars)),
			ncol=1
			)
		for (i in 1:length(unique(y$modelVars))) {
			parm.est <- parameter.estimates[which(parameter.estimates$modelVars == unique(y$modelVars)[i]), ]
			for (j in 1:dim(y)[1]) {
				z %<>% constructNLMIXEDConfInt(parm.est = parm.est)
			}
		}
		
		
		
		mutate(
			Parameter = replace(
				Parameter, 
				which(Parameter=="a0" | Parameter=="b0"), 
				"Intercept"),
			Parameter = replace(
				Parameter, 
				which(Parameter=="a1" | Parameter=="b1"), 
				"C_t"),
			Parameter = replace(
				Parameter, 
				which(Parameter=="a2" | Parameter=="b2"), 
				"Native Bug Presence"),
			Parameter = replace(
				Parameter, 
				which(Parameter=="a3" | Parameter=="b3"), 
				"Native Scale Presence"),
			Parameter = replace(
				Parameter, 
				which(Parameter=="a4" | Parameter=="b4"), 
				"Native Moth Presence"),
			Parameter = replace(
				Parameter, 
				which(Parameter=="a5" | Parameter=="b5"), 
				"T1, F/W"),
			Parameter = replace(
				Parameter, 
				which(Parameter=="a6" | Parameter=="b6"), 
				"E1, S/S"),
			Parameter = replace(
				Parameter, 
				which(Parameter=="a7" | Parameter=="b7"), 
				"F, S/S"),
			Parameter = replace(
				Parameter, 
				which(Parameter=="a8" | Parameter=="b8"), 
				"P1, S/S"),
			Parameter = replace(
				Parameter, 
				which(Parameter=="a9" | Parameter=="b9"), 
				"P2, S/S"),
			Parameter = replace(
				Parameter, 
				which(Parameter=="a10" | Parameter=="b10"), 
				"P1 F/W"),
			Parameter = replace(
				Parameter, 
				which(Parameter=="a11" | Parameter=="b11"), 
				"P2 F/W")
		) %>%
		
		
		# fit statistics
		merge(models.dimensions) %>%
		merge(convergence.status %>% dplyr::select(modelVars, Status))
	return(y)
}
