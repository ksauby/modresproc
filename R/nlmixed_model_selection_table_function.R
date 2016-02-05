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

nlmixed_table_function <- function(models.dimensions, convergence.status, parameter.estimates, round.n=2) {
	models.dimensions %<>%
		dcast(modelVars~Descr, value.var="Value") %>%
		dplyr::select(modelVars, Parameters)
	parameter.estimates$Estimate %<>% round(., round.n)	
	parameter.estimates$Lower %<>% round(., round.n)	
	parameter.estimates$Upper %<>% round(., round.n)	
	parameter.estimates %<>% mutate(Estimate.CF = paste(
		Estimate,
		" [",
		Lower,
		", ",
		Upper,
		"]",
		sep=""
	))
	convergence.status %<>% 
		dplyr::select(modelVars, Status) %>%
		filter(Status==0)
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
		# change effects to columns
		dcast(modelVars + ModelType ~ Parameter, value.var="Estimate.CF") %>%
		merge(models.dimensions) %>%
		merge(convergence.status) %>%
		dplyr::select(-Status)
	return(y)
}

#' @title Replace Parameter Names for NLMIXED Output
#' 
#' @param y Parameter estimates output
#' 
#' @export
replaceNLMIXEDnames <- function(y) {
	y[y$Parameter == "a0" | y$Parameter == "b0", ]$Parameter <- "Intercept"	
	y[y$Parameter == "a1" | y$Parameter == "b1", ]$Parameter <- "C_t"
	y[y$Parameter == "a2" | y$Parameter == "b2", ]$Parameter <- "Native Bug Presence"
	y[y$Parameter == "a3" | y$Parameter == "b4", ]$Parameter <- "Mean Max. Temp, Spring/Summer"
	y[y$Parameter == "a4", ]$Parameter 			<- "P1, Spring/Summer"
	y[y$Parameter == "b3", ]$Parameter 			<- "Native Moth Presence"
	y[y$Parameter == "b5", ]$Parameter 			<- "Mean Degree Day, Spring/Summer"
	y[y$Parameter == "b6", ]$Parameter 			<- "P1, Fall/Winter"
	return(y)
}

#' @title Add Vital Rate Column
#' 
#' @param y Parameter estimates output
#' 
#' @export
addVitalRateColumn <- function(y) {
 y$`Vital Rate` <- "Fruit Abundance"	
 y$`Vital Rate` <- replace(
 	y$`Vital Rate`,
 	grep("a", y$Parameter),
 	"Fruit Absence"
 )
 return(y)
}
