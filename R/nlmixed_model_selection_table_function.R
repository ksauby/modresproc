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
	y[y$Parameter == "A0" | y$Parameter == "B0", ]$Parameter <- "Intercept"	
	y[y$Parameter == "A1" | y$Parameter == "B1", ]$Parameter <- "C_t"
	y[y$Parameter == "A2" | y$Parameter == "B2", ]$Parameter <- "Native Bug"
	y[y$Parameter == "A3" | y$Parameter == "B3", ]$Parameter <- "Mean Max. Temp (Spring/Summer)"
	if ("B4" %in% y$Parameter) {
		y[y$Parameter == "B4", ]$Parameter 	<- "Mean Degree Day (Spring/Summer)"
	}
	if ("B5" %in% y$Parameter) {
		y[y$Parameter == "B5", ]$Parameter 	<- "P1 (Fall/Winter)"
	}
	return(y)
}

#' @title Replace Parameter Names for NLMIXED Output for GTMNERR O. stricta
#' 
#' @param y Parameter estimates output
#' 
#' @export
replaceNLMIXEDnamesGTMNERR <- function(y) {
	y[y$Parameter == "A0" | y$Parameter == "B0", ]$Parameter <- "Intercept"	
	y[y$Parameter == "A1" | y$Parameter == "B1", ]$Parameter <- "C_t"
	y[y$Parameter == "B2", ]$Parameter <- "Invasive Moth Plant Presence"
	return(y)
}

#' @title Replace Parameter Names for NLMIXED Output for O. stricta
#' 
#' @param y Parameter estimates output
#' 
#' @export
replaceNLMIXEDnamesOstricta <- function(y) {
	y[y$Parameter == "A0" | y$Parameter == "B0", ]$Parameter <- "Intercept"	
	y[y$Parameter == "A1" | y$Parameter == "B1", ]$Parameter <- "C_t"
	y[y$Parameter == "B2", ]$Parameter <- "Invasive Moth"
	y[y$Parameter == "B3", ]$Parameter <- "Native Bug"
	return(y)
}

#' @title Replace Parameter Names for NLMIXED Output
#' 
#' @param y Parameter estimates output
#' 
#' @export
replaceGENMODRowNames <- function(y) {
	if ("Ln_size_max_t_1_st" %in% y$Parameter) {
		y[y$Parameter == "Ln_size_max_t_1_st", ]$Parameter <- "$C_t$"	
	}
	if ("Ln_Size_t_1_st" %in% y$Parameter) {
		y[y$Parameter == "Ln_Size_t_1_st", ]$Parameter <- "$C_t$"	
	}
	if ("CA_t_1" %in% y$Parameter) {
		y[y$Parameter == "CA_t_1", ]$Parameter 		<- "Invasive Moth Presence, t"
	}
	if ("CH_t_1" %in% y$Parameter) {
		y[y$Parameter == "CH_t_1", ]$Parameter 		<- "Native Bug"
	}
	if ("DA_t_1" %in% y$Parameter) {
		y[y$Parameter == "DA_t_1", ]$Parameter 		<- "Native Scale"
	}
	if ("ME_t_1" %in% y$Parameter) {
		y[y$Parameter == "ME_t_1", ]$Parameter 		<- "Native Moth Presence, t"
	}
	if ("T1_FW" %in% y$Parameter) {
		y[y$Parameter == "T1_FW", ]$Parameter 		<- "T1 (Fall/Winter)"
	}
	if ("E1_SS_st" %in% y$Parameter) {
		y[y$Parameter == "E1_SS_st", ]$Parameter 	<- "Mean Maximum Daily Temperature (Spring/Summer)"
	}
	if ("F_SS_st" %in% y$Parameter) {
		y[y$Parameter == "F_SS_st", ]$Parameter 	<- "Mean Degree Day (Spring/Summer)"
	}
	if ("P1_SS" %in% y$Parameter) {
		y[y$Parameter == "P1_SS", ]$Parameter 		<- "P1 (Spring/Summer)"
	}
	if ("P2_SS" %in% y$Parameter) {
		y[y$Parameter == "P2_SS", ]$Parameter 		<- "P2 (Spring/Summer)"
	}
	if ("P1_FW" %in% y$Parameter) {
		y[y$Parameter == "P1_FW", ]$Parameter 		<- "P1 (Fall/Winter)"
	}
	if ("P2_FW" %in% y$Parameter) {
		y[y$Parameter == "P2_FW", ]$Parameter 		<- "P2 (Fall/Winter)"
	}
	if ("Old_Moth_Evidence_t_" %in% y$Parameter) {
		y[y$Parameter == "Old_Moth_Evidence_t_", ]$Parameter <- "Presence of Moth Evidence, t"
	}
	if ("OldMothPlantPres" %in% y$Parameter) {
		y[y$Parameter == "OldMothPlantPres", ]$Parameter <- "Presence of Moth Evidence During the Study"
	}
	if ("CAPlantPres" %in% y$Parameter) {
		y[y$Parameter == "CAPlantPres", ]$Parameter <- "Presence of Invasive Moth During the Study"
	}
	if ("MEPlantPres" %in% y$Parameter) {
		y[y$Parameter == "MEPlantPres", ]$Parameter <- "Presence of Native Moth During the Study"
	}
	return(y)
}