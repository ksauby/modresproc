#' Paste PCA axis data together
#' 
#' @param y
#' 
#' @export

processWeatherVariables <- function(y) {
	if ("T1" %in% names(y)) {
		y %<>%
		group_by(modelVars) %>%
		mutate(
			T1 = ifelse(
				!is.na(T1),
				paste("T1 = ", T1, sep=""),
				T1
			)
		) %>%
		ungroup %>%
		as.data.frame
	}
	if ("T2" %in% names(y)) {
		y %<>%
		group_by(modelVars) %>%
		mutate(
			T2 = ifelse(
				!is.na(T2),
				paste("T2 = ", T2, sep=""),
				T2
			)
		) %>%
   		mutate(
   			T1 = ifelse(
				!is.na(T1),
				paste(T1, T2, sep=", "),
				T1
			)
   		) %>%
		ungroup %>%
		as.data.frame
	}
	if ("P1" %in% names(y)) {
		y %<>%
		group_by(modelVars) %>%
		mutate(
			P1 = ifelse(
				!is.na(P1),
				paste("P1 = ", P1, sep=""),
				P1
			)
		) %>%
		ungroup %>%
		as.data.frame
	}
	if ("P2" %in% names(y)) {
		y %<>%
		group_by(modelVars) %>%
		mutate(
			P2 = ifelse(
				!is.na(P2),
				paste("P2 = ", P2, sep=""),
				P2
			)
		) %>%
		mutate(
			P1 = ifelse(
				!is.na(P1),
				paste(P1, P2, sep=",\n"),
				P1
			)
			
		) %>%
		ungroup %>%
		as.data.frame
	}
	return(y)
}

#' Build table of candidate models
#' 
#' @param covariance.parameter.estimates
#' @param models.dimensions
#' @param convergence.status
#' @param parameter.estimates
#' @param conditional.fit.statistics
#' 
#' @importFrom reshape2 dcast
#' @importFrom dplyr mutate select filter
#' @export

createModelSelectionTable <- function(covariance.parameter.estimates, models.dimensions, convergence.status, parameter.estimates, 
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
	# what is this code for?
	# names(covariance.parameter.estimates) <- str_replace_all(names(covariance.parameter.estimates), fixed(" "), "")
	conditional.fit.statistics %<>% 
		convertShorttoLong %>%
		.[, -3]
	models.dimensions %<>%
		convertShorttoLong %>%
		select(modelVars, `Columns in X`, starts_with("Columns in Z"))
	y <- parameter.estimates %>% constructConfInt
	if ("CA_t_1" 		%in% y$Effect) {y %<>% filter(CA_t_1!=0)}
	if ("CH_t_1" 		%in% y$Effect) {y %<>% filter(CH_t_1!=0)}
	if ("DA_t_1" 		%in% y$Effect) {y %<>% filter(DA_t_1!=0)}
	if ("ME_t_1" 		%in% y$Effect) {y %<>% filter(ME_t_1!=0)}
	if ("NatInsect_t_1" %in% y$Effect) {
		y %<>% filter(is.na(NatInsect_t_1) | NatInsect_t_1==1)
	}
	y %<>% dcast(modelVars~Effect, value.var="Estimate.CF") %>%
		# fit statistics
		merge(conditional.fit.statistics) %>%
		merge(models.dimensions) %>%
		merge(covariance.parameter.estimates) %>%
		merge(convergence.status %>% select(modelVars, pdG))
	return(y)
}

