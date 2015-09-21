#' Merge summary statistics from SAS GLIMMIX models to compare random effects models.
#' 
#' @param covariance.parms.test
#' @param convergence.status
#' @param covariance.parms.estimates
#' @param conditional.fit.statistics
#' @param fit.statistics

randeffects_model_results_function <- function(
	covariance.parms.test, 
	convergence.status, 
	covariance.parms.estimates, 
	conditional.fit.statistics, 
	fit.statistics
)
{
	conditional.fit.statistics %<>% 
		dcast(modelVars~Descr, value.var="Value") %>%
		.[, -(2:3)]
	fit.statistics %<>% 
		dcast(modelVars~Descr, value.var="Value") %>%
		.[, -(4:7)]
	modelresults = merge(covariance.parms.test, convergence.status,
		by="modelVars") %>%
		merge(covariance.parms.estimates) %>%
		merge(conditional.fit.statistics) %>%
		merge(filter(fit.statistics)) %>%
		as.data.table %>%
		.[modelVars == "YEARPLANTID", 			
			modelVars := "Year, Plant ID (Intercept)"] %>%
		.[modelVars == "LOCATIONWITHINYEAR", 	
			modelVars := "Location (Year)"] %>%
		.[modelVars == "PLANTID", 				
			modelVars := "Plant ID (Intercept)"] %>%
		.[modelVars == "LOCATIONYEARPLANTID", 	
			modelVars := "Location, Year, Plant ID (Intercept)"] %>%
		.[modelVars == "LOCATIONYEAR", 			
			modelVars := "Location, Year"] %>%
		.[modelVars == "LOCATION", 				
			modelVars := "Location"] %>%
		.[modelVars == "LOCATIONPLANTID", 		
			modelVars := "Location, Plant ID (Intercept)"] %>%
		.[modelVars == "LOCATIONWYEARPLANTID", 	
			modelVars := "Location (Year), Plant ID (Intercept)"] %>%
		.[modelVars == "YEAR", 					
			modelVars := "Year"]
	modelresults$pdG %<>% as.character
	modelresults %<>% as.data.table %>%
		.[pdG == 0, pdG := "No"] %>%
		.[pdG == 1, pdG := "Yes"] %>%
		setnames("modelVars", 				"Random Effects") %>%
		setnames("pdG", 					"Positive Definite G-Matrix?") %>%
		setnames("ChiSq", 					"Chi-Squared") %>%
		setnames("ProbChiSq", 				"Prob(Chi-Squared)") %>%
		setnames("CovParm", 				"Covariance Parameter") %>%
		setnames("StdErr", 					"Standard Error") %>%
		setnames("ZValue", 					"Z-Value") %>%
		setnames("ProbZ", 					"Prob(Z)") %>%
		setnames("LowerWaldCL", 			"Lower Wald CL") %>%
		setnames("UpperWaldCL", 			"Upper Wald CL") %>%
		setnames("AIC  (smaller is better)","AIC") %>%
		setnames("Pearson Chi-Square / DF",	"Pearson Chi-Square/DF") %>%
		setnames("-2 Log Likelihood",		"-2 Log Lik")
	return(modelresults)
}
