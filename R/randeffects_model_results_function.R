#' Merge summary statistics from SAS GLIMMIX models to compare random effects models for GTMNERR
#' 
#' @param covariance.parms.test
#' @param convergence.status
#' @param covariance.parms.estimates
#' @param conditional.fit.statistics
#' @param fit.statistics
#' 
#' @export

randeffects_model_results_function <- function(
	covariance.parms.test, 
	convergence.status, 
	covariance.parms.estimates, 
	conditional.fit.statistics, 
	fit.statistics,
	parms.estimates,
	fit.statistic="AIC"
)
{	
	conditional.fit.statistics %<>% 
		convertShorttoLong %>%
		.[, -(2:3)]
	fit.statistics %<>% 
		convertShorttoLong
	if (fit.statistic == "AIC") {
		fit.statistics %<>%
			dplyr::select(
				modelVars,    
				`-2 Log Likelihood`,
				`AIC  (smaller is better)`
			)
	}
	if (fit.statistic == "BIC") {
		fit.statistics %<>%
			dplyr::select(
				modelVars,    
				`-2 Log Likelihood`,
				`BIC  (smaller is better)`
			)
	}
	parms.estimates %<>% 
		filter(!is.na(StdErr)) %>%
		reshape2::dcast(modelVars~Effect, value.var="Estimate")
	modelresults <- merge(parms.estimates, covariance.parms.test,
		by="modelVars", all=T) %>%
		merge(convergence.status, all=T) %>%
		merge(covariance.parms.estimates, all=T) %>%
		merge(conditional.fit.statistics, all=T) %>%
		merge(filter(fit.statistics), all=T) %>%
		as.data.table %>%
		.[modelVars == "YEARPLANTID", 			
			modelVars := "Year, Plant ID (Intercept)"] %>%
		.[modelVars == "LOCATIONWITHINYEAR", 	
			modelVars := "Location X Year"] %>%
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
			modelVars := "Location X Year, Plant ID (Intercept)"] %>%
		.[modelVars == "YEAR", 					
			modelVars := "Year"] %>%
		.[modelVars == "ISLAND", 					
			modelVars := "Island"] %>%
		.[modelVars == "NETWORKISLAND", 					
			modelVars := "Network X Island"] %>%
		.[modelVars == "NETWORK", 					
			modelVars := "Network"] %>%
		.[modelVars == "NETWORKHABITATTYPE", 					
			modelVars := "Network X Habitat Type"]
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
	#	setnames("tValue", 					"t-Value") %>%
	#	setnames("Probt", 					"Prob(t)") %>%
		setnames("LowerLRCL", 				"Lower LR CL") %>%
		setnames("probLRLower", 			"Prob(Lower LR)") %>%
		setnames("UpperLRCL", 				"Upper LR CL") %>%
		setnames("probLRUpper", 			"Prob(Upper LR)") %>%
		setnames("Pearson Chi-Square / DF",	"Pearson Chi-Square/DF") %>%
		setnames("-2 Log Likelihood",		"-2 Log Lik")
	if (fit.statistic == "AIC") {
		modelresults %<>% setnames("AIC  (smaller is better)","AIC")
	}
	if (fit.statistic == "BIC") {
		modelresults %<>% setnames("BIC  (smaller is better)","BIC")
	}
	return(modelresults)
}
