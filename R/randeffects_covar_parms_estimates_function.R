#' Create table with the covariance parameter estimates from multiple SAS GLIMMIX models
#' 
#' @param modelresults

randeffects_covar_parms_estimates_function <- function(modelresults){
	modelresults %>%
		dplyr::filter(`Positive Definite G-Matrix?`=="Yes") %>%
		dplyr::select(
			`Random Effects`, 
			`Covariance Parameter`,
			Estimate,
			`Standard Error`,
			Alpha,
			`Lower LR CL`,
			`Prob(Lower LR)`,
			`Upper LR CL`,
			`Prob(Upper LR)` 			
		) %>%
		as.data.frame
}
