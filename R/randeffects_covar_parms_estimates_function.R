#' Create table with the covariance parameter estimates from multiple SAS GLIMMIX models DOES NOT LOAD RIGHT
#' 
#' @param modelresults
#' @export

randeffects_covar_parms_estimates_function <- function(modelresults){
	modelresults %>%
		filter(`Positive Definite G-Matrix?`=="Yes") %>%
		select(
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
