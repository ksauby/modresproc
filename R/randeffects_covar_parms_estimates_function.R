randeffects_covar_parms_estimates_function <- function(modelresults){
	modelresults %>%
		filter(`Positive Definite G-Matrix?`=="Yes") %>%
		dplyr::select(
			`Random Effects`, 
			`Covariance Parameter`, 
			Estimate, 
			`Standard Error`, 
			`Z-Value`, 
			`Prob(Z)`, 
			`Lower Wald CL`, 
			`Upper Wald CL`
		) %>%
		as.data.frame
}
