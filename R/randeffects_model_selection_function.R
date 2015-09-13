randeffects_model_selection_function <- function(modelresults){
	x = modelresults %>%
		filter(`Positive Definite G-Matrix?`=="Yes") %>%
		dplyr::select(
			`Random Effects`, 
			DF, 
			`Pearson Chi-Square/DF`, 
			`Chi-Squared`, 
			`Prob(Chi-Squared)`, 
			`-2 Log Lik`, 
			AIC
		) %>%
		group_by(`Random Effects`) %>%
		summarise(
			DF							= DF[1],
			`Chi-Squared`				= `Chi-Squared`[1],
			`Prob(Chi-Squared)`			= `Prob(Chi-Squared)`[1], 
			`Pearson Chi-Square/DF` 	= `Pearson Chi-Square/DF`[1],
			`-2 Log Lik` 		= `-2 Log Lik`[1],
			AIC 						= AIC[1]
		) %>%
		as.data.frame %>% 
		arrange(AIC)
		x %<>% AIC_function		
	return(x)
}
