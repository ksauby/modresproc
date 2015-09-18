#' Create table with G-side effects test results of multiple SAS GLIMMIX models
#' 
#' @param modelresults

randeffects_G_side_effects_test_function <- function(modelresults) {
	modelresults %>%
		filter(`Positive Definite G-Matrix?`=="Yes") %>%
		dplyr::select(
			`Random Effects`, 
			Label, 
			DF, 
			Objective, 
			`Chi-Squared`, 
			`Prob(Chi-Squared)`
		) %>%
		group_by(`Random Effects`) %>%
		summarise(
			DF=DF[1],
			Objective=Objective[1],
			`Chi-Squared`=`Chi-Squared`[1],
			`Prob(Chi-Squared)`=`Prob(Chi-Squared)`[1]
		) %>%
		as.data.frame
}
