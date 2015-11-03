#' Return table comparing conditional AIC (cAIC) values for multiple models produced using SAS GLIMMIX.
#' @param y

cAIC_function <- function(y) {
   y %<>%
   mutate(
   	cAIC = `-2 log L(FruitPres_t | r. effects)` + 
   		2*(`Upper Bound, Number of Parameters` + ColumnsZ)
   )
	`min(cAIC)` = min(y$cAIC)
	y %<>%
		mutate(
			`delta cAIC` = cAIC - `min(cAIC)`,
			`Model Lik` = exp((-1/2)*`delta cAIC`)
		)
	sum.L = sum(y$`Model Lik`)
	y %<>% mutate(`Prob(Model)` = `Model Lik`/sum.L)
	y$`Prob(Model)` %<>% round(digits=2)
	y$`Model Lik` %<>% round(digits=2)
	y %<>% dplyr::select(-c(
		cAIC, 
		`Model Lik`, 
		`-2 log L(FruitPres_t | r. effects)`, 
		ColumnsZ
	)) %>%
 	arrange(`delta cAIC`)
	return(y)
}
