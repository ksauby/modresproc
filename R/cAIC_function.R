Return table comparing conditional AIC (cAIC) values for multiple models produced using SAS GLIMMIX.
#' 
#' @param y

cAIC_function <- function(y) {
	`min(cAIC)` = min(y$cAIC)
	y %<>%
		mutate(
			`delta cAIC` = cAIC - `min(cAIC)`,
			`Model Lik` = exp((-1/2)*`delta cAIC`)
		) %>%
		arrange(desc(`Model Lik`))
	sum.L = sum(y$`Model Lik`)
	y %<>% mutate(`Prob(Model)` = `Model Lik`/sum.L)
	y$`Prob(Model)` %<>% round(digits=2)
	y$`Model Lik` %<>% round(digits=2)
	return(y)
}
