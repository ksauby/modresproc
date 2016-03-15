#' Return table comparing AIC values for multiple models produced using SAS GLIMMIX.
#' 
#' @param y
#' 
#' @export

AIC_function <- function(y) {
	`min(AIC)` = min(y$AIC, na.rm=T)
	y %<>%
		mutate(
			`delta AIC` = AIC - `min(AIC)`,
			`Model Lik` = exp((-1/2)*`delta AIC`)
		) %>%
		arrange(desc(`Model Lik`))
	sum.L = sum(y$`Model Lik`, na.rm=T)
	y %<>% mutate(`Prob(Model)` = `Model Lik`/sum.L)
	y$`Prob(Model)` %<>% round(digits=2)
	y$`Model Lik` %<>% round(digits=2)
	return(y)
}
