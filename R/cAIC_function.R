#' Return table comparing conditional AIC (cAIC) values for multiple models produced using SAS GLIMMIX.
#' 
#' @param y
#' 
#' @export

cAIC_function <- function(y) {
	y %<>% as.data.table
	if ("-2 log L(FruitPres_t | r. effects)" %in% names(y)) 
		{setnames(y, "-2 log L(FruitPres_t | r. effects)", "-2 LogLik")}
	if ("-2 log L(Fruit_t | r. effects)" %in% names(y)) 
		{setnames(y, "-2 log L(Fruit_t | r. effects)", "-2 LogLik")}
	if ("-2 log L(y | r. effects)" %in% names(y)) 
		{setnames(y, "-2 log L(y | r. effects)", "-2 LogLik")}
	y %<>% as.data.frame %>%
   dplyr::mutate(
   	cAIC = `-2 LogLik` + 
   		2*(`Columns in X` + `Columns in Z`)
   )
	`min(cAIC)` = min(y$cAIC)
	y %<>%
		mutate(
			`delta cAIC` = cAIC - `min(cAIC)`,
			`Model Lik` = exp((-1/2)*`delta cAIC`),
			`Number of Parameters` = paste(`Columns in X`, "+", `Columns in Z`, "=", `Columns in X` + `Columns in Z`)
		)
	sum.L = sum(y$`Model Lik`)
	y %<>% mutate(`Prob(Model)` = `Model Lik`/sum.L)
	y$`Prob(Model)` %<>% round(digits=2)
	y$`Model Lik` %<>% round(digits=2)
	y %<>% dplyr::select(-c(
		cAIC, 
		`Model Lik`, 
		`-2 LogLik`, 
		`Columns in Z`
	)) %>%
 	arrange(`delta cAIC`)
	return(y)
}
