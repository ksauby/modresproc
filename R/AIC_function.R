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

#' Return table comparing BIC values for multiple models produced using SAS GLIMMIX.
#' 
#' @param y
#' 
#' @export

BIC_function <- function(y) {
	`min(BIC)` = min(y$BIC, na.rm=T)
	y %<>%
		mutate(
			`delta BIC` = BIC - `min(BIC)`,
			`Model Lik` = exp((-1/2)*`delta BIC`)
		) %>%
		arrange(desc(`Model Lik`))
	sum.L = sum(y$`Model Lik`, na.rm=T)
	y %<>% mutate(`Prob(Model)` = `Model Lik`/sum.L)
	y$`Prob(Model)` %<>% round(digits=2)
	y$`Model Lik` %<>% round(digits=2)
	return(y)
}

#' Clean up AIC Model Selection for ZIP Models
#' @description First order by delta AIC, the model number, and vital rate. Then set the AIC model selection values to "" for the count vital rates.
#' @param y
#' @param round.n Number of digits to round to. Defaults to 2.
#' 
#' @export

cleanAICModelSelectionZIP <- function(y, round.n=2) {
	y %<>% arrange(`delta AIC`, modelVars, desc(`Vital Rate`))
	# replace AIC values
	y[which(y$`Vital Rate`=="Count"), ]$`delta AIC` <- ""
	y[which(y$`Vital Rate`=="Count"), ]$`Model Lik` <- ""
	y[which(y$`Vital Rate`=="Count"), ]$`Prob(Model)` <- ""
	y$`delta AIC` %<>% as.numeric %<>% round(round.n)
	y %<>% select(-c(AIC, Descr))
	return(y)
}


#' Clean up AIC Model Selection for ZIP Models
#' @description First order by delta AIC, the model number, and vital rate. Then set the AIC model selection values to "" for the count vital rates.
#' @param y
#' @param round.n Number of digits to round to. Defaults to 2.
#' 
#' @export

cleanBICModelSelectionZIP <- function(y, round.n=2) {
	y %<>% arrange(`delta BIC`, modelVars, desc(`Vital Rate`))
	# replace BIC values
	y[which(y$`Vital Rate`=="Count"), ]$`delta BIC` <- ""
	y[which(y$`Vital Rate`=="Count"), ]$`Model Lik` <- ""
	y[which(y$`Vital Rate`=="Count"), ]$`Prob(Model)` <- ""
	y$`delta BIC` %<>% as.numeric %<>% round(round.n)
	y %<>% select(-c(BIC, Descr))
	return(y)
}