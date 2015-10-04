#' Create table of survival model results
#' 
#' @param convergence.status
#' @param parameter.estimates
#' @param fit.statistics
#' @param replace_list
#' @param select_list

survival_model_results_function_humifusa <- function(
	convergence.status, 
	parameter.estimates, 
	fit.statistics,
	select_list
)
{
	# filter out models that didn't converge
	y = merge(convergence.status, parameter.estimates) %>%
		filter(Reason=="Algorithm converged.") %>%
		# change effects to columns
		dcast(modelVars~Parameter, value.var="Estimate", fun=sum)
		# fit statistics
	z = fit.statistics %>%
		filter(Criterion=="AIC (smaller is better)" | Criterion=="Scaled Deviance") %>%
		dcast(modelVars~Criterion, value.var="Value") %>%
		setnames("AIC (smaller is better)", "AIC") 
	y %<>% merge(z, by="modelVars") %>% 
	arrange(modelVars)
	y %<>%
	mutate(
		Intercept = paste(
			Intercept %>% round(2),
			" [",
			parameter.estimates[which(parameter.estimates$Parameter=="Intercept"), ]$LowerLRCL[1] %>% round(2),
			", ",
			parameter.estimates[which(parameter.estimates$Parameter=="Intercept"), ]$UpperLRCL[1] %>% round(2),
			"]",
			sep=""
		),
		`S_t` = paste(
			Ln_Size_t_1_st %>% round(2),
			" [",
			parameter.estimates[which(parameter.estimates$Parameter=="Ln_Size_t_1_st"), ]$LowerLRCL[1] %>% round(2),
			", ",
			parameter.estimates[which(parameter.estimates$Parameter=="Ln_Size_t_1_st"), ]$UpperLRCL[1] %>% round(2),
			"]",
			sep=""
		),
		`Native Bug` = paste(
			CH_t_1 %>% round(2),
			" [",
			parameter.estimates[which(parameter.estimates$Parameter=="CH_t_1"), ]$LowerLRCL[1] %>% round(2),
			", ",
			parameter.estimates[which(parameter.estimates$Parameter=="CH_t_1"), ]$UpperLRCL[1] %>% round(2),
			"]",
			sep=""
		),
		`Native Moth` = paste(
			ME_t_1 %>% round(2),
			" [",
			parameter.estimates[which(parameter.estimates$Parameter=="ME_t_1"), ]$LowerLRCL[1] %>% round(2),
			", ",
			parameter.estimates[which(parameter.estimates$Parameter=="ME_t_1"), ]$UpperLRCL[1] %>% round(2),
			"]",
			sep=""
		),
		`Native Scale` = paste(
			DA_t_1 %>% round(2),
			" [",
			parameter.estimates[which(parameter.estimates$Parameter=="DA_t_1"), ]$LowerLRCL[1] %>% round(2),
			", ",
			parameter.estimates[which(parameter.estimates$Parameter=="DA_t_1"), ]$UpperLRCL[1] %>% round(2),
			"]",
			sep=""
		)
	) %>%
	dplyr::select(
		Intercept, 
		S_t, 
		`Native Bug`, 
		`Native Moth`,
		`Native Scale`,
		`Scaled Deviance`
	)
	return(y)
}
