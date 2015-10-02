#' Format parameters and their estimates for a model selection table using data from SAS GLIMMIX
#' 
#' @param models.dimensions
#' @param convergence.status
#' @param parameter.estimates
#' @param conditional.fit.statistics
#' @param select_list

modelselection_model_results_function <- function(
	models.dimensions, 
	convergence.status, 
	parameter.estimates, 
	conditional.fit.statistics, 
	select_list
)
{
	conditional.fit.statistics %<>% 
		short_to_long_format_function %>%
		.[, -3]
	models.dimensions %<>%
		short_to_long_format_function %>%
		dplyr::select(modelVars, `Columns in X`, starts_with("Columns in Z"))
	y = merge(convergence.status, parameter.estimates) %>%
		# filter out models that didn't converge
		filter(pdG==1) %>%
		# change effects to columns
		short_to_long_format_X_function %>%
		# fit statistics
		merge(conditional.fit.statistics) %>%
		merge(models.dimensions) %>%
		model_dimensions_name_processing_function %>%
		mutate(
			cAIC = `-2 log L(y | r. effects)` + 
				2*(`Number of Parameters` + ColumnsZ)
		) %>%
		arrange(cAIC) %>%
		dplyr::select(-(`-2 log L(y | r. effects)`))
	# temperature and precipitation interactions
	y$`Temperature x Precipitation` = "NA"
	y %<>%
	mutate(
		`Temperature x Precipitation` = replace(`Temperature x Precipitation`, 
			which(`T1*P1`=="X" & `T1*P2`!="X" & `T2*P1`!="X" & `T2*P2`!= "X"),
			"T1 x P1"),
		`Temperature x Precipitation` = replace(`Temperature x Precipitation`, 
			which(`T1*P1`!="X" & `T1*P2`=="X" & `T2*P1`!="X" & `T2*P2`!="X"),
			"T1 x P2"),
		`Temperature x Precipitation` = replace(`Temperature x Precipitation`, 
			which(`T1*P1`!="X" & `T1*P2`!="X" & `T2*P1`=="X" & `T2*P2`!="X"),
			"T2 x P1"),
		`Temperature x Precipitation` = replace(`Temperature x Precipitation`, 
			which(`T1*P1`!="X" & `T1*P2`!="X" & `T2*P1`!="X" & `T2*P2`=="X"),
			"T2 x P2"),
		`Temperature x Precipitation` = replace(`Temperature x Precipitation`, 
			which(`T1*P1`=="X" & `T1*P2`=="X" & `T2*P1`=="X" & `T2*P2`=="X"),
			"Full")
	)
	# insect/weather interactions
	y[which(select(y, starts_with("P1*CA_t_1*CH_t")) == "X"),]$`P1*CA_t_1`<-"X"
	y[which(select(y, starts_with("P1*CA_t_1*CH_t")) == "X"),]$`P1*CH_t_1`<-"X"
	y[which(select(y, starts_with("P1*CA_t_1*CH_t")) == 
		"X"),]$`CA_t_1*CH_t_1`<-"X"
	y[which(select(y, starts_with("T1*CA_t_1*CH_t")) == "X"),]$`T1*CA_t_1`<-"X"
	y[which(select(y, starts_with("T1*CA_t_1*CH_t")) == "X"),]$`T1*CH_t_1`<-"X"
	y[which(select(y, starts_with("T1*CA_t_1*CH_t")) ==
		"X"),]$`CA_t_1*CH_t_1`<-"X"
	y <- y[, select_list]
	y %<>% cAIC_function
	y %<>% names_processing_function
	y[, "Temperature x Precipitation"][y[, "Temperature x Precipitation"] == "NA"] <- "."
	return(y)
}
