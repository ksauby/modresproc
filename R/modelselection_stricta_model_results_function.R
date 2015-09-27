#' Format parameters and their estimates for a model selection table using data from SAS GLIMMIX
#' 
#' @param models.dimensions
#' @param convergence.status
#' @param parameter.estimates
#' @param conditional.fit.statistics
#' @param replace_list
#' @param select_list

modelselection_stricta_model_results_function <- function(
	models.dimensions, 
	convergence.status, 
	parameter.estimates, 
	conditional.fit.statistics, 
	# replace_list,
	select_list
)
{
	# model dimensions
	x = models.dimensions %>% 
		dcast(modelVars~Descr, value.var="Value")
	names(x)[1:3] = c("modelVars", "ColumnsX", "ColumnsZ")
	x %<>% dplyr::select(modelVars, ColumnsX, ColumnsZ)
	# filter out models that didn't converge
	y = merge(convergence.status, parameter.estimates) %>%
		filter(pdG==1) %>%
		# change effects to columns
		dcast(modelVars~Effect, value.var="Estimate", fun=X_function) %>%
		# fit statistics
		merge(conditional.fit.statistics) %>%
		filter(Descr=="-2 log L(y | r. effects)") %>%
		setnames("Value", "-2 log L(y | r. effects)") %>%
		dplyr::select(-Descr) %>%
		merge(x) %>%
		mutate(
			cAIC = `-2 log L(y | r. effects)` + 2*(ColumnsX + ColumnsZ)
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
	# create 
	# select columns
	y <- y[, select_list]
	# cAIC	
	y %<>% cAIC_function
	# change column names
	y %<>% names_processing_function
	# replace values
	# y[, replace_list][y[, replace_list] > 0] <- "X"
	# y[, replace_list][y[, replace_list] == 0] <- "."
	y[, "Temperature x Precipitation"][y[, "Temperature x Precipitation"] == "NA"] <- "."
	return(y)
}
