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
	select_list,
	nmodels
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
		model_dimensions_name_processing_function
	# temperature and precipitation interactions
	y$`P x T` = "NA"
	# if names contain P2 and T2
	if (length(grep("P2", names(y), fixed=T)) > 0 & length(grep("T2", names(y), 
	fixed=T)) > 0) {
		y %<>%
		mutate(
			`P x T` = replace(`P x T`, 
				which(`T1*P1`=="X" & `T1*P2`!="X" & `T2*P1`!="X" & 
				`T2*P2`!= "X"),
				"T1 x P1"),
			`P x T` = replace(`P x T`, 
				which(`T1*P1`!="X" & `T1*P2`=="X" & `T2*P1`!="X" & 
				`T2*P2`!="X"),
				"T1 x P2"),
			`P x T` = replace(`P x T`, 
				which(`T1*P1`!="X" & `T1*P2`!="X" & `T2*P1`=="X" & 
				`T2*P2`!="X"),
				"T2 x P1"),
			`P x T` = replace(`P x T`, 
				which(`T1*P1`!="X" & `T1*P2`!="X" & `T2*P1`!="X" & 
				`T2*P2`=="X"),
				"T2 x P2"),
			`P x T` = replace(`P x T`, 
				which(`T1*P1`=="X" & `T1*P2`=="X" & `T2*P1`=="X" & 
				`T2*P2`=="X"),
				"T1 x T2 x P1 x P2")
		)
	}
	# if names contain T2 but NOT P2
	if (length(grep("P2", names(y), fixed=T)) == 0 & length(grep("T2", 
	names(y), fixed=T)) > 0) {
		y %<>%
		mutate(
			`P x T` = replace(`P x T`, 
				which(`T1*P1`=="X" & `T2*P1`!="X"),
				"T1 x P1"),
			`P x T` = replace(`P x T`, 
				which(`T1*P1`!="X" & `T2*P1`=="X"),
				"T2 x P1"),
			`P x T` = replace(`P x T`, 
				which(`T1*P1`=="X" & `T2*P1`=="X"),
				"T1 x T2 x P1")
		)
	}
	# if names contain P2 but NOT T2
	if (length(grep("P2", names(y), fixed=T)) > 0 & length(grep("T2", names(y), 
	fixed=T)) == 0) {
		y %<>%
		mutate(
			`P x T` = replace(`P x T`, 
				which(`T1*P1`=="X" & `T1*P2`!="X"),
				"T1 x P1"),
			`P x T` = replace(`P x T`, 
				which(`T1*P1`!="X" & `T1*P2`=="X"),
				"T1 x P2"),
			`P x T` = replace(`P x T`, 
				which(`T1*P1`=="X" & `T1*P2`=="X"),
				"T1 x P1 x P2")
		)
	}
	y$`Insect x Weather` = "NA"
	if (length(grep("CA_t_1", names(y), fixed=T)) > 0) {
		y %<>%
		mutate(
			`Insect x Weather` = replace(`Insect x Weather`, 
				which(`P1*CA_t_1`=="X"),
				"Invasive Moth x P"),
			`Insect x Weather` = replace(`Insect x Weather`, 
				which(`T1*CA_t_1`=="X"),
				"Invasive Moth x T"),
			`Insect x Weather` = replace(`Insect x Weather`, 
				which(`P1*CH_t_1`=="X"),
				"Native Bug x P"),
			`Insect x Weather` = replace(`Insect x Weather`, 
				which(`T1*CH_t_1`=="X"),
				"Native Bug x T"),
			`Insect x Weather` = replace(`Insect x Weather`, 			which(`P1*CA_t_1*CH_t`=="X"),
					"Invasive Moth x Native Bug x P"),
			`Insect x Weather` = replace(`Insect x Weather`, 				which(`T1*CA_t_1*CH_t`=="X"),
				"Invasive Moth x Native Bug x T")
		)
	}
	if (length(grep("DA_t_1", names(y), fixed=T)) > 0) {
		y %<>%
		mutate(
			`Insect x Weather` = replace(`Insect x Weather`, 
				which(`P1*CH_t_1`=="X"),
				"Native Bug x P"),
			`Insect x Weather` = replace(`Insect x Weather`, 
				which(`T1*CH_t_1`=="X"),
				"Native Bug x T"),
			`Insect x Weather` = replace(`Insect x Weather`, 
				which(`P1*DA_t_1`=="X"),
				"Native Scale x P"),
			`Insect x Weather` = replace(`Insect x Weather`, 
				which(`T1*DA_t_1`=="X"),
				"Native Scale x T"),
			`Insect x Weather` = replace(`Insect x Weather`, 
				which(`P1*ME_t_1`=="X"),
				"Native Moth x P"),
			`Insect x Weather` = replace(`Insect x Weather`, 
				which(`T1*ME_t_1`=="X"),
				"Native Moth x T")
		)
	}
	if (length(grep("CA_t_1", names(y), fixed=T)) > 0) {
		# insect/weather interactions
		y[which(select(y, 
			starts_with("P1*CA_t_1*CH_t")) == "X"),]$`P1*CA_t_1`<-"X"
		y[which(select(y, 
			starts_with("P1*CA_t_1*CH_t")) == "X"),]$`P1*CH_t_1`<-"X"
		y[which(select(y, 
			starts_with("P1*CA_t_1*CH_t")) == "X"),]$`CA_t_1*CH_t_1`<-"X"
		y[which(select(y, 
			starts_with("T1*CA_t_1*CH_t")) == "X"),]$`T1*CA_t_1`<-"X"
		y[which(select(y, 
			starts_with("T1*CA_t_1*CH_t")) == "X"),]$`T1*CH_t_1`<-"X"
		y[which(select(y, 
			starts_with("T1*CA_t_1*CH_t")) == "X"),]$`CA_t_1*CH_t_1`<-"X"
	}
	y %<>% cAIC_function
	y <- y[, select_list]
	y %<>% names_processing_function
	# take modelVars from top of list
	topmodels = y[which(y$`delta cAIC` <= 2), "modelVars"]
	top.estimates = 
	parameter.estimates[which(parameter.estimates$modelVars %in% 
		topmodels),]	
	# top.estimates = 
	#	parameter.estimates[which(parameter.estimates$modelVars %in% topmodels$modelVars),]	
	# modelVars = y[1:nmodels, "modelVars"]
	
	
	# how do I convert ln standardized back to regular numbers?
	for (i in 1:length(topmodels)) {
		Data = top.estimates[which(top.estimates$modelVars == y$modelVars[i]), ]
		if ("T1" %in% Data$Effect & "T2" %in% Data$Effect) {
			y[i, ] %<>%
			mutate(
				T = paste(
					# T1
					"T1 = ", 
					Data[which(Data$Effect=="T1"), ]$Estimate %>% round(2),
					" [",
					Data[which(Data$Effect=="T1"), ]$Lower %>% round(2),
					", ",
					Data[which(Data$Effect=="T1"), ]$Upper %>% round(2),
					"]",
					# T2
					", T2 = ", 
					Data[which(Data$Effect=="T2"), ]$Estimate %>% round(2),
					" [",
					Data[which(Data$Effect=="T2"), ]$Lower %>% round(2),
					", ",
					Data[which(Data$Effect=="T2"), ]$Upper %>% round(2),
					"]",
					sep=""
				)	
			)
		}
		if ("T1" %in% Data$Effect & !("T2" %in% Data$Effect)) {
			y[i, ] %<>%
			mutate(
				T = paste(
					# T1
					"T1 = ", 
					Data[which(Data$Effect=="T1"), ]$Estimate %>% round(2),
					" [",
					Data[which(Data$Effect=="T1"), ]$Lower %>% round(2),
					", ",
					Data[which(Data$Effect=="T1"), ]$Upper %>% round(2),
					"]",
					sep=""
				)	
			)
		}
		if ("P1" %in% Data$Effect & "P2" %in% Data$Effect) {
			y[i, ] %<>%
			mutate(	
				P = paste(
					# P1
					"P1 = ", 
					Data[which(Data$Effect=="P1"), ]$Estimate %>%
						 round(2),
					" [",
					Data[which(Data$Effect=="P1"), ]$Lower %>%
						 round(2),
					", ",
					Data[which(Data$Effect=="P1"), ]$Upper %>% 
						round(2),
					"]",
					# P2
					", P2 = ", 
					Data[which(Data$Effect=="P2"), ]$Estimate %>%
						 round(2),
					" [",
					Data[which(Data$Effect=="P2"), ]$Lower %>%
						 round(2),
					", ",
					Data[which(Data$Effect=="P2"), ]$Upper %>% 
						round(2),
					"]",
					sep=""
				)	
			)
		}	
		if ("P1" %in% Data$Effect & !("P2" %in% Data$Effect)) {
			y[i, ] %<>%
			mutate(	
				P = paste(
					# P1
					"P1 = ", 
					Data[which(Data$Effect=="P1"), ]$Estimate %>%
						 round(2),
					" [",
					Data[which(Data$Effect=="P1"), ]$Lower %>%
						 round(2),
					", ",
					Data[which(Data$Effect=="P1"), ]$Upper %>% 
						round(2),
					"]",
					sep=""
				)	
			)
		}	
		if ("CA_t_1" %in% Data$Effect) {
			y[i, ] %<>%
			mutate(	
				`Invasive Moth` = paste(
					Data[which(Data$Effect=="CA_t_1" & 
					Data$CA_t_1==1), ]$Estimate %>% round(2),
					" [",
					Data[which(Data$Effect=="CA_t_1" & 
					Data$CA_t_1==1), ]$Lower %>% round(2),
					", ",
					Data[which(Data$Effect=="CA_t_1" & 
					Data$CA_t_1==1), ]$Upper %>% round(2),
					"]",
					sep=""
				)
			)
		}	
		if ("CH_t_1" %in% Data$Effect) {
			y[i, ] %<>%
			mutate(	
				`Native Bug` = paste(
					Data[which(Data$Effect=="CH_t_1" & 
					Data$CH_t_1==1), ]$Estimate %>% round(2),
					" [",
					Data[which(Data$Effect=="CH_t_1" & 
					Data$CH_t_1==1), ]$Lower %>% round(2),
					", ",
					Data[which(Data$Effect=="CH_t_1" & 
					Data$CH_t_1==1), ]$Upper %>% round(2),
					"]",
					sep=""
				)
			)
		}	
		if ("DA_t_1" %in% Data$Effect) {
			y[i, ] %<>%
			mutate(	
				`Native Scale` = paste(
					Data[which(Data$Effect=="DA_t_1" & 
					Data$DA_t_1==1), ]$Estimate %>% round(2),
					" [",
					Data[which(Data$Effect=="DA_t_1" & 
					Data$DA_t_1==1), ]$Lower %>% round(2),
					", ",
					Data[which(Data$Effect=="DA_t_1" & 
					Data$DA_t_1==1), ]$Upper %>% round(2),
					"]",
					sep=""
				)
			)
		}	
		if ("ME_t_1" %in% Data$Effect) {
			y[i, ] %<>%
			mutate(	
				`Native Moth` = paste(
					Data[which(Data$Effect=="ME_t_1" & 
					Data$ME_t_1==1), ]$Estimate %>% round(2),
					" [",
					Data[which(Data$Effect=="ME_t_1" & 
					Data$ME_t_1==1), ]$Lower %>% round(2),
					", ",
					Data[which(Data$Effect=="ME_t_1" & 
					Data$ME_t_1==1), ]$Upper %>% round(2),
					"]",
					sep=""
				)
			)
		}	
		if ("CHyr_t_1" %in% Data$Effect) {
			y[i, ] %<>%
			mutate(	
				`Native Bug` = paste(
					Data[which(Data$Effect=="CHyr_t_1" & 
					Data$CHyr_t_1==1), ]$Estimate %>% round(2),
					" [",
					Data[which(Data$Effect=="CHyr_t_1" & 
					Data$CHyr_t_1==1), ]$Lower %>% round(2),
					", ",
					Data[which(Data$Effect=="CHyr_t_1" & 
					Data$CHyr_t_1==1), ]$Upper %>% round(2),
					"]",
					sep=""
				)
			)
		}	
		if ("DAyr_t_1" %in% Data$Effect) {
			y[i, ] %<>%
			mutate(	
				`Native Scale` = paste(
					Data[which(Data$Effect=="DAyr_t_1" & 
					Data$DAyr_t_1==1), ]$Estimate %>% round(2),
					" [",
					Data[which(Data$Effect=="DAyr_t_1" & 
					Data$DAyr_t_1==1), ]$Lower %>% round(2),
					", ",
					Data[which(Data$Effect=="DAyr_t_1" & 
					Data$DAyr_t_1==1), ]$Upper %>% round(2),
					"]",
					sep=""
				)
			)
		}	
		if ("MEyr_t_1" %in% Data$Effect) {
			y[i, ] %<>%
			mutate(	
				`Native Moth` = paste(
					Data[which(Data$Effect=="MEyr_t_1" & 
					Data$MEyr_t_1==1), ]$Estimate %>% round(2),
					" [",
					Data[which(Data$Effect=="MEyr_t_1" & 
					Data$MEyr_t_1==1), ]$Lower %>% round(2),
					", ",
					Data[which(Data$Effect=="MEyr_t_1" & 
					Data$MEyr_t_1==1), ]$Upper %>% round(2),
					"]",
					sep=""
				)
			)
		}	
		y[i, ] %<>%
		mutate(	
			# Intercept = paste( 
			# 	Data[which(Data$Effect=="Intercept"), 
			# 		]$Estimate %>% round(2),
			# 	" [",
			# 	Data[which(Data$Effect=="Intercept"), ]$Lower 
			# 		%>% round(2),
			# 	", ",
			# 	Data[which(Data$Effect=="Intercept"), ]$Upper 
			# 		%>% round(2),
			# 	"]",
			# 	sep=""
			# ),
			C_t = paste(
				Data[which(Data$Effect=="Ln_Size_t_1_st"), 
					]$Estimate %>% round(2),
				" [",
				Data[which(Data$Effect=="Ln_Size_t_1_st"), 
					]$Lower %>% round(2),
				", ",
				Data[which(Data$Effect=="Ln_Size_t_1_st"), 
					]$Upper %>% round(2),
				"]",
				sep=""
			)
		)
	}
	#y %<>% dplyr::select(-modelVars)
	y[, "P x T"][y[, "P x T"] == "NA"] <- ""
	y[, "Insect x Weather"][y[, "Insect x Weather"] == "NA"] <- ""
	y %<>% dplyr::select(-modelVars)
	return(y)
}
