#' Format parameters and their estimates for a model selection table using data from SAS GLIMMIX
#' 
#' @param models.dimensions
#' @param convergence.status
#' @param parameter.estimates
#' @param conditional.fit.statistics
#' @param select_list
#' @export

model_selection_results_function <- function(
	model_selection_table,
	select_list,
	round.n=2
)
{
  	model_selection_table %<>% filter(pdG==1) %>% select(-pdG)
	model_selection_table %<>% cAIC_function
	model_selection_table <- model_selection_table[, select_list]
	model_selection_table %<>% names_processing_function
	# take modelVars from top of list
	topmodels = model_selection_table[which(model_selection_table$`delta cAIC` <= 2), "modelVars"]
	top.estimates = 
	parameter.estimates[which(parameter.estimates$modelVars %in% 
		topmodels),]	
	# top.estimates = 
	#	parameter.estimates[which(parameter.estimates$modelVars %in% topmodels$modelVars),]	
	# modelVars = model_selection_table[1:nmodels, "modelVars"]
	
	
	# how do I convert ln standardized back to regular numbers?
	for (i in 1:length(topmodels)) {
		Data = top.estimates[which(top.estimates$modelVars == model_selection_table$modelVars[i]), ]
		if ("T1" %in% Data$Effect & "T2" %in% Data$Effect) {
			model_selection_table[i, ] %<>%
			mutate(
				T = paste(
					# T1
					"T1 = ", 
					Data[which(Data$Effect=="T1"), ]$Estimate %>% round(round.n),
					" [",
					Data[which(Data$Effect=="T1"), ]$Lower %>% round(round.n),
					", ",
					Data[which(Data$Effect=="T1"), ]$Upper %>% round(round.n),
					"]",
					# T2
					", T2 = ", 
					Data[which(Data$Effect=="T2"), ]$Estimate %>% round(round.n),
					" [",
					Data[which(Data$Effect=="T2"), ]$Lower %>% round(round.n),
					", ",
					Data[which(Data$Effect=="T2"), ]$Upper %>% round(round.n),
					"]",
					sep=""
				)	
			)
		}
		if ("T1" %in% Data$Effect & !("T2" %in% Data$Effect)) {
			model_selection_table[i, ] %<>%
			mutate(
				T = paste(
					# T1
					"T1 = ", 
					Data[which(Data$Effect=="T1"), ]$Estimate %>% round(round.n),
					" [",
					Data[which(Data$Effect=="T1"), ]$Lower %>% round(round.n),
					", ",
					Data[which(Data$Effect=="T1"), ]$Upper %>% round(round.n),
					"]",
					sep=""
				)	
			)
		}
		if ("P1" %in% Data$Effect & "P2" %in% Data$Effect) {
			model_selection_table[i, ] %<>%
			mutate(	
				P = paste(
					# P1
					"P1 = ", 
					Data[which(Data$Effect=="P1"), ]$Estimate %>%
						 round(round.n),
					" [",
					Data[which(Data$Effect=="P1"), ]$Lower %>%
						 round(round.n),
					", ",
					Data[which(Data$Effect=="P1"), ]$Upper %>% 
						round(round.n),
					"]",
					# P2
					", P2 = ", 
					Data[which(Data$Effect=="P2"), ]$Estimate %>%
						 round(round.n),
					" [",
					Data[which(Data$Effect=="P2"), ]$Lower %>%
						 round(round.n),
					", ",
					Data[which(Data$Effect=="P2"), ]$Upper %>% 
						round(round.n),
					"]",
					sep=""
				)	
			)
		}	
		if ("P1" %in% Data$Effect & !("P2" %in% Data$Effect)) {
			model_selection_table[i, ] %<>%
			mutate(	
				P = paste(
					# P1
					"P1 = ", 
					Data[which(Data$Effect=="P1"), ]$Estimate %>%
						 round(round.n),
					" [",
					Data[which(Data$Effect=="P1"), ]$Lower %>%
						 round(round.n),
					", ",
					Data[which(Data$Effect=="P1"), ]$Upper %>% 
						round(round.n),
					"]",
					sep=""
				)	
			)
		}	
		if ("CA_t_1" %in% Data$Effect) {
			model_selection_table[i, ] %<>%
			mutate(	
				`Invasive Moth` = paste(
					Data[which(Data$Effect=="CA_t_1" & 
					Data$CA_t_1==1), ]$Estimate %>% round(round.n),
					" [",
					Data[which(Data$Effect=="CA_t_1" & 
					Data$CA_t_1==1), ]$Lower %>% round(round.n),
					", ",
					Data[which(Data$Effect=="CA_t_1" & 
					Data$CA_t_1==1), ]$Upper %>% round(round.n),
					"]",
					sep=""
				)
			)
		}	
		if ("CH_t_1" %in% Data$Effect) {
			model_selection_table[i, ] %<>%
			mutate(	
				`Native Bug` = paste(
					Data[which(Data$Effect=="CH_t_1" & 
					Data$CH_t_1==1), ]$Estimate %>% round(round.n),
					" [",
					Data[which(Data$Effect=="CH_t_1" & 
					Data$CH_t_1==1), ]$Lower %>% round(round.n),
					", ",
					Data[which(Data$Effect=="CH_t_1" & 
					Data$CH_t_1==1), ]$Upper %>% round(round.n),
					"]",
					sep=""
				)
			)
		}	
		if ("DA_t_1" %in% Data$Effect) {
			model_selection_table[i, ] %<>%
			mutate(	
				`Native Scale` = paste(
					Data[which(Data$Effect=="DA_t_1" & 
					Data$DA_t_1==1), ]$Estimate %>% round(round.n),
					" [",
					Data[which(Data$Effect=="DA_t_1" & 
					Data$DA_t_1==1), ]$Lower %>% round(round.n),
					", ",
					Data[which(Data$Effect=="DA_t_1" & 
					Data$DA_t_1==1), ]$Upper %>% round(round.n),
					"]",
					sep=""
				)
			)
		}	
		if ("ME_t_1" %in% Data$Effect) {
			model_selection_table[i, ] %<>%
			mutate(	
				`Native Moth` = paste(
					Data[which(Data$Effect=="ME_t_1" & 
					Data$ME_t_1==1), ]$Estimate %>% round(round.n),
					" [",
					Data[which(Data$Effect=="ME_t_1" & 
					Data$ME_t_1==1), ]$Lower %>% round(round.n),
					", ",
					Data[which(Data$Effect=="ME_t_1" & 
					Data$ME_t_1==1), ]$Upper %>% round(round.n),
					"]",
					sep=""
				)
			)
		}	
		if ("CHyr_t_1" %in% Data$Effect) {
			model_selection_table[i, ] %<>%
			mutate(	
				`Native Bug` = paste(
					Data[which(Data$Effect=="CHyr_t_1" & 
					Data$CHyr_t_1==1), ]$Estimate %>% round(round.n),
					" [",
					Data[which(Data$Effect=="CHyr_t_1" & 
					Data$CHyr_t_1==1), ]$Lower %>% round(round.n),
					", ",
					Data[which(Data$Effect=="CHyr_t_1" & 
					Data$CHyr_t_1==1), ]$Upper %>% round(round.n),
					"]",
					sep=""
				)
			)
		}	
		if ("DAyr_t_1" %in% Data$Effect) {
			model_selection_table[i, ] %<>%
			mutate(	
				`Native Scale` = paste(
					Data[which(Data$Effect=="DAyr_t_1" & 
					Data$DAyr_t_1==1), ]$Estimate %>% round(round.n),
					" [",
					Data[which(Data$Effect=="DAyr_t_1" & 
					Data$DAyr_t_1==1), ]$Lower %>% round(round.n),
					", ",
					Data[which(Data$Effect=="DAyr_t_1" & 
					Data$DAyr_t_1==1), ]$Upper %>% round(round.n),
					"]",
					sep=""
				)
			)
		}	
		if ("MEyr_t_1" %in% Data$Effect) {
			model_selection_table[i, ] %<>%
			mutate(	
				`Native Moth` = paste(
					Data[which(Data$Effect=="MEyr_t_1" & 
					Data$MEyr_t_1==1), ]$Estimate %>% round(round.n),
					" [",
					Data[which(Data$Effect=="MEyr_t_1" & 
					Data$MEyr_t_1==1), ]$Lower %>% round(round.n),
					", ",
					Data[which(Data$Effect=="MEyr_t_1" & 
					Data$MEyr_t_1==1), ]$Upper %>% round(round.n),
					"]",
					sep=""
				)
			)
		}	
		model_selection_table[i, ] %<>%
		mutate(	
			 Intercept = paste( 
			 	Data[which(Data$Effect=="Intercept"), 
			 		]$Estimate %>% round(round.n),
			 	" [",
			 	Data[which(Data$Effect=="Intercept"), ]$Lower 
			 		%>% round(round.n),
			 	", ",
			 	Data[which(Data$Effect=="Intercept"), ]$Upper 
			 		%>% round(round.n),
			 	"]",
			 	sep=""
			),
			C_t = paste(
				Data[which(Data$Effect=="Ln_Size_t_1_st"), 
					]$Estimate %>% round(round.n),
				" [",
				Data[which(Data$Effect=="Ln_Size_t_1_st"), 
					]$Lower %>% round(round.n),
				", ",
				Data[which(Data$Effect=="Ln_Size_t_1_st"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}
	#model_selection_table %<>% dplyr::select(-modelVars)
	model_selection_table[, "P x T"][model_selection_table[, "P x T"] == "NA"] <- ""
	model_selection_table[, "Insect x Weather"][model_selection_table[, "Insect x Weather"] == "NA"] <- ""
	model_selection_table %<>% select(-modelVars)
	return(model_selection_table)
}
