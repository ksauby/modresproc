#' Construct Conf. Intervals the Hard Way
#' @param parm.est parameter.estimate output from SAS
#' 
#' @export

constructConfInt2 <- function(parm.est, round.n=2) {
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
	if ("Intercept" %in% Data$Effect) {
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
			)
		)
	}
	if ("Ln_Size_t_1_st" %in% Data$Effect) {
		model_selection_table[i, ] %<>%
		mutate(
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
	return(Data)
}

#' Construct Conf. Intervals
#' @param parameter.estimates Table of parameter.estimate output from SAS
#' 
#' @export

constructConfInt <- function(parameter.estimates, round_n=3) {
	parameter.estimates %<>% round_df(round_n)
	parameter.estimates %>% mutate(Estimate.CF = paste(
		Estimate,
		" [",
		Lower,
		", ",
		Upper,
		"]",
		sep=""
	))
}

#' Construct Conf. Intervals with p-values
#' @param parameter.estimates Table of parameter.estimate output from SAS
#' 
#' @export

constructConfIntpValue <- function(parameter.estimates, round_n=3, conf.int = FALSE) {
	parameter.estimates %<>% round_df(round_n)
	parameter.estimates %<>% round_df(round_n)
	if (conf.int == FALSE) {
		parameter.estimates %>% mutate(Estimate.CF = paste(
			Estimate,
			" (",
			Probt,
			")",
			sep=""
		))
	} else {
		parameter.estimates %>% mutate(Estimate.CF = paste(
			Estimate,
			" [",
			Lower,
			", ",
			Upper,
			"]",
			" (",
			Probt,
			")",
			sep=""
		))
	}
}


#' Construct Conf. Intervals for Fecundity Random Effects

#' @param parameter.estimates Table of parameter.estimate output from SAS
#' 
#' @export

constructConfIntforRandomEffects <- function(y) {
	for (i in 1:dim(y)[1]) {
		if (grepl("LocYr", y$Parameter[i])==T) {
			y$Estimate.CF[i] = paste(
				"Location (Fecundity Year)", 
				" = ", 
				y$Estimate.CF[i], 
				sep=""
			)
		} else if (grepl("Loc", y$Parameter[i])==T & grepl("LocYr", 
			y$Parameter[i])==F) {
			y$Estimate.CF[i] = paste(
				"Location", 
				"\n= ", 
				y$Estimate.CF[i], 
				sep=""
			)
		}
		if (grepl("Yr", y$Parameter[i])==T & grepl("LocYr", y$Parameter[i])==F) {
			y$Estimate.CF[i] = paste(
				"Fecundity Year", 
				"\n= ", 
				y$Estimate.CF[i], 
				sep=""
			)
		}
		if (grepl("PlantID", y$Parameter[i])==T) {
			y$Estimate.CF[i] = paste(
				"Plant ID", 
				"\n= ", 
				y$Estimate.CF[i], 
				sep=""
			)
		}
		if (grepl("Island", y$Parameter[i])==T) {
			if (grepl("NetIsland", y$Parameter[i])==T) {
				y$Estimate.CF[i] = paste(
					"Network x Island", 
					"\n= ", 
					y$Estimate.CF[i], 
					sep=""
				)
			} else {
			y$Estimate.CF[i] = paste(
					"Island", 
					"\n= ", 
					y$Estimate.CF[i], 
					sep=""
				)
			}
		}
		if (grepl("Network", y$Parameter[i])==T) {
			y$Estimate.CF[i] = paste(
				"Network", 
				"\n= ", 
				y$Estimate.CF[i], 
				sep=""
			)
		}
		
		
	}
	return(y)
}

#' Construct Conf. Intervals for Fecundity Random Effects

#' @param parameter.estimates Table of parameter.estimate output from SAS
#' 
#' @export

constructConfIntforWeatherData <- function(y) {
	for (i in 1:dim(y)[1]) {
		if (grepl("Spring", y$Parameter[i])==T) {
			y$Estimate.CF[i] = paste(y$Parameter[i], "\n= ", y$Estimate.CF[i], sep="")
		}
		if (grepl("Fall", y$Parameter[i])==T) {
			y$Estimate.CF[i] = paste(y$Parameter[i], "\n= ", y$Estimate.CF[i], sep="")
		}
	}
	return(y)
}
