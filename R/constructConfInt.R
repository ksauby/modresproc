constructConfInt <- function(parm.est) {
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

constructNLMIXEDConfInt <- function(parm.est, round.n=2) {
	z = data.frame()
	if ("b0" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			Intercept = paste(
				parm.est[which(parm.est$Parameter=="b0"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="b0"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="b0"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}
	if ("b1" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			Ln_size_max_t_1_st = paste(
				parm.est[which(parm.est$Parameter=="b1"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="b1"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="b1"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}
	if ("b2" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			CH_t_1 = paste(
				parm.est[which(parm.est$Parameter=="b2"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="b2"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="b2"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}
	if ("b3" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			DA_t_1 = paste(
				parm.est[which(parm.est$Parameter=="b3"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="b3"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="b3"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}
	if ("b4" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			ME_t_1 = paste(
				parm.est[which(parm.est$Parameter=="b4"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="b4"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="b4"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	if ("b5" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			T1_FW = paste(
				parm.est[which(parm.est$Parameter=="b5"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="b5"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="b5"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	if ("b6" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			E1_SS_st = paste(
				parm.est[which(parm.est$Parameter=="b6"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="b6"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="b6"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	if ("b7" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			F_SS_st = paste(
				parm.est[which(parm.est$Parameter=="b7"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="b7"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="b7"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	if ("b8" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			P1_SS = paste(
				parm.est[which(parm.est$Parameter=="b8"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="b8"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="b8"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	if ("b9" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			P2_SS = paste(
				parm.est[which(parm.est$Parameter=="b9"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="b9"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="b9"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	if ("b10" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			P1_FW = paste(
				parm.est[which(parm.est$Parameter=="b10"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="b10"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="b10"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	if ("b11" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			P2_FW = paste(
				parm.est[which(parm.est$Parameter=="b11"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="b11"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="b11"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	if ("a0" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			Intercept = paste(
				parm.est[which(parm.est$Parameter=="a0"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="a0"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="a0"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	if ("a1" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			Ln_size_max_t_1_st = paste(
				parm.est[which(parm.est$Parameter=="a1"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="a1"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="a1"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	if ("a2" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			CH_t_1 = paste(
				parm.est[which(parm.est$Parameter=="a2"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="a2"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="a2"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	if ("a3" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			DA_t_1 = paste(
				parm.est[which(parm.est$Parameter=="a3"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="a3"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="a3"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	if ("a4" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			ME_t_1 = paste(
				parm.est[which(parm.est$Parameter=="a4"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="a4"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="a4"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	if ("a5" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			T1_FW = paste(
				parm.est[which(parm.est$Parameter=="a5"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="a5"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="a5"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	if ("a6" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			E1_SS_st = paste(
				parm.est[which(parm.est$Parameter=="a6"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="a6"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="a6"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	if ("a7" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			F_SS_st = paste(
				parm.est[which(parm.est$Parameter=="a7"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="a7"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="a7"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	if ("a8" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			P1_SS = paste(
				parm.est[which(parm.est$Parameter=="a8"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="a8"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="a8"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	if ("a9" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			P2_SS = paste(
				parm.est[which(parm.est$Parameter=="a9"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="a9"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="a9"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	if ("a10" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			P1_FW = paste(
				parm.est[which(parm.est$Parameter=="a10"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="a10"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="a10"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	if ("a11" %in% parm.est$Parameter) {
		z[i, ] %<>%
		mutate(
			P2_FW = paste(
				parm.est[which(parm.est$Parameter=="a11"), 
					]$Estimate %>% round(round.n),
				" [",
				parm.est[which(parm.est$Parameter=="a11"), 
					]$Lower %>% round(round.n),
				", ",
				parm.est[which(parm.est$Parameter=="a11"), 
					]$Upper %>% round(round.n),
				"]",
				sep=""
			)
		)
	}	
	return(z)
}