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
	y$`P x T` = "NA"
	y %<>%
	mutate(
		`P x T` = replace(`P x T`, 
			which(`T1*P1`=="X" & `T1*P2`!="X" & `T2*P1`!="X" & `T2*P2`!= "X"),
			"T1 x P1"),
		`P x T` = replace(`P x T`, 
			which(`T1*P1`!="X" & `T1*P2`=="X" & `T2*P1`!="X" & `T2*P2`!="X"),
			"T1 x P2"),
		`P x T` = replace(`P x T`, 
			which(`T1*P1`!="X" & `T1*P2`!="X" & `T2*P1`=="X" & `T2*P2`!="X"),
			"T2 x P1"),
		`P x T` = replace(`P x T`, 
			which(`T1*P1`!="X" & `T1*P2`!="X" & `T2*P1`!="X" & `T2*P2`=="X"),
			"T2 x P2"),
		`P x T` = replace(`P x T`, 
			which(`T1*P1`=="X" & `T1*P2`=="X" & `T2*P1`=="X" & `T2*P2`=="X"),
			"T1 x T2 x P1 x P2")
	)
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
		
		# take modelVars from top of list
		top.estimates = 
parameter.estimates[which(parameter.estimates$modelVars==y[1, "modelVars"]),]

	y <- y[, select_list]
	y %<>% cAIC_function
	y %<>% names_processing_function

	# how do I convert ln standardized back to regular numbers?
		
	if ("T1" %in% top.estimates$Effect & "T2" %in% top.estimates$Effect) {
		y %<>%
		mutate(
			T = replace(T,
				which(modelVars==top.estimates$modelVars[1]),
				paste(
					"T1=", 
					round(top.estimates[which(top.estimates$Effect=="T1"),
						]$Estimate, 2),
					", T2=", 
					round(top.estimates[which(top.estimates$Effect=="T2"), 
						]$Estimate, 2),
					sep="")
			)
		)
	}
	if ("P1" %in% top.estimates$Effect & "P2" %in% top.estimates$Effect) {
		y %<>%
		mutate(	
			P = replace(P,
				which(modelVars==top.estimates$modelVars[1]),
				paste(
					"P1=", 
					round(top.estimates[which(top.estimates$Effect=="P1"),
						]$Estimate, 2),
					", P2=", 
					round(top.estimates[which(top.estimates$Effect=="P2"), 
						]$Estimate, 2),
					sep="")
			)
		)
	}
	if ("ME_t_1" %in% top.estimates$Effect) {
		y %<>%
		mutate(	
			`Native Moth` = replace(`Native Moth`,
				which(modelVars==top.estimates$modelVars[1]),
				round(top.estimates[which(top.estimates$Effect=="ME_t_1" & 
					top.estimates$ME_t_1==0), ]$Estimate, 2)
			)
		)
	}	
	if ("CA_t_1" %in% top.estimates$Effect) {
		y %<>%
		mutate(	
			`Invasive Moth` = replace(`Invasive Moth`,
				which(modelVars==top.estimates$modelVars[1]),
				round(top.estimates[which(top.estimates$Effect=="CA_t_1" & 
					top.estimates$CA_t_1==0), ]$Estimate, 2)
			)
		)
	}	
	y %<>%
	mutate(	
		S_t = replace(S_t,
			which(modelVars==top.estimates$modelVars[1]),
			round(top.estimates[which(top.estimates$Effect=="Ln_Size_t_1_st"), 
				]$Estimate, 2)
		)
	) 
				
	y[, "P x T"][y[, "P x T"] == "NA"] <- ""
	y[, "Insect x Weather"][y[, "Insect x Weather"] == "NA"] <- ""
	return(y)
}
