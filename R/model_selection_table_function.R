#' @title Build table of candidate models
#' 
#' @param covariance.parameter.estimates
#' @param models.dimensions
#' @param convergence.status
#' @param parameter.estimates
#' @param conditional.fit.statistics
#' 
#' @importFrom reshape2 dcast
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace
#' @export

model_selection_table_function <- function(covariance.parameter.estimates, models.dimensions, convergence.status, parameter.estimates, 
	conditional.fit.statistics) {
	# to verify that all models have the same covariance parameters included
	if ("Subject" %in% names(covariance.parameter.estimates)) {
		covariance.parameter.estimates %<>% 
			mutate(CovParm = paste(CovParm, Subject)) %>%
			dcast(modelVars~CovParm, value.var="Estimate")
	} else {
		covariance.parameter.estimates %<>% 
			mutate(CovParm = paste(CovParm)) %>%
			dcast(modelVars~CovParm, value.var="Estimate")
	}
	names(covariance.parameter.estimates) <- str_replace_all(names(covariance.parameter.estimates), fixed(" "), "")
	conditional.fit.statistics %<>% 
		short_to_long_format_function %>%
		.[, -3]
	models.dimensions %<>%
		short_to_long_format_function %>%
		select(modelVars, `Columns in X`, starts_with("Columns in Z"))
	parameter.estimates %<>% constructConfInt
	y = parameter.estimates %>%
		filter(CH_t_1!=0) %>%
		filter(DA_t_1!=0) %>%
		filter(ME_t_1!=0) %>%
		filter(NatInsect_t_1!=0) %>%		
		dcast(modelVars~Effect, value.var="Estimate.CF") %>%
		# fit statistics
		merge(conditional.fit.statistics) %>%
		merge(models.dimensions) %>%
		merge(covariance.parameter.estimates) %>%
		model_dimensions_name_processing_function %>%
		merge(convergence.status %>% dplyr::select(modelVars, pdG))
	# temperature and precipitation interactions
	y$`P x T` = "NA"
	# if names contain P2 and T2
	if (
		length(grep("P2", names(y), fixed=T)) > 0 & 
		length(grep("T2", names(y), fixed=T)) > 0 &
		length(grep("T1*P1", names(y), fixed=T)) > 0
	) {
		y %<>%
		mutate(
			`P x T` = replace(`P x T`, 
				which((!is.na(`T1*P1`)) & is.na(`T1*P2`) & is.na(`T2*P1`) & 
				is.na(`T2*P2`)),
				"T1 x P1")
				)
				,
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
	# if names contain P2 and T2
	if (
		length(grep("P2", names(y), fixed=T)) > 0 & 
		length(grep("T2", names(y), fixed=T)) > 0 &
		length(grep("T1*P1", names(y), fixed=T)) == 0
	) {
		y %<>%
		mutate(
			`P x T` = replace(`P x T`, 
				which(
					`T1*P2`=="X" & `T2*P1`!="X" & 
					`T2*P2`!="X"),
					"T1 x P2"
				),
			`P x T` = replace(`P x T`, 
				which(
					`T1*P2`!="X" & `T2*P1`=="X" & 
					`T2*P2`!="X"),
					"T2 x P1"
				),
			`P x T` = replace(`P x T`, 
				which(
					`T1*P2`!="X" & `T2*P1`!="X" & 
					`T2*P2`=="X"),
					"T2 x P2"
				),
			`P x T` = replace(`P x T`, 
				which(
					`T1*P2`=="X" & `T2*P1`=="X" & 
					`T2*P2`=="X"),
					"T1 x T2 x P1 x P2"
				)
		)
	}
	# if names contain T2 but NOT P2
	if (
		length(grep("P2", names(y), fixed=T)) == 0 & 
		length(grep("T2", names(y), fixed=T)) > 0
	) {
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
	if (
		length(grep("P2", names(y), fixed=T)) > 0 & 
		length(grep("T2", names(y), fixed=T)) == 0
	) {
		y %<>%
		mutate(
			`P x T` = replace(
				`P x T`, 
				which((!is.na(`T1*P1`) & is.na(`T1*P2`)),
				"T1 x P1")
			))
				,
			`P x T` = replace(`P x T`, 
				which(`T1*P1`!="X" & `T1*P2`=="X"),
				"T1 x P2"),
			`P x T` = replace(`P x T`, 
				which(`T1*P1`=="X" & `T1*P2`=="X"),
				"T1 x P1 x P2")
		)
	}
	
	y %<>%
	mutate(
		`P x T` = replace(`P x T`, 
			which((!is.na(`T1*P1`)) & is.na(`T1*P2`) & is.na(`T2*P1`) & 
			is.na(`T2*P2`)),
			"T1 x P1")
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
	if (length(grep("CAyr_t_1", names(y), fixed=T)) > 0) {
		if (length(grep("P1*CAyr_t_1", names(y), fixed=T)) > 0) {
			y %<>%
			mutate(
				`Insect x Weather` = replace(`Insect x Weather`, 
					which(`P1*CAyr_t_1`=="X"),
					"Invasive Moth x P")
			)
		}
		if (length(grep("T1*CAyr_t_1", names(y), fixed=T)) > 0) {
			y %<>%
			mutate(
				`Insect x Weather` = replace(`Insect x Weather`, 
					which(`T1*CAyr_t_1`=="X"),
					"Invasive Moth x T")
			)
		}
		if (length(grep("P1*CHyr_t_1", names(y), fixed=T)) > 0) {
			y %<>%
			mutate(
				`Insect x Weather` = replace(`Insect x Weather`, 
					which(`P1*CHyr_t_1`=="X"),
					"Native Bug x P")
			)
		}
		if (length(grep("T1*CHyr_t_1", names(y), fixed=T)) > 0) {
			y %<>%
			mutate(
				`Insect x Weather` = replace(`Insect x Weather`, 
					which(`T1*CHyr_t_1`=="X"),
					"Native Bug x T")
			)
		}		
		if (length(grep("P1*CAyr_t_1*CH", names(y), fixed=T)) > 0) {
			y %<>%
			mutate(
				`Insect x Weather` = replace(`Insect x Weather`, 					which(`P1*CAyr_t_1*CHyr_t`=="X"),
					"Invasive Moth x Native Bug x P")
			)
		}	
		if (length(grep("T1*CAyr_t_1*CH", names(y), fixed=T)) > 0) {
			y %<>%
			mutate(
				`Insect x Weather` = replace(`Insect x Weather`, 					which(`T1*CAyr_t_1*CHyr_t`=="X"),
					"Invasive Moth x Native Bug x T")
			)
		}		
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
	if (length(grep("DAyr_t_1", names(y), fixed=T)) > 0) {
		if (length(grep("P1*CHyr_t_1", names(y), fixed=T)) > 0) {
			y %<>%
			mutate(
				`Insect x Weather` = replace(`Insect x Weather`, 
					which(`P1*CHyr_t_1`=="X"),
					"Native Bug x P")
			)
		}
		if (length(grep("T1*CHyr_t_1", names(y), fixed=T)) > 0) {
			y %<>%
			mutate(
				`Insect x Weather` = replace(`Insect x Weather`, 
					which(`T1*CHyr_t_1`=="X"),
					"Native Bug x T")
			)
		}
		if (length(grep("P1*DAyr_t_1", names(y), fixed=T)) > 0) {
			y %<>%
			mutate(
				`Insect x Weather` = replace(`Insect x Weather`, 
					which(`P1*DAyr_t_1`=="X"),
					"Native Scale x P")
			)
		}
		if (length(grep("T1*DAyr_t_1", names(y), fixed=T)) > 0) {
			y %<>%
			mutate(
				`Insect x Weather` = replace(`Insect x Weather`, 
					which(`T1*DAyr_t_1`=="X"),
					"Native Scale x T")
			)
		}		
		if (length(grep("P1*MEyr_t_1", names(y), fixed=T)) > 0) {
			y %<>%
			mutate(
				`Insect x Weather` = replace(`Insect x Weather`, 
					which(`P1*MEyr_t_1`=="X"),
					"Native Moth x P")
			)
		}	
		if (length(grep("T1*MEyr_t_1", names(y), fixed=T)) > 0) {
			y %<>%
			mutate(
				`Insect x Weather` = replace(`Insect x Weather`, 
					which(`T1*MEyr_t_1`=="X"),
					"Native Moth x T")
			)
		}		
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
	return(y)
}
