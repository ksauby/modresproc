#' Add column of Temperature x Precipitation interactions
#' 
#' @ description Add column of Temperature x Precipitation interactions.
#' @param y
#' 
#' @export

addTempPrecipInterCol <- function(y) {
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
	return(y)
}

#' Add column of Insect x Weather interactions
#' 
#' @ description Add column of Insect x Weather interactions.
#' @param y
#' 
#' @export

addInsectWeatherCol <- function(y) {
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
	return(y)
}
