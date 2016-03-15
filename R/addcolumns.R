#' Add column of Temperature x Precipitation interactions
#' 
#' @ description Add column of Temperature x Precipitation interactions.
#' @param y
#' 
#' @export

addTempxPrecipColumns <- function(y) {
	# temperature and precipitation interactions
	y$`P x T` = ""
	# if names contain P2 and T2
	if (
		length(grep("P2", names(y), fixed=T)) > 0 & 
		length(grep("T2", names(y), fixed=T)) > 0 &
		length(grep("T1*P1", names(y), fixed=T)) > 0
	) {
		y %<>%
		mutate(
			`P x T` = replace(
				`P x T`, 
				which((!is.na(`T1*P1`)) & is.na(`T1*P2`) & is.na(`T2*P1`) & 
				is.na(`T2*P2`)),
				"T1 x P1"
			),
			`P x T` = replace(
				`P x T`, 
				which(is.na(`T1*P1`) & !is.na(`T1*P2`) & is.na(`T2*P1`) & 
				is.na(`T2*P2`)),
				"T1 x P2"
			),
			`P x T` = replace(
				`P x T`, 
				which(is.na(`T1*P1`) & is.na(`T1*P2`) & !is.na(`T2*P1`) & 
				is.na(`T2*P2`)),
				"T2 x P1"
			),
			`P x T` = replace(
				`P x T`, 
				which(is.na(`T1*P1`) & is.na(`T1*P2`) & is.na(`T2*P1`) & 
				!is.na(`T2*P2`)),
				"T2 x P2"
			),
			`P x T` = replace(
				`P x T`, 
				which(!is.na(`T1*P1`) & !is.na(`T1*P2`) & !is.na(`T2*P1`) & 
				!is.na(`T2*P2`)),
				"T1 x T2 x P1 x P2"
			)
		)
	}
	# if names contain P2, T2, but no interaction
	if (
		length(grep("P2", names(y), fixed=T)) > 0 & 
		length(grep("T2", names(y), fixed=T)) > 0 &
		length(grep("T1*P1", names(y), fixed=T)) == 0
	) {
		y %<>%
		mutate(
			`P x T` = replace(
				`P x T`, 
				which(!is.na(`T1*P2`) & is.na(`T2*P1`) & is.na(`T2*P2`)),
				"T1 x P2"
			),
			`P x T` = replace(
				`P x T`, 
				which(is.na(`T1*P2`) & !is.na(`T2*P1`) & is.na(`T2*P2`)),
				"T2 x P1"
			),
			`P x T` = replace(
				`P x T`, 
				which(is.na(`T1*P2`) & is.na(`T2*P1`) & !is.na(`T2*P2`)),
				"T2 x P2"
			),
			`P x T` = replace(
				`P x T`, 
				which(!is.na(`T1*P2`) & !is.na(`T2*P1`) & !is.na(`T2*P2`)),
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
			`P x T` = replace(
				`P x T`, 
				which(!is.na(`T1*P1`) & is.na(`T2*P1`)),
				"T1 x P1"
			),
			`P x T` = replace(
				`P x T`, 
				which(is.na(`T1*P1`) & !is.na(`T2*P1`)),
				"T2 x P1"
			),
			`P x T` = replace(`P x T`, 
				which(!is.na(`T1*P1`) & !is.na(`T2*P1`)),
				"T1 x T2 x P1"
			)
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
				which((!is.na(`T1*P1`) & is.na(`T1*P2`))),
				"T1 x P1"
			),
			`P x T` = replace(
				`P x T`, 
				which(is.na(`T1*P1`) & !is.na(`T1*P2`)),
				"T1 x P2"
			),
			`P x T` = replace(`P x T`, 
				which(!is.na(`T1*P1`) & !is.na(`T1*P2`)),
				"T1 x P1 x P2"
			)
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
	y$`Insect x Weather` = ""
	if (length(grep("CA_t_1", names(y), fixed=T)) > 0) {
		y %<>%
		mutate(
			`Insect x Weather` = replace(
				`Insect x Weather`, 
				which(!is.na(`P1*CA_t_1`)),
				"Invasive Moth x P"
			),
			`Insect x Weather` = replace(
				`Insect x Weather`, 
				which(!is.na(`T1*CA_t_1`)),
				"Invasive Moth x T"
			),
			`Insect x Weather` = replace(
				`Insect x Weather`, 
				which(!is.na(`P1*CH_t_1`)),
				"Native Bug x P"
			),
			`Insect x Weather` = replace(
				`Insect x Weather`, 
				which(!is.na(`T1*CH_t_1`)),
				"Native Bug x T"
			),
			`Insect x Weather` = replace(
				`Insect x Weather`,
				which(!is.na(`P1*CA_t_1*CH_t_1`)),
				"Invasive Moth x Native Bug x P"
			),
			`Insect x Weather` = replace(
				`Insect x Weather`, 				which(!is.na(`T1*CA_t_1*CH_t_1`)),
				"Invasive Moth x Native Bug x T"
			)
		)
	}
	if (length(grep("DA_t_1", names(y), fixed=T)) > 0) {
		y %<>%
		mutate(
			`Insect x Weather` = replace(
				`Insect x Weather`, 
				which(!is.na(`P1*CH_t_1`)),
				"Native Bug x P"
			),
			`Insect x Weather` = replace(
				`Insect x Weather`, 
				which(!is.na(`T1*CH_t_1`)),
				"Native Bug x T"
			),
			`Insect x Weather` = replace(
				`Insect x Weather`, 
				which(!is.na(`P1*DA_t_1`)),
				"Native Scale x P"
			),
			`Insect x Weather` = replace(
				`Insect x Weather`, 
				which(!is.na(`T1*DA_t_1`)),
				"Native Scale x T"
			),
			`Insect x Weather` = replace(
				`Insect x Weather`, 
				which(!is.na(`P1*ME_t_1`)),
				"Native Moth x P"
			),
			`Insect x Weather` = replace(
				`Insect x Weather`, 
				which(!is.na(`T1*ME_t_1`)),
				"Native Moth x T"
			)
		)
	}
	return(y)
}
