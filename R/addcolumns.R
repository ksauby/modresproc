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
			
			`P x T` = ifelse(
				(!is.na(`T1*P1`)) & is.na(`T1*P2`) & is.na(`T2*P1`) & 
					is.na(`T2*P2`),
				paste("T1 x P1 =", `T1*P1`, sep=" "),
				`P x T`
			),		
			`P x T` = ifelse(
				is.na(`T1*P1`) & !is.na(`T1*P2`) & is.na(`T2*P1`) & 
					is.na(`T2*P2`),
				paste("T1 x P2 =", `T1*P2`, sep=" "),
				`P x T`
			),
			`P x T` = ifelse(
				is.na(`T1*P1`) & is.na(`T1*P2`) & !is.na(`T2*P1`) & 
					is.na(`T2*P2`),
				paste("T2 x P1 =", `T2*P1`, sep=" "),
				`P x T`
			),
			`P x T` = replace(
				`P x T`, 
				which(is.na(`T1*P1`) & is.na(`T1*P2`) & is.na(`T2*P1`) & 
				!is.na(`T2*P2`)),
				"T2 x P2"
			),
			`P x T` = ifelse(
				!is.na(`T1*P1`) & !is.na(`T1*P2`) & !is.na(`T2*P1`) & 
					!is.na(`T2*P2`),
				paste(
					"T1 x P1 = ", `T1*P1`,
					",\nT1 x P2 = ", `T1*P2`,
					",\nT2 x P1 = ", `T2*P1`,
					",\nT2 x P2 = ", `T2*P2`,
					sep=""
				),
				`P x T`
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
			`P x T` = ifelse(
				!is.na(`T1*P2`) & is.na(`T2*P1`) & is.na(`T2*P2`),
				paste("T1 x P2 =", `T1*P2`, sep=" "),
				`P x T`
			),		
			`P x T` = ifelse(
				is.na(`T1*P2`) & !is.na(`T2*P1`) & is.na(`T2*P2`),
				paste("T2 x P1 =", `T2*P1`, sep=" "),
				`P x T`
			),
			`P x T` = ifelse(
				is.na(`T1*P2`) & is.na(`T2*P1`) & !is.na(`T2*P2`),
				paste("T2 x P2 =", `T2*P2`, sep=" "),
				`P x T`
			),
			`P x T` = ifelse(
				!is.na(`T1*P2`) & !is.na(`T2*P1`) & !is.na(`T2*P2`),
				paste(
					"T1 x P1 = ", `T1*P1`,
					",\nT1 x P2 = ", `T1*P2`,
					",\nT2 x P1 = ", `T2*P1`,
					",\nT2 x P2 = ", `T2*P2`,
					sep=""
				),
				`P x T`
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
			`P x T` = ifelse(
				!is.na(`T1*P1`) & is.na(`T2*P1`),
				paste("T1 x P1 =", `T1*P1`, sep=" "),
				`P x T`
			),		
			`P x T` = ifelse(
				is.na(`T1*P1`) & !is.na(`T2*P1`),
				paste("T2 x P1 =", `T2*P1`, sep=" "),
				`P x T`
			),
			`P x T` = ifelse(
				!is.na(`T1*P1`) & !is.na(`T2*P1`),
				paste(
					"T1 x P1 = ", `T1*P1`,
					",\nT2 x P1 = ", `T2*P1`,
					sep=""
				),
				`P x T`
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
			`P x T` = ifelse(
				!is.na(`T1*P1`) & is.na(`T1*P2`),
				paste("T1 x P1 =", `T1*P1`, sep=" "),
				`P x T`
			),		
			`P x T` = ifelse(
				is.na(`T1*P1`) & !is.na(`T1*P2`),
				paste("T1 x P2 =", `T1*P2`, sep=" "),
				`P x T`
			),
			`P x T` = ifelse(
				!is.na(`T1*P1`) & !is.na(`T1*P2`),
				paste(
					"T1 x P1 = ", `T1*P1`,
					",\nT1 x P2 = ", `T1*P2`,
					sep=""
				),
				`P x T`
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
			`Insect x Weather` = ifelse(
				!is.na(`P1*CA_t_1`),
				paste("Invasive Moth x P =", `P1*CA_t_1`, sep=" "),
				`Insect x Weather`
			),
			`Insect x Weather` = ifelse(
				!is.na(`T1*CA_t_1`),
				paste("Invasive Moth x T =", `T1*CA_t_1`, sep =" "),
				`Insect x Weather`
			),
			`Insect x Weather` = ifelse(
				!is.na(`P1*CH_t_1`),
				paste("Native Bug x P =", `P1*CH_t_1`, sep =" "),
				`Insect x Weather`
			),
			`Insect x Weather` = ifelse(
				!is.na(`T1*CH_t_1`),
				paste("Native Bug x T =", `T1*CH_t_1`, sep =" "),
				`Insect x Weather`
			),
			`Insect x Weather` = ifelse(
				!is.na(`P1*CA_t_1*CH_t_1`),
				"Invasive Moth x Native Bug x P",
				`Insect x Weather`
			),
			`Insect x Weather` = ifelse(
				!is.na(`T1*CA_t_1*CH_t_1`),
				"Invasive Moth x Native Bug x T",
				`Insect x Weather`
			)
		)
	}
	if (length(grep("DA_t_1", names(y), fixed=T)) > 0) {
		y %<>%
		mutate(
			`Insect x Weather` = ifelse(
				!is.na(`P1*CH_t_1`),
				paste("Native Bug x P =", `P1*CH_t_1`, sep =" "),
				`Insect x Weather`
			),
			`Insect x Weather` = ifelse(
				!is.na(`T1*CH_t_1`),
				paste("Native Bug x T =", `T1*CH_t_1`, sep =" "),
				`Insect x Weather`
			),
			`Insect x Weather` = ifelse(
				!is.na(`P1*DA_t_1`),
				paste("Native Scale x P =", `P1*DA_t_1`, sep =" "),
				`Insect x Weather`
			),
			`Insect x Weather` = ifelse(
				!is.na(`T1*DA_t_1`),
				paste("Native Scale x T =", `T1*DA_t_1`, sep =" "),
				`Insect x Weather`
			),
			`Insect x Weather` = ifelse(
				!is.na(`P1*ME_t_1`),
				paste("Native Moth x P =", `P1*ME_t_1`, sep =" "),
				`Insect x Weather`
			),
			`Insect x Weather` = ifelse(
				!is.na(`T1*ME_t_1`),
				paste("Native Moth x T =", `T1*ME_t_1`, sep =" "),
				`Insect x Weather`
			)
		)
	}
	return(y)
}

#' @title Add Vital Rate Column
#' 
#' @param y Parameter estimates output
#' 
#' @export

addVitalRateColumn <- function(y) {
 y$`Vital Rate` <- "Fruit Abundance"	
 y$`Vital Rate` <- replace(
 	y$`Vital Rate`,
 	grep("s2_a", y$Parameter, ignore.case=T),
 	"Fruit Absence"
 )
 y$`Vital Rate` <- replace(
 	y$`Vital Rate`,
 	grep("A", y$Parameter),
 	"Fruit Absence"
 )
 return(y)
}

