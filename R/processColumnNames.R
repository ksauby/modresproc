#' Change Names of Columns in a Model Selection Table from SAS GLIMMIX Model Output 
#' 
#' @param y
#' @param select_list
#' 
#' @export

processColumnNames <- function(y) {
	# change column names
	if ("Columns in X" %in% names(y)) {
		setnames(y, "Columns in X", "Number of Fixed Effects Parameters")
	}
	if (length(grep("Columns in Z", names(y), fixed=T)) > 0) {
		setnames(
			y, 
			names(y)[grep("Columns in Z", names(y), fixed=T)], 
			"ColumnsZ"
		)
	}
	# insects
	if ("CA" %in% names(y)) {setnames(y, "CA", "Invasive Moth")}
	if ("CH" %in% names(y)) {setnames(y, "CH", "Native Bug")}
	if ("DA" %in% names(y)) {setnames(y, "DA", "Native Scale")}
	if ("ME" %in% names(y)) {setnames(y, "ME", "Native Moth")}
	if ("CA_t_1" %in% names(y)) {setnames(y, "CA_t_1", "Invasive Moth")}
	if ("CAyr_t_1" %in% names(y)) {setnames(y, "CAyr_t_1", "Invasive Moth")}
	if ("CH_t_1" %in% names(y)) {setnames(y, "CH_t_1", "Native Bug")}
	if ("CHyr_t_1" %in% names(y)) {setnames(y, "CHyr_t_1", "Native Bug")}
	if ("DA_t_1" %in% names(y)) {setnames(y, "DA_t_1", "Native Scale")}
	if ("DAyr_t_1" %in% names(y)) {setnames(y, "DAyr_t_1", "Native Scale")}
	if ("ME_t_1" %in% names(y)) {setnames(y, "ME_t_1", "Native Moth")}
	if ("MEyr_t_1" %in% names(y)) {setnames(y, "MEyr_t_1", "Native Moth")}
	if ("NatInsect_t_1" %in% names(y)) {
		setnames(y, "NatInsect_t_1", "Native Insects")
	}
	if ("NatInsectyr_t_" %in% names(y)) {
		setnames(y, "NatInsectyr_t_", "Native Insects")
	}
	# plant size
	if ("Ln_Size_t_1_st" %in% names(y)) {setnames(y, "Ln_Size_t_1_st", "C_t")}
	if ("Ln_Cone_t_1_st" %in% names(y)) {
		setnames(
			y, 
			"Ln_Cone_t_1_st", 
			"ln(Cone Volume), Stand."
		)
	}
	if (length(grep("Ln_Cylinder", names(y), fixed=T)) > 0) {
		setnames(
			y, 
			names(y)[grep("Ln_Cylinder", names(y), fixed=T)], 
			"ln(Cylinder Volume), Stand."
		)
	}
	# interactions
	if ("CA_t_1*CH_t_1" %in% names(y)) {
		setnames(y, "CA_t_1*CH_t_1", "Invasive Moth x Native Bug")
	}
	if (length(grep("CA_t_1*NatInse", names(y), fixed=T)) > 0) {
		setnames(
			y, 
			names(y)[grep("CA_t_1*NatInse", names(y), fixed=T)], 
			"Invasive Moth x Native Insects"
		)
	}
	if ("P1*CA_t_1" %in% names(y)) {
		setnames(y, "P1*CA_t_1", "Invasive Moth x Precipitation")
	}
	if ("T1*CA_t_1" %in% names(y)) {
		setnames(y, "T1*CA_t_1", "Invasive Moth x Temperature")
	}
	if ("P1*CH_t_1" %in% names(y)) {
		setnames(y, "P1*CH_t_1", "Native Bug x Precipitation")
	}
	if ("T1*CH_t_1" %in% names(y)) {
		setnames(y, "T1*CH_t_1", "Native Bug x Temperature")
	}
	if ("P1*DA_t_1" %in% names(y)) {
		setnames(y, "P1*DA_t_1", "Native Scale x Precipitation")
	}
	if ("T1*DA_t_1" %in% names(y)) {
		setnames(y, "T1*DA_t_1", "Native Scale x Temperature")
	}
	if ("P1*ME_t_1" %in% names(y)) {
		setnames(y, "P1*ME_t_1", "Native Moth x Precipitation")
	}
	if ("T1*ME_t_1" %in% names(y)) {
		setnames(y, "T1*ME_t_1", "Native Moth x Temperature")
	}
	return(y)			
}

#' Change Names of Precipitation and Temperature Columns
#' 
#' @param y
#' @param select_list
#' 
#' @export

processT1P1ColumnNames <- function(y) {
	if ("T1" %in% names(y)) {setnames(y, "T1", "T")}
	if ("P1" %in% names(y)) {setnames(y, "P1", "P")}
}
