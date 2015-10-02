#' Process model results for a model selection table using data from SAS GLIMMIX models
#' 
#' @param y
#' @param select_list

names_processing_function <- function(y) {
	# change column names
	if ("CA" %in% names(y)) {setnames(y, "CA", "Invasive Moth")}
	if ("CH" %in% names(y)) {setnames(y, "CH", "Native Bug")}
	if ("DA" %in% names(y)) {setnames(y, "DA", "Native Scale")}
	if ("ME" %in% names(y)) {setnames(y, "ME", "Native Moth")}
	if ("Ln_Size_t_1_st" %in% names(y)) 
		{setnames(y, "Ln_Size_t_1_st", "ln(Size [t-1]), Stand.")}
	if ("Ln_Cone_t_1_st" %in% names(y)) 
		{setnames(y, "Ln_Cone_t_1_st", "ln(Cone Volume [t-1]), Stand.")}
	if (length(grep("Ln_Cylinder", names(y), fixed=T)) > 0) 
		{setnames(y, names(y)[grep("Ln_Cylinder", names(y), fixed=T)], 
		"ln(Cylinder Volume [t-1]), Stand.")}
	if ("CA_t_1" %in% names(y)) 
		{setnames(y, "CA_t_1", "Invasive Moth [t-1]")}
	if ("CH_t_1" %in% names(y)) 
		{setnames(y, "CH_t_1", "Native Bug [t-1]")}
	if ("DA_t_1" %in% names(y)) 
		{setnames(y, "DA_t_1", "Native Scale [t-1]")}
	if ("ME_t_1" %in% names(y)) 
		{setnames(y, "ME_t_1", "Native Moth [t-1]")}
	if ("NatInsect_t_1" %in% names(y)) 
		{setnames(y, "NatInsect_t_1", "Native Insects [t-1]")}
	if ("CA_t_1*CH_t_1" %in% names(y)) 
		{setnames(y, "CA_t_1*CH_t_1", "Invasive Moth [t-1] x Native Bug [t-1]")}
	if (length(grep("CA_t_1*NatInse", names(y), fixed=T)) > 0) 
		{setnames(y, names(y)[grep("CA_t_1*NatInse", names(y), fixed=T)], 
		"Invasive Moth [t-1] x Native Insects [t-1]")}
	if ("P1*CA_t_1" %in% names(y)) 
		{setnames(y, "P1*CA_t_1", "Invasive Moth [t-1] x Precipitation")}
	if ("T1*CA_t_1" %in% names(y)) 
		{setnames(y, "T1*CA_t_1", "Invasive Moth [t-1] x Temperature")}
	if ("P1*CH_t_1" %in% names(y)) 
		{setnames(y, "P1*CH_t_1", "Native Bug [t-1] x Precipitation")}
	if ("T1*CH_t_1" %in% names(y)) 
		{setnames(y, "T1*CH_t_1", "Native Bug [t-1] x Temperature")}
	if ("P1*DA_t_1" %in% names(y)) 
		{setnames(y, "P1*DA_t_1", "Native Scale [t-1] x Precipitation")}
	if ("T1*DA_t_1" %in% names(y)) 
		{setnames(y, "T1*DA_t_1", "Native Scale [t-1] x Temperature")}
	if ("P1*ME_t_1" %in% names(y)) 
		{setnames(y, "P1*ME_t_1", "Native Moth [t-1] x Precipitation")}
	if ("T1*CH_t_1" %in% names(y)) 
		{setnames(y, "T1*ME_t_1", "Native Moth [t-1] x Temperature")}
	if ("T1" %in% names(y)) 
		{setnames(y, "T1", "Temperature")}
	if ("P1" %in% names(y)) 
		{setnames(y, "P1", "Precipitation")}
	return(y)			
}