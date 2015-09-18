#' Format parameters and their estimates for a model selection table using data from SAS GLIMMIX
#' 
#' @param y

genmod_modelselection_format_parameter_estimates_function <- function(y) {	
	y %<>% 
	dplyr::select(
		Parameter,
		DF,
		Estimate,
		StdErr,
		LowerWaldCL,
		UpperWaldCL,
		ChiSq,
		ProbChiSq
	)
	# change effect names
	y[y$Parameter == "Ln_Cone_t_1_st", ]$Parameter 	<- "Standardized Cone Volume [t-1]"
	y[y$Parameter == "Ln_Size_t_1_st", ]$Parameter 	<- "Standardized Size [t-1]"
	y[y$Parameter == "Ln_Size_t_st", ]$Parameter 	<- "Standardized Size [t]"
	y[y$Parameter == "Ln_Size_min_st", ]$Parameter 	<- "Standardized Minimum Size"
	y[y$Parameter == "CA_t_1", ]$Parameter 			<- "Invasive Moth [t-1]"
	y[y$Parameter == "CH_t_1", ]$Parameter 			<- "Native Bug [t-1]"
	y[y$Parameter == "T1", ]$Parameter 				<- "Temp. PCA Axis 1"
	y[y$Parameter == "T2", ]$Parameter 				<- "Temp. PCA Axis 2"
	y[y$Parameter == "P1", ]$Parameter 				<- "Precip. PCA Axis 1"
	y[y$Parameter == "P2", ]$Parameter 				<- "Precip. PCA Axis 2"
	y[y$Parameter == "T1*P1", ]$Parameter 			<- "Temp. PCA Axis 1 x Precip. PCA Axis 1"
	y[y$Parameter == "T2*P1", ]$Parameter 			<- "Temp. PCA Axis 2 x Precip. PCA Axis 1"
	y[y$Parameter == "T1*P2", ]$Parameter 			<- "Temp. PCA Axis 1 x Precip. PCA Axis 2"
	y[y$Parameter == "T2*P2", ]$Parameter 			<- "Temp. PCA Axis 2 x Precip. PCA Axis 2"
	y %<>% as.data.table %>%   
	# rename column names
	setnames("StdErr",		"Standard Error") %>%
	setnames("LowerWaldCL",	"Lower Wald CL") %>%
	setnames("UpperWaldCL",	"Upper Wald CL") %>%
	setnames("ChiSq",		"Chi-squared") %>%
	setnames("ProbChiSq",	"Prob(Chi-squared)") %>%
	return(y)
}	
