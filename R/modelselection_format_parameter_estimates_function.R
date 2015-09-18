#' Format parameters and their estimates for a model selection table using data from SAS GLIMMIX
#' 
#' @param y

modelselection_format_parameter_estimates_function <- function(y) {	
	y %<>% 
	dplyr::select(
		Effect,
		Estimate,
		StdErr,
		DF,
		tValue,
		Probt
	) %>%
	as.data.table %>%   
	# rename column names
	setnames("StdErr",	"Standard Error") %>%
	setnames("tValue",	"t-Value") %>%
	setnames("Probt",	"Prob(t)")
	# change effect names
	y[y$"Effect" == "Ln_Cone_t_1_st"]$Effect 	<- "Standardized Cone Volume [t-1]"
	y[y$"Effect" == "Ln_Size_t_1_st"]$Effect 	<- "Standardized Size [t-1]"
	y[y$"Effect" == "CA_t_1"]$Effect 			<- "Invasive Moth [t-1]"
	y[y$"Effect" == "CH_t_1"]$Effect 			<- "Native Bug [t-1]"
	y[y$"Effect" == "T1"]$Effect 				<- "Temp. PCA Axis 1"
	y[y$"Effect" == "T2"]$Effect 				<- "Temp. PCA Axis 2"
	y[y$"Effect" == "P1"]$Effect 				<- "Precip. PCA Axis 1"
	y[y$"Effect" == "P2"]$Effect 				<- "Precip. PCA Axis 2"
	y[y$"Effect" == "T1*P1"]$Effect 				<- "Temp. PCA Axis 1 x Precip. PCA Axis 1"
	y[y$"Effect" == "T2*P1"]$Effect 				<- "Temp. PCA Axis 2 x Precip. PCA Axis 1"
	y[y$"Effect" == "T1*P2"]$Effect 				<- "Temp. PCA Axis 1 x Precip. PCA Axis 2"
	y[y$"Effect" == "T2*P2"]$Effect 				<- "Temp. PCA Axis 2 x Precip. PCA Axis 2"
	return(y)
}	
