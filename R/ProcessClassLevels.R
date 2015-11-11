#' Process ClassLevels output from SAS
#' 
#' @param x

ProcessClassLevels <- function(x) {
	x %>%
		filter(Class=="Location" | Class=="PlantID") %>%
		dplyr::select(Class,Levels,modelVars) %>%
		dcast(modelVars ~ Class, value.var="Levels")
}
