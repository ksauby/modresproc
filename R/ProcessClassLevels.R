#' Process ClassLevels output from SAS
#' 
#' @param x
#' 
#' @export

ProcessClassLevels <- function(x) {
	x %>%
		filter(Class=="Location" | Class=="PlantID") %>%
		select(Class,Levels,modelVars) %>%
		dcast(modelVars ~ Class, value.var="Levels")
}
