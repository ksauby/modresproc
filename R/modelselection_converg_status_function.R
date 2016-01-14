#' Create table with the convergence status of multiple SAS GLIMMIX models
#' 
#' @param modelresults
#' 
#' @export

randeffects_converg_status_function <- function(modelresults) {
	modelresults %>% as.data.frame %>%
		dplyr::group_by(`Random Effects`) %>%
		dplyr::summarise(
			`Positive Definite G-Matrix?` = `Positive Definite G-Matrix?`[1]
		) %>%
		as.data.frame %>%
		dplyr::arrange(desc(`Positive Definite G-Matrix?`))
		
}
