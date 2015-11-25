#' Create table with the convergence status of multiple SAS GLIMMIX models
#' 
#' @param modelresults
#' @importFrom dplyr group_by summarise arrange desc
#' @import data.table
#' @import magrittr
#' @export

randeffects_converg_status_function <- function(modelresults) {
	modelresults %>% as.data.frame %>%
		group_by(`Random Effects`) %>%
		summarise(
			`Positive Non-Zero Covariance Estimates?` = `Positive Definite G-Matrix?`[1]
		) %>%
		as.data.frame %>%
		arrange(desc(`Positive Non-Zero Covariance Estimates?`))
		
}
