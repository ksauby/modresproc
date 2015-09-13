randeffects_converg_status_function <- function(modelresults) {
	modelresults %>% as.data.frame %>%
		group_by(`Random Effects`) %>%
		summarise(
			`Positive Definite G-Matrix?` = `Positive Definite G-Matrix?`[1]
		) %>%
		as.data.frame %>%
		arrange(desc(`Positive Definite G-Matrix?`))
		
}
