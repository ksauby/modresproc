#' Process PCA Eigenvalue Results from SAS
#' 
#' @param PCA.all.surveys.eigenvalues
#' @param PCA.winter.eigenvalues
#' @param PCA.spring.eigenvalues

Process_PCA_Eigenvalues <- function(PCA.all.surveys.eigenvalues, PCA.winter.eigenvalues, PCA.spring.eigenvalues) {
	PCA.all.surveys.eigenvalues$Data	<- "All Surveys"
	PCA.winter.eigenvalues$Data 		<- "Winter"
	PCA.spring.eigenvalues$Data 		<- "Spring"
	PCA_eigenvalue_summary = PCA.all.surveys.eigenvalues %>%
		rbind(PCA.winter.eigenvalues) %>%
		rbind(PCA.spring.eigenvalues) %>%
		group_by(modelVars, Data) %>%
		summarise(
			First.Eigenvalue = Eigenvalue[1],
			Second.Eigenvalue = Eigenvalue[2],
			Cumulative.Var.1 = Cumulative[1],
			Cumulative.Var.2 = Cumulative[2]
		)
		for (i in 1:dim(PCA_eigenvalue_summary)[1]) {
			if (PCA_eigenvalue_summary$Second.Eigenvalue[i] < 1) {
				PCA_eigenvalue_summary$Second.Eigenvalue[i] <- NA
				PCA_eigenvalue_summary$Cumulative.Var.2[i] <- NA
			} else {
				PCA_eigenvalue_summary$Cumulative.Var.1[i] <- NA
			}
		} 
	PCA_eigenvalue_summary %<>%
		as.data.frame %>%
		reshape2::melt(id.vars=c("modelVars", "Data", "Cumulative.Var.1",  
			"Cumulative.Var.2" ))
	names(PCA_eigenvalue_summary)[5:6] <- c("Eigenvalue Type", "Eigenvalue")
	PCA_eigenvalue_summary %<>%
			reshape2::melt(id.vars=c("modelVars", "Data", "Eigenvalue Type",
				 "Eigenvalue")) %>%
			.[, -5]
	names(PCA_eigenvalue_summary)[5] <- c("Cumulative Variance")
	PCA_eigenvalue_summary %<>% 
		filter(!is.na(Eigenvalue) & !is.na(`Cumulative Variance`))
	return(PCA_eigenvalue_summary)
}