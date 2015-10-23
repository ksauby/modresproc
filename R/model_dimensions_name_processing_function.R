model_dimensions_name_processing_function <- function(y) {
	if ("Columns in X" %in% names(y)) 
		{setnames(y, "Columns in X", "Upper Bound, Number of Parameters")}
	if (length(grep("Columns in Z", names(y), fixed=T)) > 0) 
		{setnames(y, names(y)[grep("Columns in Z", names(y), fixed=T)], 
		"ColumnsZ")}
	return(y)
	}