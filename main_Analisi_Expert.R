# Script Main di esempio, richiamo della funzione macro Prof. Finos

#Param: PathScript
#Param: PathDati
#Param:
#Param:
#Param:
#Param:

InvokeR <- function(PathScript, PathDati)
{
	setwd(PathScript)
	source('Utils.R');

	rmarkdown::render("01_report_preprocessing.Rmd")
}

InvokeR('C:\\Users\\USER\\Downloads\\App_Data\\ScriptR\\');
