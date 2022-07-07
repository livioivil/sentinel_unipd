# Script Main di esempio, richiamo della funzione macro Prof. Finos

#PathScript <- 'C:\\Users\\USER\\Downloads\\App_Data\\ScriptR\\' 
#PathOutput1 <- 'C:\\Users\\USER\\Downloads\\App_Data\\Analisi\\Output1.html' 
#PathOutput2 <- 'C:\\Users\\USER\\Downloads\\App_Data\\Analisi\\Output2.html' 

PathScript <- $PathScript$
PathOutput_1 <- $PathOutput_1$
PathOutput_2 <- $PathOutput_2$
Gruppo1 <- 400-1500
Gruppo2 <- 1500-2500
Gruppo3 <- 2500-12000
Gruppo4 <- 2100-12000
Gruppo5 <- 400-6000
Gruppo6 <- 400-12000
Gruppo7 <- 400-2100

InvokeR <- function()
{
    Sys.setenv(HOME='/srv/sentinel/')
	setwd(PathScript)
	source('utils.R');

	rmarkdown::render("01_report_preprocessing.Rmd", output_file = PathOutput_1)
}

InvokeR();