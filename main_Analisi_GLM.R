# Script Main di esempio, richiamo della funzione macro Prof. Finos

PathScript <- 'C:\\Users\\USER\\Downloads\\App_Data\\ScriptR\\' 
PathAnalisi <- '' 
Gruppo1 <- 400-1500
Gruppo2 <- 1500-2500
Gruppo3 <- 2500-12000
Gruppo4 <- 2100-12000
Gruppo5 <- 400-6000
Gruppo6 <- 400-12000
Gruppo7 <- 400-2100

InvokeR <- function()
{
	setwd(PathScript)
	source('Utils.R');

	rmarkdown::render("01_report_preprocessing.Rmd", output_file = "output.html")
}

InvokeR();