# adeguare nuova palette a tutti i grafici. ok?
# grafico gruppi cells vs inquinanti. ok?
# controllo tra piastre, controlli omogenei. explorazione visiva

# in analisi_glm:
# effetto togliere una piastra.
# plot effetti separati per group (non molto leggibile ad ora).

PathScript <- '.' 
PathAnalisi <- '.' 
PathData <- 'G:\\My Drive\\lavorincorso\\brain_perm\\pfas_bos_taurus/data/' 

# gli estremi degli intervalli sempre separati di " - " (occhio agli spazi prima e dopo il -)
# usare Inf e -Inf (senza spazio il - prima di Inf); 
#    per esempio: >32 deve essere indicato come "32 - Inf" e 
#                 <14 come "-Inf - 14"                
tab_txt= "ID,Group      , Nuclei - Nucleus Length [µm],Nuclei - Intensity Nucleus HOECHST 33342 Mean,Nuclei - Nucleus Ratio Width to Length \n
1, G0          , 9 - 27, 300 - 900, 0.2 - 0.9\n
2, S           , 9 - 27, 900 - 1600, 0.2 - 0.9\n
3, G2 + early M, 12 - 27, 1600 - 3000, 0.2 - 0.9\n
4, M           , 8 - 12, 1600 - 3000, 0.2 - 0.9\n
5, Large       , 27 - Inf , 300 - 3000, 0.2 - 0.9\n
6, Small       , -Inf - 8  , 300 - 3000, 0.2 - 0.9\n
6, Small       , 8 - 9  , 300 - 1600, 0.2 - 0.9"




InvokeR <- function()
{
  setwd(PathScript)
  
  rmarkdown::render("analysis_parametric_Leave1PlateOut.Rmd", 
                    params = list(leaveout="1"),
                    output_file = paste0(PathScript,"/Output_1.html"))
  rmarkdown::render("analysis_parametric_Leave1PlateOut.Rmd", 
                    params = list(leaveout="2"),
                    output_file = paste0(PathScript,"/Output_2.html"))
  rmarkdown::render("analysis_parametric_Leave1PlateOut.Rmd", 
                    params = list(leaveout="3"),
                    output_file = paste0(PathScript,"/Output_3.html"))
  rmarkdown::render("analysis_parametric_Leave1PlateOut.Rmd", 
                    params = list(leaveout="4"),
                    output_file = paste0(PathScript,"/Output_4.html"))
  
}

InvokeR();
