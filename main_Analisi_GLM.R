# adeguare nuova palette a tutti i grafici. ok?
# grafico gruppi cells vs inquinanti. ok?
# controllo tra piastre, controlli omogenei. explorazione visiva

# in analisi_glm:
# effetto togliere una piastra.
# plot effetti separati per group (non molto leggibile ad ora).

PathScript <- '.' 
PathAnalisi <- '.' 
PathData <- c('G:/My Drive/lavorincorso/brain_perm/pfas_tursiops_troncatus/data/Plate 1_19112021_ID457_Tm_P20_SingleData.csv',
              'G:/My Drive/lavorincorso/brain_perm/pfas_tursiops_troncatus/data/Plate 2_19112021_ID457_Tm_P21_SingleData.csv',
              'G:/My Drive/lavorincorso/brain_perm/pfas_tursiops_troncatus/data/Plate 3_19112021_ID457_Tm_P22_SingleData.csv',
              'G:/My Drive/lavorincorso/brain_perm/pfas_tursiops_troncatus/data/Plate 4_19112021_ID457_Tm_P24_SingleData.csv')

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
  
  rmarkdown::render("preprocessing_data.Rmd", 
                    params = list(PathData=PathData,tab_txt=tab_txt),
                    output_file = paste0(PathScript,"/Output_1.html"))
  
  rmarkdown::render("analysis_parametric.Rmd", 
                    output_file = paste0(PathScript,"/Output_2.html"))
  
}

InvokeR();
