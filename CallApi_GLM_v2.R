library(httr)  # Per inviare richieste HTTP
library(readr) # Per leggere il CSV
library(MASS)  # Per glm.nb
library(jsonlite)  # Per convertire in JSON

download_and_load_csv <- function(api_url, token, save_path) {

  response <- GET(
    api_url,
    add_headers(Authorization = paste("Bearer", token))
  )
  
  if (status_code(response) == 200) {
    raw_data <- content(response, "raw") 
    writeBin(raw_data, save_path)
    
    dataset <- read.csv(save_path, sep = ";")   
    return(dataset)
    
  } else {
    stop("Errore nella chiamata all'API: ", status_code(response))
  }
}

###### questa forse va messa in qualche file utils? non fa altro che aggiungere gli asterischi sui p significativi
get_stars <- function(ps){
  stars=rep("",length(ps))
  stars[ps<=.05]=paste0(stars[ps<=.05],"*")
  stars[ps<=.01]=paste0(stars[ps<=.01],"*")
  stars[ps<=.001]=paste0(stars[ps<=.001],"*")
  stars
}

############## 
make_up_res <- function(ps){
  ps=sprintf("%.4f", round(ps,4))  
  ps[ps<.001]="<0.001"
  ps
}
##############


run_glm_model <- function(dataset) {
  
  dataset$Cond <- factor(dataset$Condition)
  dataset$Compound <- factor(dataset$Compound)
  dataset$Grp <- dataset$Group
  dataset$Cond <- relevel(dataset$Cond, 'Control:0')
  dataset$Cond <- factor(dataset$Cond)
  
  plates=unique(dataset$Plate)
  nplates=length(plates)
  
  res=lapply(plates, function(plt){
    model <- glm.nb(Counts ~ Cond * Grp, data = dataset[dataset$Plate==plt,])
    model_summary <- summary(model)
    as.data.frame(model_summary$coefficients)[,c(1,2,3,4)]
  })
  res=as.data.frame(res)
  fisher_stat=rowSums(-2*log(res[,grep("^Pr",colnames(res))]))
  fisher_ps=1-pchisq(fisher_stat,2*nplates)
  
  #liptak_stat=rowSums(res[,grep("z\\.value",colnames(res))])/sqrt(nplates)
  #liptak_ps=2*(pnorm(-abs(liptak_stat)))
  
  p.adj=p.adjust(fisher_ps,"holm")
  mean_coeff=rowMeans(res[,grep("Estimate",colnames(res))])
  
  
  smry=data.frame(mean_coeff=mean_coeff,
                  Fisher_stat=fisher_stat,
                  p.value=make_up_res(fisher_ps),
                  adj.p.value=make_up_res(p.adj),
                  get_stars(p.adj)
                  )
  names(smry)[5]=""

  print("Coefficients of the GLM model (Averaged over plates) and Fisher combinations of p-values:")
  print(smry,digits=4)
  coefficients=as.matrix(smry)
  coefficients_json <- toJSON(split(coefficients, row(coefficients)))
  
  return(coefficients_json)
}

ids <- c(567,565,564) 
ids_string <- paste(ids, collapse = ";")  
current_dir <- getwd()

save_path <- file.path(current_dir, "merged_data.csv")

api_url <- paste0("https://sentinel.bca.unipd.it/RemoteAnalysis/GetToxicityMergedDataSet/", ids_string)

token <- "livio.finos@unipd.it"
dataset <- download_and_load_csv(api_url, token, save_path)
coefficients_json <- run_glm_model(dataset)

print("Model coefficients in JSON format:")
print(coefficients_json)

