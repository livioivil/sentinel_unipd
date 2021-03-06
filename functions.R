my_plot1 <- function(dati){
  library(ggplot2)
  p1 <- ggplot(dati, aes(x=Nuclei_length, y=Nuclei_intensity,col=Group0)) + geom_point(size=.01) + guides(color = guide_legend(override.aes = list(size=1)))
  p2 <- ggplot(dati, aes(x=Nuclei_Ratio_W_L, y=Nuclei_intensity,col=Group0)) + geom_point(size=.01) + guides(color = guide_legend(override.aes = list(size=1)))
  p3 <- ggplot(dati, aes(y=Nuclei_Ratio_W_L, x=Nuclei_length,col=Group0)) + geom_point(size=.01) + guides(color = guide_legend(override.aes = list(size = 1)))
  # p4 <- ggplot(dati, aes(Nuclei_intensity)) + geom_histogram()
  
  library(gridExtra)
  grid.arrange(p1, p2,p3, nrow = 2)
  
}

my_plot2 <- function(dati){
  library(ggplot2)
  p1 <- ggplot(dati[dati$Group3=="Small",], aes(Nuclei_intensity)) + geom_histogram(bins=100)+ggtitle("Small") + xlim(c(0,7500))
  p2 <- ggplot(dati[dati$Group3=="Normal",], aes(Nuclei_intensity)) + geom_histogram(bins=100)+ggtitle("Normal") + xlim(c(0,7500))
  p3 <- ggplot(dati[dati$Group3=="Large",], aes(Nuclei_intensity)) + geom_histogram(bins=100)+ggtitle("Large") + xlim(c(0,7500))
  
  library(gridExtra)
  grid.arrange(p1,p2,p3, nrow = 3)
  
}

plot_row_data <- function(filename){
  load(filename)
  library(ggplot2)
  p1 <- ggplot(dati, aes(x=Nuclei_length, y=Nuclei_intensity)) + geom_point()
  p2 <- ggplot(dati, aes(x=Nuclei_Ratio_W_L, y=Nuclei_intensity)) + geom_point()
  p3 <- ggplot(dati, aes(y=Nuclei_Ratio_W_L, x=Nuclei_length)) + geom_point()
  p4 <- ggplot(dati, aes(Nuclei_intensity)) + geom_histogram()
  
  library(gridExtra)
  grid.arrange(p1, p2,p3,p4, nrow = 2)
  
}


library(sjPlot)
library(ggplot2)
theme_set(theme_sjplot())

import_data_add_group <- function(filename){
  library(readxl)
  dati <- read_excel(filename)
  print(dim(dati))
  
  dati_reduce=function(dati,filename){
    dati=dati[sample(nrow(dati),nrow(dati)/20),]
    save(dati,file=gsub("xlsx","Rdata",filename))
    NULL
  }
  dati_reduce(dati,filename)
  
  dati <- add_groups(dati)
  # dati=dati[,-grep("Nuclei",names(dati))]
  
  D=aggregate(Nuclei_length ~ Group3+Group+Plate+Concentration+Compound+Row+Column, dati,  FUN=length)
  D$Counts=D$Nuclei_length
  D$Nuclei_length<- NULL
  D
}

######## CREAZIONE GRUPPI: 

add_groups <- function(dati){

  dati$Group=
    1*in_range(dati$Nuclei_length,lims = c(14,30))*
    in_range(dati$Nuclei_intensity,lims = c(400,1500))*
    in_range(dati$Nuclei_Ratio_W_L,lims = c(.2,.9))+
    2*in_range(dati$Nuclei_length,lims = c(14,30))*
    in_range(dati$Nuclei_intensity,lims = c(1500,2500))*
    in_range(dati$Nuclei_Ratio_W_L,lims = c(.2,.9))+
    3*in_range(dati$Nuclei_length,lims = c(14,30))*
    in_range(dati$Nuclei_intensity,lims = c(2500,12000))*
    in_range(dati$Nuclei_Ratio_W_L,lims = c(.2,.9))+
    4*in_range(dati$Nuclei_length,lims = c(6,14))*
    in_range(dati$Nuclei_intensity,lims = c(2100,12000))*
    in_range(dati$Nuclei_Ratio_W_L,lims = c(.2,.9))+
    5*in_range(dati$Nuclei_length,lims = c(30,Inf))*
    in_range(dati$Nuclei_intensity,lims = c(400,6000))*
    in_range(dati$Nuclei_Ratio_W_L,lims = c(.2,.9))+
    6*in_range(dati$Nuclei_length,lims = c(-Inf,6))*
    in_range(dati$Nuclei_intensity,lims = c(400,12000))*
    in_range(dati$Nuclei_Ratio_W_L,lims = c(.2,.9))+
    7*in_range(dati$Nuclei_length,lims = c(6,14))*
    in_range(dati$Nuclei_intensity,lims = c(400,2100))*
    in_range(dati$Nuclei_Ratio_W_L,lims = c(.2,.9))
  
  dati$Group=factor(dati$Group)
  levels(dati$Group)=c("0:None","1:G0","2:S","3:G2+M_early","4:M",
                       "5:Large", "6:Small","6:Small")
  dati$Group3=dati$Group
  levels(dati$Group3)[6]="Large"
  levels(dati$Group3)[7]="Small"
  levels(dati$Group3)[2:5]="Normal"
  
  print(table(dati$Group3))
  
  print(table(dati$Group))
  
  print(table(dati$Group))  
  # cat("
  #     qualcosa non va nel 9 gruppi?
  #     ")
  # 
  # summary(dati$Nuclei_length[dati$Group==0])
  # summary(dati$Nuclei_intensity[dati$Group==0])
  # summary(dati$Nuclei_Ratio_W_L[dati$Group==0])
  # 
  # hist(dati$Nuclei_length[dati$Group==0])
  # hist(dati$Nuclei_intensity[dati$Group==0])
  # hist(dati$Nuclei_Ratio_W_L[dati$Group==0])
  
  dati
}
