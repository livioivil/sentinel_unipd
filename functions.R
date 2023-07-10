#cbPalette <- c("#999999", "#E69F00",, "#56B4E9",  "#F0E442", "#0072B2", "#D55E00", "#009E73")
cbPalette <- c("#F8766D","#B79F00","#00BA38","black","#619CFF","#F564E3")
############ 
my_plot_results <- function(mod){ 
  p=plot_model(mod, type = "eff", terms = c("Cond","Grp")) 
  p=p+coord_flip()+scale_y_continuous(trans='log10') +scale_colour_manual(values=cbPalette)
  temp=get_model_data(mod, type = "eff", terms = c("Cond","Grp")) 
  temp=data.frame(temp) 
  temp=temp[temp$x==1,c("predicted","group_col")] 
  temp$group_col=factor(temp$group_col,levels = sort(as.character(temp$group_col)))
  # rownames(temp)=temp[,"group_col"]
  # temp=temp[,1,drop=FALSE]
  p=p + geom_hline(data = temp,aes(yintercept=predicted,col=group_col))#+scale_colour_manual(values=cbPalette)#as.numeric(temp$group_col)) 
  p 
} 


my_plot_results_splitted <- function(mod){ 
  lvs=levels(mod$model$Grp)
  for(i in 1:length(lvs)) {  
    p=plot_model(mod, type = "eff", terms = c("Cond",paste0("Grp [",lvs[i],"]")),colors="gs") 
    p=p+coord_flip()+scale_y_continuous(trans='log10') +scale_colour_manual(values=cbPalette)
    temp=get_model_data(mod, type = "eff", terms = c("Cond","Grp")) 
    temp=data.frame(temp) 
    temp=temp[temp$x==1,c("predicted","group_col")] 
    colnames(temp)[2]="Group"
    temp$Group=factor(temp$Group,levels = sort(as.character(temp$Group)))
    # rownames(temp)=temp[,"group_col"]
    # temp=temp[,1,drop=FALSE]
    p=p + geom_hline(data = temp,aes(yintercept=predicted[i],col=Group[i]))#+scale_colour_manual(values=cbPalette)#as.numeric(temp$group_col)) 
    print(p) 
  }
  } 

###############################
my_plot1 <- function(dati){ 
  library(ggplot2) 
  qs=list(Nuclei_length=quantile(dati$Nuclei_length,.99,na.rm=TRUE)*1.01,
       Nuclei_intensity=quantile(dati$Nuclei_intensity,.99,na.rm=TRUE)*1.01,
       Nuclei_Ratio_W_L=quantile(dati$Nuclei_Ratio_W_L,.99,na.rm=TRUE)*1.01)
  p1 <- ggplot(dati, aes(x=Nuclei_length, y=Nuclei_intensity,col=Group0)) + geom_point(size=1) + 
    guides(color = guide_legend(override.aes = list(size=1))) + xlim(c(0,qs$Nuclei_length))+ ylim(c(0,qs$Nuclei_intensity))+
    xlab("Nuclei Length")+ylab("Nuclei Intensity")+scale_colour_manual(values=cbPalette)
  p2 <- ggplot(dati, aes(x=Nuclei_Ratio_W_L, y=Nuclei_intensity,col=Group0)) + geom_point(size=1) + 
    guides(color = guide_legend(override.aes = list(size=1))) + xlim(c(0,qs$Nuclei_Ratio_W_L))+ ylim(c(0,qs$Nuclei_intensity)) +
    xlab("Nuclei Ratio Width/Length")+ylab("Nuclei Intensity")+scale_colour_manual(values=cbPalette)
  p3 <- ggplot(dati, aes(x=Nuclei_length, y=Nuclei_Ratio_W_L,col=Group0)) + geom_point(size=1) + 
    guides(color = guide_legend(override.aes = list(size = 1))) + xlim(c(0,qs$Nuclei_length))+ ylim(c(0,qs$Nuclei_Ratio_W_L))+
    xlab("Nuclei Length")+ylab("Nuclei Ratio Width/Length")+scale_colour_manual(values=cbPalette)
  # p4 <- ggplot(dati, aes(Nuclei_intensity)) + geom_histogram() 
  
  library(gridExtra) 
  grid.arrange(p1, p2,p3, nrow = 2) 
  
} 

my_plot2 <- function(dati){ 
  library(ggplot2) 
  p1 <- ggplot(dati[dati$Group3=="Small",], aes(Nuclei_intensity)) + geom_histogram(bins=100)+ggtitle("Small") + xlim(c(0,7500)) +scale_colour_manual(values=cbPalette)
  p2 <- ggplot(dati[dati$Group3=="Normal",], aes(Nuclei_intensity)) + geom_histogram(bins=100)+ggtitle("Normal") + xlim(c(0,7500)) +scale_colour_manual(values=cbPalette)
  p3 <- ggplot(dati[dati$Group3=="Large",], aes(Nuclei_intensity)) + geom_histogram(bins=100)+ggtitle("Large") + xlim(c(0,7500)) +scale_colour_manual(values=cbPalette)
  
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


###########
explore_control_group_plate<- function(D){
  
  # library(data.table)
  # DD <- data.table(dati[,c("Population","Shape","Layer_ID",par_i)])
  DD <- D[D$Condition=="Control:0",c("Group","Cond","Plate","Counts"),]
  DD$Plate=as.numeric(DD$Plate)

  p=ggplot(data=DD) +   
    aes(y=Plate, x=Counts, group = interaction(Plate,Group)) + 
    ylab("Plate")+geom_point(aes(color =  Group, group=interaction(Plate,Group)),size=3)+
    scale_x_continuous(trans='log10')+scale_colour_manual(values=cbPalette)
print(p)
}

explore_control_group_plate_params<- function(D,par_i){
  
  # library(data.table)
  # DD <- data.table(dati[,c("Population","Shape","Layer_ID",par_i)])
  DD <- D[D$Cond=="Control:0",c("ID","Group","Row","Column","Cond","Plate",par_i)]
  DD <- DD[DD$Group!="0:None",]
  DD$Plate=as.factor(as.numeric(DD$Plate))
  
  # p=ggplot(data=DD) +   
  #   aes(y=Plate, x=Nuclei_intensity, group = interaction(Plate,Group)) + 
  #   ylab("Plate")+geom_point(aes(color =  Group, group=interaction(Plate,Group)),size=3)+
  #   scale_x_continuous(trans='log10')+scale_colour_manual(values=cbPalette)
  # print(p)
  # 
  p <- ggplot(DD, aes(group=interaction(Plate,ID), x=.data[[par_i]],
                      y=interaction(ID,Plate),fill=Plate)) + 
     theme(legend.position = "none")+geom_boxplot()+ggtitle(par_i)
#    geom_point(aes(color =  Plate, group=interaction(Plate,ID)),size=3)
  p
  
}

########################### SAVE FUNCTIONS
save_sampled_raw_data=function(dati,filename){ 
  save(dati,file=gsub("xlsx","Rdata",filename)) 
  dati=dati[sample(nrow(dati),nrow(dati)/50),] 
  save(dati,file=gsub("\\.xlsx","_sampled.Rdata",filename))
  NULL 
} 

library(sjPlot) 
library(ggplot2) 
theme_set(theme_sjplot()) 

import_data_add_group <- function(filename,tab_txt){ 
  print(paste0("Importing file ",filename)) 
  
  library(readr)
  dati <- read_csv2(filename,show_col_types = FALSE) 
  print(("and preprocessing it.")) 
  
  # print(dim(dati)) 
  
  if(!("Plate"%in%names(dati))) dati$Plate=filename
  dati <- add_groups(dati,tab_txt) 
  # dati=dati[,-grep("Nuclei",names(dati))] 
  save_sampled_raw_data(dati,filename) 
  
  D=aggregate(Nuclei_length ~ Group+Plate+Concentration+Compound+Row+Column, dati,  FUN=length) 
  D$Counts=D$Nuclei_length 
  D$Nuclei_length<- NULL 
  D 
} 

######## CREAZIONE GRUPPI:  

add_groups <- function(dati,tab_txt){ 
  
  library(readr)
  tab <- as.data.frame(read_csv(tab_txt,show_col_types = FALSE))
  #tab=read.csv(text=tab_txt) 
  
  #nomi=names(tab)[-(1:2)] 
  
  
  res=apply(tab,1,function(x){#browser() 
    res_grp=lapply(3:5, function(i_col){ 
      xx=as.numeric(strsplit(x[i_col]," - ")[[1]]) 
      in_range(dati[,names(x)[i_col]],lims = xx) 
    }  
    ) 
    temp=apply(as.data.frame(res_grp),1,prod) 
  }) 
  
  temp=rowSums(res) 
  if(any(temp>1)) warning(paste0("There are ",sum(temp>1)," cells belonging to more than one group. Please check the definition of groups!! (now I set them no groups)")) 
  res[temp>1,]=0 
  
  dati$Group=(res%*%(as.numeric(tab$ID)))[,1] 
  tab$Group=gsub(" ","",tab$Group) 
  dati$Group=factor(dati$Group) 
  
  
  names(dati)[names(dati)==names(tab)[3]] = "Nuclei_length"
  names(dati)[names(dati)==names(tab)[4]] = "Nuclei_intensity"
  names(dati)[names(dati)==names(tab)[5]] = "Nuclei_Ratio_W_L"
  #rownames(tab)=tab$ID 
  
  for (l in tab$ID) levels(dati$Group)[levels(dati$Group)==l]=tab[l,"Group"] 
  
  levels(dati$Group)[levels(dati$Group)=="0"]="0:None" 
  
  #    
  # dati$Group3=dati$Group 
  # levels(dati$Group3)[6]="Large" 
  # levels(dati$Group3)[7]="Small" 
  # levels(dati$Group3)[2:5]="Normal" 
  #  
  # print(table(dati$Group3)) 
  #  
  print(table(dati$Group)) 
  
  #  print(table(dati$Group))   
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


