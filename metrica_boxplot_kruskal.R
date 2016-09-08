
#Grafico boxplot con comparación de medianas de kruskall wallis para metricas
#Hugo Andrés Dorado B.                             09/07/2016
#Lectura de graficos


graficoBoxplotMetricas <- function(metricas,ggtitle,colr="white"){
   
  library(reshape)
  library(ggplot2)
  library(agricolae)
  library(car)
  library(stringr)
  library(cowplot)
   
  inpts <- row.names(metricas)
  inpts <- inpts[-length(inpts)]
  inpts <- c(inpts,"Variety")
  row.names(metricas) <- inpts
  metricas1 <- metricas

  metricas1 <- t(metricas1)


  sorVars <- names(sort(apply(metricas1,2,median),decreasing = F))

  vSort <- as.data.frame(metricas1[,sorVars])

  newV <-  melt(vSort)

  noParameOut <- kruskal(newV$value,newV$variable,group = T)

  groupsData <- data.frame(noParameOut$groups$trt,noParameOut$groups$M)

  groupsData$noParameOut.groups.trt <- str_replace_all(groupsData$noParameOut.groups.trt, pattern=" ", repl="")

  groupsData$max <- sort(tapply(newV$value,newV$variable,max),decreasing = T)+4

  newV1 <- merge(newV,groupsData,by.x="variable",by.y="noParameOut.groups.trt",all.x=T,all.y=F)


  m <- ggplot(newV1, aes(x=variable, y=value))

  m <- m + geom_boxplot(fill=colr) + ylab("Importance")+ xlab("Input variable")+
  theme_bw() +
  ggtitle(ggtitle) +   theme(axis.text.x = element_text(angle=0, hjust=0.5, vjust=0),plot.title = element_text(vjust=3,size=10))+ 
  coord_flip()+ geom_text(aes(y = max,label = noParameOut.groups.M))

  graficoFinal <- ggdraw(switch_axis_position(m, 'x'))

  return(graficoFinal)

}

#Ejemplo
# rm(list=ls())
# 
# setwd(".")
# 
# metricas <- read.csv("weighMatrix.csv",row.names=1)
# 
# r.squart <- round(sum(apply(metricas,1,mean)),2)
# 
# ggtitle <- paste("Riego_546", "Importance of variables (with a mean R2 of", round(r.squart,2), "%)")
# 
# g <- graficoBoxplotMetricas(metricas,ggtitle,colr = "aquamarine")
# 
# g
# 
# #,width = 560, hei = 800, pointsize = 20
# 
# tiff(paste0(namVar),width  = 5.5,height = 6.5,res=300,units='in')
# g
# dev.off()








