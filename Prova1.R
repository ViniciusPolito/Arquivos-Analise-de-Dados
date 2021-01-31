getwd()
setwd("C:\\Users\\vinib\\Desktop\\Arquivos-Analise-de-Dados-main")

install.packages("tidyr")
install.packages("dplyr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("AER")
install.packages("readr")
library("tidyr")
library("dplyr")
library("stringr")
library("ggplot2")
library("AER")
library("readr")


strg <- c("Voda 30", "bylinky 25", "ZEM 23", "zlAto 22")

paste(tolower(strg),"%",sep="")#Resposta 1
#5!eAZ

download.file("https://www.dropbox.com/s/ed4kb56305t1udj/gambler.txt?raw=1" , destfile = "C:\\Users\\vinib\\Desktop\\Arquivos-Analise-de-Dados-main\\gambler.txt", method = "libcurl")
gambler<-read_file("gambler.txt")
nchar(gambler)#2569

regex<-c("www.dogman.com", "http://rotterdam.com", "https://facebook.com", "httpx://sims.com", "fungame.http")

grep(pattern = "(^https)|(^http):", regex, value = TRUE)#(^https)|(^http):

download.file("https://www.dropbox.com/s/rl86524vniqb8fh/Forbes2000_V2.csv?raw=1" , destfile = "C:\\Users\\vinib\\Desktop\\Arquivos-Analise-de-Dados-main\\forbes.csv", method = "libcurl")
forbes <- read.table("forbes.csv",header = T,sep =",",dec=".")

summary(forbes)

ranqueamento <-function(ranque, categoria=NULL, opcao){
  if(opcao==1){
    x<-forbes[order(forbes$marketvalue,decreasing = T),]
    return(x[2,ranque])
  }else{
    x<-forbes[order(forbes$category,forbes$marketvalue,decreasing = T),]
    return(x[2,ranque])
  }
}
ranqueamento2 <-function(ranque, categoria=NULL, opcao){
  if(opcao==1){
    return(forbes[order(forbes$marketvalue,decreasing = T),])
  }else{
    return(forbes[order(forbes$category,forbes$marketvalue,decreasing = T),])
  }
}
ranqueamento(1,opcao=2)


#OTP Bank
#Media
#Chi Mei Optoelectronics
#Categoria não existe


help(diamonds)
View(diamonds)

dima<-diamonds[which(diamonds$cut=="Very Good" & diamonds$carat>0.7),]
mean(dima$price)

summary(dima)#6512

dima1<-diamonds[which(diamonds$carat>0.5),]
min(dima1$price)#H

dima2<-diamonds[which(diamonds$cut=="Premium"),]
dima0<-diamonds

#13791/53940 = 0.25567297

data("Fertility")

help("Fertility")

fertility<-Fertility[35:50,c("age","work")]

kids<-Fertility[which(Fertility$morekids=="yes" & Fertility$work>30),]#0.103191

muie<-Fertility[which(Fertility$age)]#0.5036608

download.file("https://www.dropbox.com/s/mqc1x7sc4cuukv0/Catfish.csv?raw=1" , destfile = "C:\\Users\\vinib\\Desktop\\Arquivos-Analise-de-Dados-main\\cat.csv", method = "libcurl")
cat <- read.table("cat.csv",header = T,sep =",",dec=".")

download.file("https://www.dropbox.com/s/0heut39hfpwf7jc/Treatment.csv?raw=1" , destfile = "C:\\Users\\vinib\\Desktop\\Arquivos-Analise-de-Dados-main\\treatment.csv", method = "libcurl")
treatment <- read.table("treatment.csv",header = T,sep =",",dec=".")

str(cat)
str(treatment)

#72.1
#Catfish_Treatment$AcimaMedia<-ifelse(Catfish_Treatment$Weight>mean(Catfish_Treatment$Weight),'V','F')
#Todos os Tanks possuem a mesma média.


meninas<-c("Maria","Teresa","Francisca","Joaquina")
abdominal<-c(42,38,50,40)
salto<-c(70,173,150,140)
flexao<-c(40,71,40,80)
correr<-c(1500,1554,1900,2093)
conhecimento<-c(97,70,40,100)

abdominal.n<-((abdominal-(30))/6)
salto.n<-((salto-(155))/23)
flexao.n<-((flexao-(50))/8)
correr.n<-((correr-(1829))/274)
conhecimento.n<-((conhecimento-(75))/12)

resultados<-((abdominal.n+salto.n+flexao.n+correr.n+conhecimento.n)/5)
ganhadora<-max((abdominal.n+salto.n+flexao.n+correr.n+conhecimento.n)/5)

meninas[ganhadora==resultados]#JOAQUINA

summary(airquality)

set.seed(10)
df1 <- data.frame(matrix(data = sample(100,100,replace=TRUE), ncol = 10))
df1[df1>80] <- NA


casosCompletos<- function( df,  linhas=NA){
  df1 <- df[!(apply(df1, 1, function(x) any(is.na(x)))),]
  return(length(df1))
}

casosCompletos(df=airquality)#111

casosCompletos<- function( df,  linhas=NA){
  ifelse(is.na(linhas), return(sum(complete.cases(df))), return(sum(complete.cases(df[linhas,]))))
}

#177
