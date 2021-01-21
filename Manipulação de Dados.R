getwd()
setwd("C:\\Users\\vinib\\Desktop\\Arquivos-Analise-de-Dados-main")
download.file(fileURL, destfile = "C:\\Users\\vinib\\Desktop\\Arquivos-Analise-de-Dados-main\\arquivo.csv", method = "curl")
list.files("C:\\Users\\vinib\\Desktop\\Arquivos-Analise-de-Dados-main\\")
V1 <- read.table("pica-pau.txt",header = TRUE, sep = "",dec = ".")#dados do brasil, usa virgula no dec
read.csv("arquivo.csv")
str()#estrutura do dataset
unique()#vê os elementos unicos
levels()#usado para manipular os niveis do dataset
#converter factor para caracter e então para numerico, se necessario
#Remove valores duplicados
which(duplicated("Caracol_data"))
index<- which(duplicated("Caracol_data"))
Caracol_data<-Caracol_data[-index,]
which(duplicated("Caracol_data"))
#Exportar dados
write.csv()
write.table()


#Manipulação Básica de Dados
#Q1
download.file("https://www.dropbox.com/s/w4xv9urbowbig3s/catsM.csv?raw=1" , destfile = "C:\\Users\\vinib\\Desktop\\Arquivos-Analise-de-Dados-main\\catsM.csv", method = "libcurl")
x <- read.table("catsM.csv",header = T,sep =",",dec=".")
mean(as.numeric(x$Bwt))#2.9
#Q2
download.file("https://www.dropbox.com/s/9wnr69i6bjhqyct/Snail_feeding.csv?raw=1" , destfile = "C:\\Users\\vinib\\Desktop\\Arquivos-Analise-de-Dados-main\\Caracol_data_checked.csv", method = "libcurl")
y <- read.csv("Caracol_data_checked.csv",header = T,strip.white = T,na.strings = "")
y <- y[,1:7]
str(y)
y$Sex<-as.factor(y$Sex)
y$Size<-as.factor(y$Size)
unique(y$Sex)
levels(y$Sex)
#levels(y$Sex)[2] <-"female"
#levels(y$Sex)[3] <-"male"
#levels(y$Sex)[5] <-"male"
y$Distance<-as.numeric(y$Distance)
which(is.na(y$Distance))
y[682,"Distance"] <-0.58
y[755,"Distance"] <-0.356452
which(duplicated(y))
index<- which(duplicated(y))
y<-y[-index,]
which(duplicated(y))
summary(y)
y[which(y$Depth >2),]
y[8,6] <- 1.62

mean(y$Depth)#1.507601
#Q3
y1<-y[(which(y$Size=='small'& y$Sex=='female')),]
max(y1$Distance)#1
#Q4
download.file("https://www.dropbox.com/s/jci311cfsj6uva7/Sparrows.csv?raw=1" , destfile = "C:\\Users\\vinib\\Desktop\\Arquivos-Analise-de-Dados-main\\Sparrows.csv", method = "libcurl")
z<-read.table(file = "Sparrows.csv", header = TRUE,sep=",")#Faltou o argumento sep=",".
str(z)
z1<-z[which(z$Species=="SSTS"),]
summary(z1)#29.2, 33.5
#Q5
which(duplicated(z))#23  37 140

str(z)
summary(z)
z$Sex<-as.factor(z$Sex)
levels(z$Sex)
levels(z$Sex)[5] <-"Male"

z2<-z[which(z$Sex=="Male"),]
mean(z2$Tarsus)#21.12007, 21.62668
#Q6
Sparrows<-Sparrows[-which(is.na(Sparrows$Wing)),]#Resposta
Sparrows[64,3]<-59
Sparrows[250,3]<-56.5
Sparrows[806,3]<-57
summary(Sparrows)
mean(Sparrows$Wing)#57.86205
Sparrows_Ordenado<-Sparrows[order(Sparrows$Wing,Sparrows$Head),]#Resposta


#Manipulação Avançada de Dados

merge(x=,y=,by="",all=T)
aggregate(formula = dv ~ iv, FUN= fun,subset = , data=df)
#dv é a variavel e iv é o grupo
#fun é a função que deseja aplicar
#subset(opcional) dá uma condição aos subsets
#data é o objeto dataframe contendo dv e iv

#pacote dplyr
install.packages("dplyr")
library(dplyr)
install.packages("readr")
library(readr)

filter()#filtra linhas
select()#retorna as colunas desejadas, se utilizar head(select()), retorna apenas as 6 primeiras
mutate()#usada para criar nova coluna
arrange()#usada para ordenar colunas
# o %>% concatena varias funções

#Q1
install.packages("dplyr")
library(dplyr)
df=data.frame(Theoph)
#Q1
df%>%select(Dose)#Resposta
#Q2
df%>%filter(Dose>5)#Resposta
#Q3
df%>%slice(c(10:20))#Resposta
#Q4
df%>%filter(Dose>5&Time>mean(Time))#Resposta
#Q5
df%>%arrange(desc(Wt))#Resposta
#Q6
df%>%arrange(Wt,desc(Time))
#Q7
df%>%mutate(tendencia=Time-mean(Time))
#Q8
df%>%summarise(max(conc))
#Q9
download.file("https://www.dropbox.com/s/73bp8dl8nph6ufz/L_UNIQUE_CARRIERS.csv_?raw=1" , destfile = "C:\\Users\\vinib\\Desktop\\Arquivos-Analise-de-Dados-main\\unique.csv", method = "libcurl")
mylookup<-read.table(file = "unique.csv", header = TRUE,quote="\"", sep = ",")#Faltou o argumento sep=",".
download.file("https://www.dropbox.com/s/gi59a1nq3ga9gb7/673598238_T_ONTIME_REPORTING.csv?raw=1" , destfile = "C:\\Users\\vinib\\Desktop\\Arquivos-Analise-de-Dados-main\\time.csv", method = "libcurl")
mydf<-read.table(file = "time.csv", header = TRUE,quote="\"", sep = ",")#Faltou o argumento sep=",".

join_df <- merge ( mydf , mylookup , by.x = "OP_UNIQUE_CARRIER" , by.y = "Code" , all.x = TRUE , all.y = FALSE ) 

joined_tibble <- left_join(mydf, mylookup, by = c("OP_UNIQUE_CARRIER" = "Code"))
#MAIOR ATRASO: American Airlines Inc.
#MAIS ATRASA MEDIA:Southwest Airlines Co.
#MENOS ATRASA MEDIA:Hawaiian Airlines Inc.
#PROP:American Airlines Inc.
summary(join_df)

join_df$Description<-as.factor(join_df$Description)

Atrasos <- join_df %>%
  filter(!is.na(DEP_DELAY_NEW)&DEP_DELAY_NEW>0) %>%
  group_by(Description) %>%
  summarise(atrasos = sum(DEP_DELAY_NEW > 0))

#223309
#0.0036547163
#2054610
#37300465

