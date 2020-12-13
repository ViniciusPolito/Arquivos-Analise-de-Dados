#Vetores

xx <- c(TRUE, 2)
xx

x <- 1:4  
y <- 2:3
x*y

pessoa <-c("Maria","Joanna")
v1 <- c(42,38)
v2 <- c(102,173)
v3 <- c(38,71)
v4 <- c(2149,1554)
v5 <- c(97,370)

v1.n <-(v1-30)/6
v2.n <-(v1-155)/23
v3.n <-(v1-50)/8
v4.n <-(v1-1829)/274
v5.n <-(v1-75)/12

media <- (v1.n + v2.n +v3.n +v4.n +v5.n)/5
max(media)
pessoa[max(media)==media]

load("C:/Users/vinib/OneDrive/Área de Trabalho/R/vetor.RData")

summary(vetor01)
mean(vetor01)
mean(vetor01, na.rm = TRUE) 
sd(vetor01, na.rm = TRUE)

tamanho <- (vetor01 > 7.00 & vetor01 < 8.00)
length(which(tamanho))

tamanho <- (vetor01 > 9.00 | vetor01 < 1.00)
length(which(tamanho))


vetor01 <- vetor01[!is.na(vetor01)]

#Fatores, listas e matrizes
#Q1
gl(2,100,labels=c("M","F"))
#Q2 
drinks <- factor(c("beer","beer","wine","water"))

levels(drinks)

mean(drinks == "beer")

#Q3
drinks <- factor(c("beer","beer","wine","water"))

levels(drinks)[1] <- "water"

drinks

#Q4
lista_pessoas <- list(nomes=c("João", "Paula", "Maria", "Ingrid", "José", "Marcos"),pesos=c(80, 65, 70, 58, 78, 70),alturas=c(1.70, 1.66, 1.65, 1.60, 1.76, 1.70))

lista_pessoas$IMC <- (lista_pessoas$pesos/(lista_pessoas$alturas^2))

lista_pessoas

lista_pessoas[[1]][1]

#Q5 Char,Numb,Log.

#Q6
lista_pessoas[[4]][3]#25.71166

#Q7
airquality

lista<-lapply(airquality, function(x){mean(x)})

lista$Temp
#Q8
load("C:/Users/vinib/OneDrive/Área de Trabalho/R/chuvas.RData")
chuvas
mean(chuvas)#5.049933
#Q9
which.max(rowSums(chuvas))
#Q10
which.min(colSums(chuvas))
#Q11
aux<-chuvas["mun_81" , ]; aux<-sum(aux[1:10])#ok
aux<-chuvas[81 , ]; aux<-sum(aux[1:10])#ok
aux<-chuvas[81 , ]; aux<-sum(aux[seq(1,10,1)])#ok
aux<-chuvas["mun_81" ]; aux<-sum(aux[1:10])#no
aux<-chuvas["mun_81" , ]; aux<-sum(aux[c(1:10)])#ok

#DataFrames
mouse.color <- c('purple', 'red', 'yellow','brown')
mouse.weight <- c(23, 21, 18, 26)
mouse.info <- data.frame("colour" = mouse.color,"weight" = mouse.weight)
#Q1
mouse.info
str(mouse.info)#Dá estrutura do dado
#Q2
mouse.info[3,]
#Q3
mouse.info[,1]
#Q4
mouse.info[4,1]
#Q5
min(airquality$Ozone[airquality$Month == 5])#1
#Q6
x <-airquality[(airquality$Ozone > 25) & (airquality$Temp <90) ,]
x <- x[complete.cases(x), ]
mean(x$Solar.R)#215.2041
#Q7
y <- airquality[complete.cases(airquality),]
y#153
#Q8
genomas <- as.data.frame(read.csv("https://www.dropbox.com/s/vgh6qk395ck86fp/genomes.csv?dl=1"))
aux <- genomas[genomas$Chromosomes >40,]
#Q9
#Yangia,Vibrio tubiashii, Vibrio scophthalmi
#Q10
length(table(genomas$Groups))#310
#Q11
cancer_stats <- as.data.frame(read.csv("https://www.dropbox.com/s/g97bsxeuu0tajkj/cancer_stats.csv?dl=1"))
cancer_stats
#gall
#Q12
#Testis
with(cancer_stats, (Male.Deaths)/(Male.Cases))#taxa de morte
#Q13
#Pancreas
with(cancer_stats, (Female.Deaths)/(Female.Cases))#taxa de morte




