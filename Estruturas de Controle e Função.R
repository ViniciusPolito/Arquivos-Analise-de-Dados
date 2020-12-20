
saida <- vector("double", ncol(airquality))

for(i in seq_along(airquality)){
  saida[i] <- mean(airquality[,i],na.rm =T)
}

#Loops e Família Apply

View(iris)
#Q1
apply(iris[ , 1:4], 1, FUN = mean)#Não
apply(iris, 2, FUN = mean)#Não
colMeans(iris)#Não
apply(iris[ , 1:4], 2, FUN = mean)#Okay
sapply(iris[ , 1:4], 2, FUN = mean)#Okay
mapply(iris[ , 1:4], mean)#Não
apply(iris[1:4, ], 2, FUN = mean)#Não
#Q2
tapply(iris$Petal.Length, iris$Species, mean)#Okay
tapply(iris[,3], iris$Species, mean)#Okay
with(iris, tapply(Petal.Length, Species, mean))#Okay
sapply(iris, 2, mean)#Não
mapply(iris$Petal.Length, iris$Species, mean)#Não
mean(iris$Petal.Length, iris$Species)#Não
#Q3
#Repeat
#Q4
while (T) {}#Infinito
#Q5
for (i in 1:4){ }#4
#Q6
for (i in 1:4){ break }#1
#Q7
for (i in 1:4){next }#4

print(i)
#Q8
mapply(rep,c("Rural","Amo"),10:1)
#Q9
for(i in 1:length(1:3)){
  for(j in 1:10){
    print(i+j-1)
  }
}
#i+j-1#Resposta
#Q10
student.df = data.frame (nome= c ("Sue", "Eva", "Henry", "Jan"), sexo= c ("f", "f", "m", "m"), anos= c (21,15,17,19))

student.df$menor<-ifelse(student.df$sexo=="m"&student.df$anos<18,"V","F")#Resposta
View(student.df)
#Q11
View(USArrests)
lapply(USArrests,sum)#Resposta
#Q12
x=0
a=0
b=-5

if(a>0){
  if(b <0){
    x=x+5
  }else if(a>5){
    x=x+4
  }else{
    x=x+3
  }
}else{
  x = x+2
}

print(x)#2
#Q13
x=0
while(x < 100){
  x=x+2
}
print(x)#100
#Funções
#Q1
y <-5

mult <- function(x,y){
  return(x*y)
}
mult(10)#ERRO
#Q2
y <-5

mult <- function(x){
  return(x*y)
}
mult(10)#50
#Q3
View(USArrests)

`%notin%` <- Negate(`%in%`)

prisoes <- function(estados,tiposPrisoes){
  if(is.null(estados) | is.null(tiposPrisoes)){
    print("Estado ou Tipo de Prisão está faltando")
  } else if(any(estados %notin% rownames(USArrests))){
    print("Estado Inválido")
  } else if(any(tiposPrisoes %notin% colnames(USArrests))){
    print("Tipo de Prisão Inválida")
  } else{
    estadosAux <- USArrests[estados,tiposPrisoes]
    return (sum(estadosAux))
  }
    
}

prisoes(estados="Tennessee", tiposPrisoes=c("Rape","Murder"))#40.1
#Q4
prisoes(estados=c("California ","Miami", "Arizona"), tiposPrisoes=("Assault"))#"Estado Inválido"
#Q5
prisoes(estados=c("Pennsylvania","Mississippi", "Nebraska"), tiposPrisoes=c("Rape","UrbanPop","Assault"))#693.5
#Q6
prisoes(estados=c("Vermont","Wisconsin", "Texas"), tiposPrisoes=c("Rape","Assalto"))#"Tipo de Prisão Inválida"


any(F,c("Rape","Assalto") %notin% colnames(USArrests))

#Q7

x<-c(7,9.1,4)
x<-c(1,3,7)
sum(x[x >min(x)])/2



minhasNotas_1 <- function(Exe_1, VA_1, Exe_2, Proj, VA_2, VA_3, Opt=factor(c(1,2,3,4)), threshold){
  
  #Tratar os NA nas notas para 0
  Exe_1[is.na(Exe_1)] <- 0
  Exe_2[is.na(Exe_2)] <- 0
  Proj[is.na(Proj)] <- 0
  
   #Verifica validade das notas  
  if((any(Exe_1>10)) | (any(Exe_1<0))){
    return("Notas Invalidas para 1VA")
  }
  if((any(Exe_2>10)) | (any(Exe_2<0))){
    return("Notas Invalidas para 2VA")
  }
  
  #Media aritmetica dos vetores
  EX1 <- (sum(Exe_1)/length(Exe_1))
  EX2 <- (sum(Exe_2)/length(Exe_2))
  PROJ <- (sum(Proj)/length(Proj))

  #Fazer os calculos das notas
  VA1 <-((EX1 + VA_1)/2)
  VA2 <-(((EX2*2) + (PROJ*5) + (VA_2*3))/10)
  aux <-c(VA1,VA2,VA_3)
  final <-(sum(aux[aux >min(aux)])/2)
  
  if(Opt==1){
    return (paste("Média da 1 VA foi",VA1, passa(VA1,threshold,1), sep=" "))
  }else if(Opt==2){
    return (paste("Média da 2 VA foi",VA2, passa(VA2,threshold,2), sep=" "))
  }else if(Opt==3){
    return(paste("Média da 3 VA foi",VA_3, passa(VA_3,threshold,3), sep=" "))
  }else{
   return (paste("Média da final foi",final, passa(final,threshold,4), sep=" "))
  }
}

passa <-function(VA,media,Opt=factor(c(1,2,3,4))){
  
  if(Opt!=4){
    if(VA<0){
      return("ERRO,NOTA ABAIXO DE 0")
    }else if(VA>10){
      return("ERRO,NOTA ACIMA DE 10")
    }else{
      if(VA<media){
        return("Nota abaixo da média")
      }else{
        return("Nota na média")
      }
    }
  }else{
    if(VA<0){
      return("ERRO,NOTA ABAIXO DE 0")
    }else if(VA>10){
      return("ERRO,NOTA ACIMA DE 10")
    }else{
      if(VA<media){
        return("Vai para Final")
      }else{
        return("Aprovado")
      }
    }
  }
}

minhasNotas_1(Exe_1=c(10,9,7,5,10,4,NA,6,7,8,10), VA_1=8, Exe_2=c(8,5,7,8), Proj=c(7,5), VA_2=7, VA_3=9, Opt=1, threshold=8)
#"Média da 1 VA foi 7.45454545454546 Nota abaixo da média"
#Q8
minhasNotas_1(Exe_1=c(10,9,7,5,NA,4,5,6,7,8,NA), VA_1=2, Exe_2=c(10,5.7,7.8,NA), Proj=4, VA_2=7, VA_3=9, Opt=2, threshold=5)
#"Média da 2 VA foi 5.275 Nota na média"
#Q9
minhasNotas_1(Exe_1=c(10,9,7,5,10,4,5,6,7,8,10), VA_1=8, Exe_2=c(8,5,7,8), Proj=c(4,5), VA_2=7, VA_3=9, Opt=2, threshold=8)
#"Média da 2 VA foi 5.75 Nota abaixo da média
#Q10
minhasNotas_1(Exe_1=c(NA,NA,7.5,1,10,2,5,6,7,8,10), VA_1=4, Exe_2=c(7,11,8.8,5,7,8), Proj=4, VA_2=7, VA_3=9, Opt=4, threshold=7)
#Nota(s) Inválida(s) para a Média Final!
#Q11
minhasNotas_1(Exe_1=c(10,9,7,5,10,4,5,6,7,8,10), VA_1=8, Exe_2=c(6,8,9,8,5,7,8), Proj=7, VA_2=7, VA_3=9, Opt=4, threshold=8) 
#"8.34090909090909 -- Aprovado!!"



