##################################################################
#Libreria
##################################################################
library(tidyverse)
library("writexl")  
library(openxlsx)
library(caret)
library(parallel)
##################################################################
#lee los datos 
fn <- "https://www.opt.math.tugraz.at/qaplib/data.d/qapdata.tar.gz"
download.file(fn,destfile="D:/segundo cuatrimestre/ICO/Tarea 2/BD/tmp.tar.gz")
untar("D:/segundo cuatrimestre/ICO/Tarea 2/BD/tmp.tar.gz",list=TRUE)  ## check contents

untar("D:/segundo cuatrimestre/ICO/Tarea 2/BD/tmp.tar.gz",files="lipa20a.dat",exdir="D:/segundo cuatrimestre/ICO/Tarea 2/BD")
untar("D:/segundo cuatrimestre/ICO/Tarea 2/BD/tmp.tar.gz",files="kra30a.dat",exdir="D:/segundo cuatrimestre/ICO/Tarea 2/BD")
untar("D:/segundo cuatrimestre/ICO/Tarea 2/BD/tmp.tar.gz",files="tai35b.dat",exdir="D:/segundo cuatrimestre/ICO/Tarea 2/BD")
untar("D:/segundo cuatrimestre/ICO/Tarea 2/BD/tmp.tar.gz",files="tho40.dat",exdir="D:/segundo cuatrimestre/ICO/Tarea 2/BD")
untar("D:/segundo cuatrimestre/ICO/Tarea 2/BD/tmp.tar.gz",files="sko100d.dat",exdir="D:/segundo cuatrimestre/ICO/Tarea 2/BD")
untar("D:/segundo cuatrimestre/ICO/Tarea 2/BD/tmp.tar.gz",files="had12.dat",exdir="D:/segundo cuatrimestre/ICO/Tarea 2/BD")


lab=paste0('D:/segundo cuatrimestre/ICO/Tarea 2/BD/',c('lipa20a','kra30a','tai35b','tho40','sko100d','had12'),'.dat')
lab.sln=paste0('D:/segundo cuatrimestre/ICO/Tarea 2/BD/',c('lipa20a','kra30a','tai35b','tho40','sko100d','had12'),'.sln')



#lee las soluciones
fn <- "https://www.opt.math.tugraz.at/qaplib/soln.d/qapsoln.tar.gz"
download.file(fn,destfile="D:/segundo cuatrimestre/ICO/Tarea 2/BD/sol.tar.gz")
untar("D:/segundo cuatrimestre/ICO/Tarea 2/BD/sol.tar.gz",list=TRUE)  ## check contents

untar("D:/segundo cuatrimestre/ICO/Tarea 2/BD/sol.tar.gz",files="lipa20a.sln",exdir="D:/segundo cuatrimestre/ICO/Tarea 2/BD")
untar("D:/segundo cuatrimestre/ICO/Tarea 2/BD/sol.tar.gz",files="kra30a.sln",exdir="D:/segundo cuatrimestre/ICO/Tarea 2/BD")
untar("D:/segundo cuatrimestre/ICO/Tarea 2/BD/sol.tar.gz",files="tai35b.sln",exdir="D:/segundo cuatrimestre/ICO/Tarea 2/BD")
untar("D:/segundo cuatrimestre/ICO/Tarea 2/BD/sol.tar.gz",files="tho40.sln",exdir="D:/segundo cuatrimestre/ICO/Tarea 2/BD")
untar("D:/segundo cuatrimestre/ICO/Tarea 2/BD/tsol.tar.gz",files="sko100d.sln",exdir="D:/segundo cuatrimestre/ICO/Tarea 2/BD")


datos=function(num_dat){
  data <- read.delim(file = lab[num_dat], header = FALSE,sep='')
  data=as.matrix(data)
  n=as.numeric(data[1,1]) 
  d=matrix(0,n,n)
  w=d
  n1= as.numeric(n/dim(data)[2]) 
  tem=2
  tem1=n*n/dim(data)[2]+2
  for (i in 1:(n)) {
    d[i,]=matrix(t(data[tem:(tem+n1-1),]),nrow=1,ncol=n)
    tem=tem+n1
    w[i,]=matrix(t(data[tem1:(tem1+n1-1),]),nrow=1,ncol=n)
    tem1=tem1+n1
  }
  return(list(d=d,w=w))
}


#determina el fitness

fitness=function(s,d,w){
  salida=0
  for (i in 1:(length(s))) {
    for (j in (1):length(s)) {
      salida=salida+d[i,j]*w[s[i],s[j]]
    }
  }
  return(salida)
}

#inicializa la poblacion

inicilization=function(n,d,w){
  output=matrix(0,300,n+1)
  output[1,1:n]=sample(n,n)
  output[1,n+1]=fitness(output[1,1:n],d,w)
  temp=str_c(output[1,1:n], collapse = ".")
  i=2
  while (i!=301) {
    tem1=sample(n,n)
    if(!(str_c(tem1, collapse = ".")%in%temp)){
      output[i,1:n]=tem1
      output[i,n+1]=fitness(output[i,1:n],d,w)
      i=i+1
    }
  }
  return(output)
}



selecciona=function(output,sel){
  n=dim(output)[2]
  n1=dim(output)[1]
  aabb=c(3:8)
  met=sample(aabb,1,prob = sel[1,aabb]/sum(sel[1,aabb]))
  ordenado=order(output[,n],decreasing = T)
  if(met%in%c(1,3,5,7)){
    if(met==1){
      salida=c(ordenado[300],sample(n1,4))
    }
    if(met==3){
      salida=sample(n1,5)
    }
    if(met==5){
      salida=sample(n1,5,prob =(1/output[,n])/sum(1/output[,n]))
    }
    if(met==7){
      salida=sample(n1,5,prob=ordenado/(n1*(n1+1)/2))
    }
  }else{
    if(met==2){
      salida=c(ordenado[300],sample(n1,9))
    }
    if(met==4){
      salida=sample(n1,10)
    }
    if(met==6){
      salida=sample(n1,10,prob=(1/output[,n])/sum(1/output[,n]))
    }
    if(met==8){
      salida=sample(n1,10,prob=ordenado/(n1*(n1+1)/2))
    }
  }
  return(list(seleccion=salida,met=met))
}


#crusamiento habitual
crosorver1=function(y1,y2){
  n=length(y1)
  corte=sample(n-2,1)
  z1=y1[1:corte]
  z2=y2[1:corte]
  tem1=c(y1[(corte+1):n],y1[1:corte])
  tem2=c(y2[(corte+1):n],y2[1:corte])
  z1=c(z1,tem2[!(tem2%in%z1)])
  z2=c(z2,tem1[!(tem1%in%z2)])
  return(list(son1=c(z1),son2=c(z2)))
}


#crusamiento doble
crosorver2=function(y1,y2){
  y=crosorver1(y1,y2)
  y1=y$son1
  y2=y$son2
  return(crosorver1(y1,y2))
}

# #crea una mutacion cambia solo un elemento
# motation1=function(y){
#   n=length(y)
#   x=sample(n,2)
#   y[x]=c(y[x[2]],y[x[1]])
#   return(y)
# }

#crea una mutacion cambia elementos segun ensallo bernouly
motation1=function(y){
  n=length(y)
  x=sample(n,2*sum(rbinom(n, 1,runif(1, min = 1/300, max = 1/length(y)))))
  n=length(x)
  if(n>0){
    for (i in seq(1,n,2)) {
      y[x[i:(i+1)]]=c(y[x[i+1]],y[x[i]])
    }
  }
  return(y)
}

#invierte un segmento
motation2=function(y){
  n=length(y)
  x=sample(n,2)
  x=x[order(x)]
  y[x[1]:x[2]]=y[x[2]:x[1]]
  return(y)
}



#riega un segmento
motation3=function(y){
  n=length(y)
  x=sample(n,2)
  x=x[order(x)]
  y[x[1]:x[2]]=y[sample(x[1]:x[2],length(x[1]:x[2]))]
  return(y)
}

#actualiza la poblacion segun las caracteristicas de los hijos
actualization= function(output,y,ayuda){
  temp=ayuda
  n=dim(output)[2]
  maximo=max(output[,n])
  posicion=which.max(output[,n])
  if(y[n]<maximo){
    if(sum(str_c(y[1:(n-1)], collapse = ".")==temp)==0){
      output[posicion,]=y
      temp[posicion]=str_c(y[1:(n-1)], collapse = ".")
    }
  }
  return(list(ayuda=temp,output=output))
}

#caracteristicas de las soluciones
disty1y2=function(y1,y2){
  salida=dist(rbind(y1, y2))/dist(rbind(1:length(y1),length(y1):1))
  return(salida)
}

disty1cent=function(y1,y2){
  salida=dist(rbind(y1, y2))/dist(rbind(1:floor(length(y1)/2),floor(length(y1)/2):1))
  return(salida)
}


vect_caracteris=function(y1){
  n=length(y1)
  n1=floor(n/2)
  salida=matrix(0,1,9)
  colnames(salida)=c('disty10','areay1','disy1cen','media_dist','reactarr','rectaba','trianarrib','triangabaj','recta')
  salida[1,1]=disty1y2(y1,1:length(y1))
  salida[1,2]=n*(n+1)/2-y1[1]/2-y1[n]/2
  minimo=n*(n+1)/2-1/2-1
  maximo=n*(n+1)/2-n/2-(n-1)/2
  salida[1,2]=(salida[1,2]-minimo)/(maximo-minimo)
  salida[1,3]=disty1cent(y1[1:n1],y1[(n-n1+1):n])
  
  for (i in 2:n) {
    salida[1,4]=abs(y1[i]-y1[i-1])
    if(y1[i]>y1[i-1]){
      salida[1,5]=salida[1,5]+1
    }else{
      salida[1,6]=salida[1,6]+1
    }
    if(i<n){
      if(abs(y1[i]-y1[i-1])==1&abs(y1[i]-y1[i+1])==1){
        salida[1,9]=salida[1,9]+1
      }else{
        if(y1[i]>y1[i-1]&y1[i]>y1[i+1]){
          salida[1,7]=salida[1,7]+1
        }else{
          salida[1,8]=salida[1,8]+1
        }
      }
    }
  }
  salida[1,4]=(n-1)/salida[1,4]
  
  return(salida)
}















qap_simple_inicial=function(d,w){
  sel=as.data.frame(t(rep(1,8)))
  colnames(sel)=c('merjor5','mejor10','aleatorio5','aleatorio10','propfit5','propfit10','propfit5o','propfit10o')
  sel_crusa=as.data.frame(t(rep(1,2)))
  colnames(sel_crusa)=c('crusa1','crusa2')
  sel_muta=as.data.frame(t(rep(1,2,3)))
  colnames(sel_muta)=c('muta1','muta2','muta3')
  salida=matrix(0,1,12)
  colnames(salida)=c('disty10','areay1','disy1cen','media_dist','reactarr','rectaba','trianarrib','triangabaj','recta','f1','f2','val')
  
  n=dim(d)[1]
  output=inicilization(n,d,w)
  conteo=300
  
  ayuda=c()
  for (i in 1:300) {
    ayuda=c(ayuda,str_c(output[i,1:n], collapse = "."))
  }
  
  while(conteo<1000){
    temp1=selecciona(output,sel)
    temp=temp1$seleccion
    temp=temp[order(output[temp,n+1])]
    padres_temporales=output[temp[c(1,2)],]
    
    alet1=runif(1)
    crusar=0
    if(alet1<0.9){
      crusar=sample(1:2,1,prob=sel_crusa/sum(sel_crusa))
      if (crusar==1) {
        hijos=crosorver1(padres_temporales[1,1:n],padres_temporales[2,1:n])
        hijo1=hijos$son1
        hijo2=hijos$son2
      }else{
        hijos=crosorver2(padres_temporales[1,1:n],padres_temporales[2,1:n])
        hijo1=hijos$son1
        hijo2=hijos$son2
      }
    }
    
    alet1=runif(1)
    muta=0
    if(alet1<0.2){
      muta=sample(1:3,1,prob=sel_muta/sum(sel_muta))
      if(crusar!=0){
        if(muta==1){
          hijo1=motation1(hijos$son1)
          hijo2=motation1(hijos$son2)
        }
        if(muta==2){
          hijo1=motation2(hijos$son1)
          hijo2=motation2(hijos$son2)
        }
        if(muta==3){
          hijo1=motation3(hijos$son1)
          hijo2=motation3(hijos$son2)
        }
      }else{
        if(muta==1){
          hijo1=motation1(padres_temporales[1,1:n])
          hijo2=motation1(padres_temporales[2,1:n])
        }
        if(muta==2){
          hijo1=motation2(padres_temporales[1,1:n])
          hijo2=motation2(padres_temporales[2,1:n])
        }
        if(muta==3){
          hijo1=motation3(padres_temporales[1,1:n])
          hijo2=motation3(padres_temporales[2,1:n])
        }
      }
    }
    
    if(muta+crusar>0){
      
      fit1=fitness(hijo1,d,w)
      fit2=fitness(hijo2,d,w)
      tem11=c(vect_caracteris(hijo1),padres_temporales[1,1+n],padres_temporales[2,1+n])
      tem12=c(vect_caracteris(hijo2),padres_temporales[1,1+n],padres_temporales[2,1+n])
      if(sum(c(padres_temporales[1,1+n],padres_temporales[2,1+n])>=fit1)>0){
        tem11=c(tem11,1)
      }else{tem11=c(tem11,0)}
      
      if(sum(c(padres_temporales[1,1+n],padres_temporales[2,1+n])>=fit2)>0){
        tem12=c(tem12,1)
      }else{tem12=c(tem12,0)}
      salida=rbind(salida,tem11,tem12)
      
      conteo=conteo+2
      if(sum(max(padres_temporales[,n+1])>=c(fit1,fit2))>0){
        sel[1,temp1$met]=sel[1,temp1$met]+1
        if(muta>0){
          sel_muta[1,muta]=sel_muta[1,muta]+1
        }
        if(crusar>0){
          sel_crusa[1,crusar]=sel_crusa[1,crusar]+1
        }
      }
      tem1=actualization(output,c(hijo1,fit1),ayuda)
      ayuda=tem1[["ayuda"]]
      output=tem1[["output"]]
      
      tem1=actualization(output,c(hijo2,fit2),ayuda)
      ayuda=tem1[["ayuda"]]
      output=tem1[["output"]]
    }
    
  }
  #str_c(c('disty10','areay1','disy1cen','media_dist','reactarr','rectaba','trianarrib','triangabaj','recta','f1','f2','val'), collapse = "+")
  salida=as.data.frame(salida[-1,])
  glm.fit <- glm(val ~ disty10+areay1+disy1cen+media_dist+reactarr+rectaba+trianarrib+triangabaj+recta+f1+f2, data =salida, family = binomial)
  return(list(glm.fit=glm.fit,output=output,salida=salida,sel=sel,sel_crusa=sel_crusa,sel_muta=sel_muta))
}











qap_complejo=function(d,w){
  abc11=qap_simple_inicial(d,w)
  sel=abc11$sel
  sel_crusa=abc11$sel_crusa
  sel_muta=abc11$sel_muta
  salida=abc11$salida
  output=abc11$output
  glm.fit=abc11$glm.fit
  n112=2
  n113=2
  n=dim(d)[1]
  fit1=1000000
  fit2=1000000
  conteo=1000
  
  ayuda=c()
  for (i in 1:300) {
    ayuda=c(ayuda,str_c(output[i,1:n], collapse = "."))
  }
  
  while(conteo<50000){
    temp1=selecciona(output,sel)
    temp=temp1$seleccion
    temp=temp[order(output[temp,n+1])]
    padres_temporales=output[temp[c(1,2)],]
    
    alet1=runif(1)
    crusar=0
    if(alet1<0.9){
      crusar=sample(1:2,1,prob=sel_crusa/sum(sel_crusa))
      if (crusar==1) {
        hijos=crosorver1(padres_temporales[1,1:n],padres_temporales[2,1:n])
        hijo1=hijos$son1
        hijo2=hijos$son2
      }else{
        hijos=crosorver2(padres_temporales[1,1:n],padres_temporales[2,1:n])
        hijo1=hijos$son1
        hijo2=hijos$son2
      }
    }
    
    alet1=runif(1)
    muta=0
    if(alet1<0.2){
      muta=sample(1:3,1,prob=sel_muta/sum(sel_muta))
      if(crusar!=0){
        if(muta==1){
          hijo1=motation1(hijos$son1)
          hijo2=motation1(hijos$son2)
        }
        if(muta==2){
          hijo1=motation2(hijos$son1)
          hijo2=motation2(hijos$son2)
        }
        if(muta==3){
          hijo1=motation3(hijos$son1)
          hijo2=motation3(hijos$son2)
        }
      }else{
        if(muta==1){
          hijo1=motation1(padres_temporales[1,1:n])
          hijo2=motation1(padres_temporales[2,1:n])
        }
        if(muta==2){
          hijo1=motation2(padres_temporales[1,1:n])
          hijo2=motation2(padres_temporales[2,1:n])
        }
        if(muta==3){
          hijo1=motation3(padres_temporales[1,1:n])
          hijo2=motation3(padres_temporales[2,1:n])
        }
      }
    }
    
    if(muta+crusar>0){
      tem11=as.data.frame(t(c(vect_caracteris(hijo1),padres_temporales[1,1+n],padres_temporales[2,1+n],0)))
      tem12=as.data.frame(t(c(vect_caracteris(hijo2),padres_temporales[1,1+n],padres_temporales[2,1+n],0)))
      colnames(tem11)=colnames(salida)
      colnames(tem12)=colnames(salida)
      tem13=rbind(tem11,tem12)
      options(warn = -1)
      # sal13=predict.glm(glm.fit, tem13, type = "response")
      sal13=c(1,1)
      alet1=runif(1)
      # ttt123=str_c(hijo1, collapse = ".")
      # if(ttt123%in%historial){
      #   alet1=sal13[1]+1
      # }
      
      if(alet1<=sal13[1]){
        fit1=fitness(hijo1,d,w)
        # historial=c(historial,ttt123)
        if(sum(c(padres_temporales[1,1+n],padres_temporales[2,1+n])>=fit1)>0){
          tem13[1,dim(tem13)[2]]=1
        }
        
        if(conteo<n112){salida=rbind(salida,tem13[1,])}
        
        
        conteo=conteo+1
        if(sum(max(padres_temporales[,n+1])>=c(fit1))>0){
          sel[1,temp1$met]=sel[1,temp1$met]+1
          if(muta>0){
            sel_muta[1,muta]=sel_muta[1,muta]+1
          }
          if(crusar>0){
            sel_crusa[1,crusar]=sel_crusa[1,crusar]+1
          }
        }
        tem1=actualization(output,c(hijo1,fit1),ayuda)
        ayuda=tem1[["ayuda"]]
        output=tem1[["output"]]
        
      }
      
      
      alet1=runif(1)
      # ttt123=str_c(hijo2, collapse = ".")
      # if(ttt123%in%historial){
      #   alet1=sal13[2]+1
      # }
      if(alet1<=sal13[2]){
        fit2=fitness(hijo2,d,w)
        # historial=c(historial,ttt123)
        if(sum(c(padres_temporales[1,1+n],padres_temporales[2,1+n])>=fit2)>0){
          tem13[2,dim(tem13)[2]]=1
        }
        
        
        if(conteo<n112){salida=rbind(salida,tem13[2,])}
        
        conteo=conteo+1
        if(sum(max(padres_temporales[,n+1])>=c(fit2))>0){
          sel[1,temp1$met]=sel[1,temp1$met]+1
          if(muta>0){
            sel_muta[1,muta]=sel_muta[1,muta]+1
          }
          if(crusar>0){
            sel_crusa[1,crusar]=sel_crusa[1,crusar]+1
          }
        }
        tem1=actualization(output,c(hijo2,fit2),ayuda)
        ayuda=tem1[["ayuda"]]
        output=tem1[["output"]]
        
      }
      
    }
    if(conteo==1000|conteo==1001){
      sel[1,]=1
      sel_muta[1,]=1
      sel_crusa[1,]=1
    }
    # if(conteo%in%c(seq(12000,50000,7000),seq(12001,50000+1,7000))){
    #   sel_muta[1,]=1
    # }
    if(conteo%in%c(35000,35001)){
      output[order(output[,n+1])[251:300],]=inicilization(n,d,w)[1:50,]
    }
    # print(conteo)
    # if(1652%in%c(fit1,fit2)){break}
    # if(conteo%in%c(seq(2000,n113,1000),seq(2001,n113+1,1000))){
    #   glm.fit <- glm(val ~ disty10+areay1+disy1cen+media_dist+reactarr+rectaba+trianarrib+triangabaj+recta+f1+f2, data =salida, family = binomial)
    # }
    
  }
  return(list(glm.fit=glm.fit,output=output[order(output[,n+1]),],salida=salida,sel=sel,sel_crusa=sel_crusa,sel_muta=sel_muta))
}


sol3=function(d,w){
  output11=matrix(0,32,dim(d)+1+2)
  row.names(output11)=c(1:30,'VE','SD')
  for (i in 1:3) {
    print(i)
    t0=proc.time()
    temp=qap_complejo(d,w)
    tf=proc.time()
    output11[i,1]=(tf-t0)[3]
    b=temp$output
    output11[i,2:(dim(d)[1]+2)]=b[1,]
  }
  return(output11)
}








sol30=function(d,w){
  output11=matrix(0,32,dim(d)+1+2)
  row.names(output11)=c(1:30,'VE','SD')
  for (i in 1:30) {
    print(i)
    t0=proc.time()
    temp=qap_complejo(d,w)
    tf=proc.time()
    output11[i,1]=(tf-t0)[3]
    b=temp$output
    output11[i,2:(dim(d)[1]+2)]=b[1,]
  }
  return(output11)
}


acopla=function(abcd1){
  salida=abcd1[[1]][1:3,]
  for (i in 2:10) {
    salida=rbind(salida,abcd1[[i]][1:3,])
    
  }
  salida=rbind(salida,abcd1[[1]][29:30,])
  salida[31,1]=sum(salida[1:30,1])/30
  salida[31,22]=sum(salida[1:30,22])/30
  salida[32,1]=sd(salida[1:30,1])
  salida[32,22]=sd(salida[1:30,22])
  return(salida)
}


###########################################################

# s=c(19 ,17 ,7, 1, 5,9 ,10 ,12, 4, 16, 20, 6, 3 ,14 ,11 ,15, 13 ,8, 2 ,18)
# 
# data <- read.delim(file = lab.sln[1], header = FALSE,sep='')
# 
# 
# data <- read.delim(file = lab.sln[2], header = FALSE,sep='')
# s=c(as.numeric(data[2,]),as.numeric(data[3,1:13]))
# 
# data <- read.delim(file = lab.sln[3], header = FALSE,sep='')
# s=c(as.numeric(data[2,]),as.numeric(data[3,1:17]))
# 

#############################################################

#ejecucion
data=datos(1)
d=data$d
w=data$w


t0=proc.time()


cl <- makeCluster(detectCores()-2) # Crea un cluster con 4 núcleos
clusterExport(cl, c("sol3","motation1","motation2","motation3","actualization","disty1cent","disty1y2","vect_caracteris","crosorver1","crosorver2","selecciona","qap_complejo","str_c","fitness","qap_simple_inicial", "inicilization")) # Exporta la función "f" al cluster
clusterExport(cl, c("d",'w'))
# Ejecuta la función "f" en cada núcleo del cluster
abcd1 <- parLapply(cl, 1:10,function(x) {sol3(d,w)})
abcd1=acopla(abcd1)
stopCluster(cl) # Detiene el cluster

tf=proc.time()
(tf-t0)[3]
save(abcd1,file = "D:/segundo cuatrimestre/ICO/Tarea 2/result/dat_sin.RData")

data=datos(2)
d=data$d
w=data$w

cl <- makeCluster(detectCores()-2) # Crea un cluster con 4 núcleos
clusterExport(cl, c("sol3","motation1","motation2","motation3","actualization","disty1cent","disty1y2","vect_caracteris","crosorver1","crosorver2","selecciona","qap_complejo","str_c","fitness","qap_simple_inicial", "inicilization")) # Exporta la función "f" al cluster
clusterExport(cl, c("d",'w'))
# Ejecuta la función "f" en cada núcleo del cluster
abcd2 <- parLapply(cl, 1:10,function(x) {sol3(d,w)})
abcd2=acopla(abcd2)
stopCluster(cl) # Detiene el cluster


save(abcd1, abcd2,file = "D:/segundo cuatrimestre/ICO/Tarea 2/result/dat_sin.RData")
data=datos(3)
d=data$d
w=data$w

cl <- makeCluster(detectCores()-2) # Crea un cluster con 4 núcleos
clusterExport(cl, c("sol3","motation1","motation2","motation3","actualization","disty1cent","disty1y2","vect_caracteris","crosorver1","crosorver2","selecciona","qap_complejo","str_c","fitness","qap_simple_inicial", "inicilization")) # Exporta la función "f" al cluster
clusterExport(cl, c("d",'w'))
# Ejecuta la función "f" en cada núcleo del cluster
abcd3 <- parLapply(cl, 1:10,function(x) {sol3(d,w)})
abcd3=acopla(abcd3)
stopCluster(cl) # Detiene el cluster

# abcd3=sol30(d,w)
save(abcd1, abcd2,abcd3,file = "D:/segundo cuatrimestre/ICO/Tarea 2/result/dat_sin.RData")
data=datos(4)
d=data$d
w=data$w
cl <- makeCluster(detectCores()-2) # Crea un cluster con 4 núcleos
clusterExport(cl, c("sol3","motation1","motation2","motation3","actualization","disty1cent","disty1y2","vect_caracteris","crosorver1","crosorver2","selecciona","qap_complejo","str_c","fitness","qap_simple_inicial", "inicilization")) # Exporta la función "f" al cluster
clusterExport(cl, c("d",'w'))
# Ejecuta la función "f" en cada núcleo del cluster
abcd4 <- parLapply(cl, 1:10,function(x) {sol3(d,w)})
abcd4=acopla(abcd4)
stopCluster(cl) # Detiene el cluster
# abcd4=sol30(d,w)
save(abcd1, abcd2,abcd3,abcd4,file = "D:/segundo cuatrimestre/ICO/Tarea 2/result/dat_sin.RData")
data=datos(5)
d=data$d
w=data$w
cl <- makeCluster(detectCores()-2) # Crea un cluster con 4 núcleos
clusterExport(cl, c("sol3","motation1","motation2","motation3","actualization","disty1cent","disty1y2","vect_caracteris","crosorver1","crosorver2","selecciona","qap_complejo","str_c","fitness","qap_simple_inicial", "inicilization")) # Exporta la función "f" al cluster
clusterExport(cl, c("d",'w'))
# Ejecuta la función "f" en cada núcleo del cluster
abcd5 <- parLapply(cl, 1:10,function(x) {sol3(d,w)})
abcd5=acopla(abcd5)
stopCluster(cl) # Detiene el cluster
# abcd5=sol30(d,w)
save(abcd1, abcd2,abcd3,abcd4,abcd5,file = "D:/segundo cuatrimestre/ICO/Tarea 2/result/dat_sin.RData")
