#N número de genes
#k número de grupos
#P_1 población de agentes capa 1


# #automatic install of packages if they are not installed already
# list.of.packages <- c(
#   "foreach",
#   "doParallel",
#   "ranger",
#   "palmerpenguins",
#   "tidyverse",
#   "kableExtra"
# )
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# 
# if(length(new.packages) > 0){
#   install.packages(new.packages, dep=TRUE)
# }
# 
# #loading packages
# for(package.i in list.of.packages){
#   suppressPackageStartupMessages(
#     library(
#       package.i, 
#       character.only = TRUE
#     )
#   )
# }
# 
# #loading example data
# data("penguins")
# 
# parallel::detectCores()
# 
# n.cores <- parallel::detectCores() - 1
# 
# #create the cluster
# my.cluster <- parallel::makeCluster(
#   n.cores, 
#   type = "PSOCK"
# )
# 
# #check cluster definition (optional)
# print(my.cluster)
# 
# 
# #register it to be used by %dopar%
# doParallel::registerDoParallel(cl = my.cluster)
# 
# #check if it is registered (optional)
# foreach::getDoParRegistered()
# 
# foreach::getDoParWorkers()




##################################################

crearpoblacioninicial<-function(N,k,P_1){
  pob<-matrix(NA,nrow=P_1,ncol=(k+3))
  for(i in 1:P_1){
    pob[i,1:k]<-sample(1:N,k,replace=F)
  }
  return(pob)
}

# evaluarporfila<<-function(N,k,f,No_evaluados,Me,Mb,alpha){
#   cercanos<-c()
#   for(i in 1:N){
#     #buscar medoide mas cercano para cada gen
#     grupo<-which.min(alpha*Mb[i,No_evaluados[f,1:k]]+(1-alpha)*Me[i,No_evaluados[f,1:k]]) #0.5*Mb[i,No_evaluados[f,1:k]]+0.5*Me[i,No_evaluados[f,1:k]]
#     cercanos<-c(cercanos, grupo)
#   }
#   
#   #Evaluación Expresión
#   suma_dist<-matrix(0,nrow=k,ncol=k)
#   contador<-matrix(0,nrow=k,ncol=k)
#   
#   for(i in 1:N){
#     for(j in 1:k){
#       if(i!=No_evaluados[f,j]){
#         suma_dist[j,cercanos[i]]<-suma_dist[j,cercanos[i]]+Me[No_evaluados[f,j],i]
#         contador[j,cercanos[i]]<-contador[j,cercanos[i]]+1
#         
#       }
#     }
#   }
#   
#   dist_promedio<-suma_dist/contador
#   silueta<-c()
#   for(i in 1:k){
#     a<-0
#     b<-10
#     for(j in 1:k){
#       if(i==j){
#         a<-dist_promedio[i,j]
#       }else{
#         b<-min(b,dist_promedio[i,j])
#       }
#     }
#     si<-(b-a)/max(a,b)
#     if(is.nan(a)){
#       si<-0
#     }
#     silueta<-c(silueta,si)
#   }
#   No_evaluados[f,k+1]<-mean(silueta) #el valor de silueta será el promedio
#   
#   #Evaluación Biológica
#   suma_dist<-matrix(0,nrow=k,ncol=k)
#   contador<-matrix(0,nrow=k,ncol=k)
#   
#   for(i in 1:N){
#     for(j in 1:k){
#       if(i!=No_evaluados[f,j]){
#         suma_dist[j,cercanos[i]]<-suma_dist[j,cercanos[i]]+Mb[No_evaluados[f,j],i]
#         contador[j,cercanos[i]]<-contador[j,cercanos[i]]+1
#         
#       }
#     }
#   }
#   dist_promedio<-suma_dist/contador
#   silueta<-c()
#   for(i in 1:k){
#     a<-0
#     b<-10
#     for(j in 1:k){
#       if(i==j){
#         a<-dist_promedio[i,j]
#       }else{
#         b<-min(b,dist_promedio[i,j])
#       }
#     }
#     si<-(b-a)/max(a,b)
#     if(is.nan(a)){
#       si<-0
#     }
#     silueta<-c(silueta,si)
#   }
#   No_evaluados[f,k+2]<-mean(silueta)
#   return(c(No_evaluados[f,k+1],No_evaluados[f,k+2]))
# }

#No_evaluados datos a evaluar
#Me matriz de expresión
#Mb matriz biológica



# exported_func <- c("evaluarporfila")
# parallel::clusterExport(my.cluster, exported_func)



# evaluarB<-function(N,k,No_evaluados,Me,Mb,alpha){
#   #primero se evalua si las configuraciones de Medoides a evaluar son varias o solo uno.
#   #print("evaluando")
#   tipo<-length(dim(No_evaluados))
#   if(tipo==2){
#     filas<-length(No_evaluados[,1])
#     
#     
#     
#     # #loading example data
#     # #data("penguins")
#     # 
#     # parallel::detectCores()
#     # 
#     # n.cores <- parallel::detectCores() - 1
#     # 
#     # #create the cluster
#     # my.cluster <- parallel::makeCluster(
#     #   n.cores, 
#     #   type = "PSOCK"
#     # )
#     # 
#     # #check cluster definition (optional)
#     # print(my.cluster)
#     # 
#     # 
#     # #register it to be used by %dopar%
#     # doParallel::registerDoParallel(cl = my.cluster)
#     # 
#     # #check if it is registered (optional)
#     # foreach::getDoParRegistered()
#     # 
#     # foreach::getDoParWorkers()
#     
#     ###################################################
#     evaluarporfila<<-function(N,k,f,No_evaluados,Me,Mb,alpha){
#       cercanos<-c()
#       for(i in 1:N){
#         #buscar medoide mas cercano para cada gen
#         grupo<-which.min(alpha*Mb[i,No_evaluados[f,1:k]]+(1-alpha)*Me[i,No_evaluados[f,1:k]]) #0.5*Mb[i,No_evaluados[f,1:k]]+0.5*Me[i,No_evaluados[f,1:k]]
#         cercanos<-c(cercanos, grupo)
#       }
#       
#       #Evaluación Expresión
#       suma_dist<-matrix(0,nrow=k,ncol=k)
#       contador<-matrix(0,nrow=k,ncol=k)
#       
#       for(i in 1:N){
#         for(j in 1:k){
#           if(i!=No_evaluados[f,j]){
#             suma_dist[j,cercanos[i]]<-suma_dist[j,cercanos[i]]+Me[No_evaluados[f,j],i]
#             contador[j,cercanos[i]]<-contador[j,cercanos[i]]+1
#             
#           }
#         }
#       }
#       
#       dist_promedio<-suma_dist/contador
#       silueta<-c()
#       for(i in 1:k){
#         a<-0
#         b<-10
#         for(j in 1:k){
#           if(i==j){
#             a<-dist_promedio[i,j]
#           }else{
#             b<-min(b,dist_promedio[i,j])
#           }
#         }
#         si<-(b-a)/max(a,b)
#         if(is.nan(a)){
#           si<-0
#         }
#         silueta<-c(silueta,si)
#       }
#       No_evaluados[f,k+1]<-mean(silueta) #el valor de silueta será el promedio
#       
#       #Evaluación Biológica
#       suma_dist<-matrix(0,nrow=k,ncol=k)
#       contador<-matrix(0,nrow=k,ncol=k)
#       
#       for(i in 1:N){
#         for(j in 1:k){
#           if(i!=No_evaluados[f,j]){
#             suma_dist[j,cercanos[i]]<-suma_dist[j,cercanos[i]]+Mb[No_evaluados[f,j],i]
#             contador[j,cercanos[i]]<-contador[j,cercanos[i]]+1
#             
#           }
#         }
#       }
#       dist_promedio<-suma_dist/contador
#       silueta<-c()
#       for(i in 1:k){
#         a<-0
#         b<-10
#         for(j in 1:k){
#           if(i==j){
#             a<-dist_promedio[i,j]
#           }else{
#             b<-min(b,dist_promedio[i,j])
#           }
#         }
#         si<-(b-a)/max(a,b)
#         if(is.nan(a)){
#           si<-0
#         }
#         silueta<-c(silueta,si)
#       }
#       No_evaluados[f,k+2]<-mean(silueta)
#       return(c(No_evaluados[f,k+1],No_evaluados[f,k+2]))
#     }
#     
#     
#     evaluacionbioyexp<-foreach(f=1:filas,.combine='rbind') %dopar% {evaluarporfila(N,k,f,No_evaluados,Me,Mb,alpha)}
#     ##############################################################################
#     
#     No_evaluados[,c(k+1,k+2)]<-evaluacionbioyexp
#     Eva<<-Eva+filas
#     #???print(paste("Número de evaluaciones actuales: ",Eva))
#     return(No_evaluados)
#   }else{
#     cercanos<-c()
#     for(i in 1:N){
#       #buscar medoide mas cercano para cada gen
#       grupo<-which.min(alpha*Mb[i,No_evaluados[1:k]]+(1-alpha)*Me[i,No_evaluados[1:k]]) #0.5*Mb[i,No_evaluados[1:k]]+0.5*Me[i,No_evaluados[1:k]]
#       cercanos<-c(cercanos, grupo)
#     }
#     
#     #Evaluación Expresión
#     suma_dist<-matrix(0,nrow=k,ncol=k)
#     contador<-matrix(0,nrow=k,ncol=k)
#     
#     for(i in 1:N){
#       for(j in 1:k){
#         if(i!=No_evaluados[j]){
#           suma_dist[j,cercanos[i]]<-suma_dist[j,cercanos[i]]+Me[No_evaluados[j],i]
#           contador[j,cercanos[i]]<-contador[j,cercanos[i]]+1
#           
#         }
#       }
#     }
#     
#     dist_promedio<-suma_dist/contador
#     silueta<-c()
#     for(i in 1:k){
#       a<-0
#       b<-10
#       for(j in 1:k){
#         if(i==j){
#           a<-dist_promedio[i,j]
#         }else{
#           b<-min(b,dist_promedio[i,j])
#         }
#       }
#       si<-(b-a)/max(a,b)
#       if(is.nan(a)){
#         si<-0
#       }
#       silueta<-c(silueta,si)
#     }
#     No_evaluados[k+1]<-mean(silueta) #el valor de silueta será el promedio
#     
#     #Evaluación Biológica
#     suma_dist<-matrix(0,nrow=k,ncol=k)
#     contador<-matrix(0,nrow=k,ncol=k)
#     
#     for(i in 1:N){
#       for(j in 1:k){
#         if(i!=No_evaluados[j]){
#           suma_dist[j,cercanos[i]]<-suma_dist[j,cercanos[i]]+Mb[No_evaluados[j],i]
#           contador[j,cercanos[i]]<-contador[j,cercanos[i]]+1
#           
#         }
#       }
#     }
#     dist_promedio<-suma_dist/contador
#     silueta<-c()
#     for(i in 1:k){
#       a<-0
#       b<-10
#       for(j in 1:k){
#         if(i==j){
#           a<-dist_promedio[i,j]
#         }else{
#           b<-min(b,dist_promedio[i,j])
#         }
#       }
#       si<-(b-a)/max(a,b)
#       if(is.nan(a)){
#         si<-0
#       }
#       silueta<-c(silueta,si)
#     }
#     No_evaluados[k+2]<-mean(silueta) #el valor de silueta será el promedio
#     Eva<<-Eva+1
#     #print(paste("Número de evaluaciones actuales: ",Eva))
#     return(No_evaluados)
#   }
# }

evaluar<-function(N,k,No_evaluados,Me,Mb,alpha){
  #primero se evalua si las configuraciones de Medoides a evaluar son varias o solo uno.
  #print("evaluando")
  tipo<-length(dim(No_evaluados))
  if(tipo==2){
    filas<-length(No_evaluados[,1])
    for(f in 1:filas){
      cercanos<-c()
      for(i in 1:N){
        #buscar medoide mas cercano para cada gen
        grupo<-which.min(alpha*Mb[i,No_evaluados[f,1:k]]+(1-alpha)*Me[i,No_evaluados[f,1:k]]) #0.5*Mb[i,No_evaluados[f,1:k]]+0.5*Me[i,No_evaluados[f,1:k]]
        cercanos<-c(cercanos, grupo)
      }
      
      #Evaluación Expresión
      suma_dist<-matrix(0,nrow=k,ncol=k)
      contador<-matrix(0,nrow=k,ncol=k)
      
      for(i in 1:N){
        for(j in 1:k){
          if(i!=No_evaluados[f,j]){
            suma_dist[j,cercanos[i]]<-suma_dist[j,cercanos[i]]+Me[No_evaluados[f,j],i]
            contador[j,cercanos[i]]<-contador[j,cercanos[i]]+1
            
          }
        }
      }
      
      dist_promedio<-suma_dist/contador
      silueta<-c()
      for(i in 1:k){
        a<-0
        b<-10
        for(j in 1:k){
          if(i==j){
            a<-dist_promedio[i,j]
          }else{
            b<-min(b,dist_promedio[i,j])
          }
        }
        si<-(b-a)/max(a,b)
        if(is.nan(a)){
          si<-0
        }
        silueta<-c(silueta,si)
      }
      No_evaluados[f,k+1]<-mean(silueta) #el valor de silueta será el promedio
      
      #Evaluación Biológica
      suma_dist<-matrix(0,nrow=k,ncol=k)
      contador<-matrix(0,nrow=k,ncol=k)
      
      for(i in 1:N){
        for(j in 1:k){
          if(i!=No_evaluados[f,j]){
            suma_dist[j,cercanos[i]]<-suma_dist[j,cercanos[i]]+Mb[No_evaluados[f,j],i]
            contador[j,cercanos[i]]<-contador[j,cercanos[i]]+1
            
          }
        }
      }
      dist_promedio<-suma_dist/contador
      silueta<-c()
      for(i in 1:k){
        a<-0
        b<-10
        for(j in 1:k){
          if(i==j){
            a<-dist_promedio[i,j]
          }else{
            b<-min(b,dist_promedio[i,j])
          }
        }
        si<-(b-a)/max(a,b)
        if(is.nan(a)){
          si<-0
        }
        silueta<-c(silueta,si)
      }
      No_evaluados[f,k+2]<-mean(silueta) #el valor de silueta será el promedio
    }
    Eva<<-Eva+filas
    #print(paste("Número de evaluaciones actuales: ",Eva))
    return(No_evaluados)
  }else{
    cercanos<-c()
    for(i in 1:N){
      #buscar medoide mas cercano para cada gen
      grupo<-which.min(alpha*Mb[i,No_evaluados[1:k]]+(1-alpha)*Me[i,No_evaluados[1:k]]) #0.5*Mb[i,No_evaluados[1:k]]+0.5*Me[i,No_evaluados[1:k]]
      cercanos<-c(cercanos, grupo)
    }
    
    #Evaluación Expresión
    suma_dist<-matrix(0,nrow=k,ncol=k)
    contador<-matrix(0,nrow=k,ncol=k)
    
    for(i in 1:N){
      for(j in 1:k){
        if(i!=No_evaluados[j]){
          suma_dist[j,cercanos[i]]<-suma_dist[j,cercanos[i]]+Me[No_evaluados[j],i]
          contador[j,cercanos[i]]<-contador[j,cercanos[i]]+1
          
        }
      }
    }
    
    dist_promedio<-suma_dist/contador
    silueta<-c()
    for(i in 1:k){
      a<-0
      b<-10
      for(j in 1:k){
        if(i==j){
          a<-dist_promedio[i,j]
        }else{
          b<-min(b,dist_promedio[i,j])
        }
      }
      si<-(b-a)/max(a,b)
      if(is.nan(a)){
        si<-0
      }
      silueta<-c(silueta,si)
    }
    No_evaluados[k+1]<-mean(silueta) #el valor de silueta será el promedio
    
    #Evaluación Biológica
    suma_dist<-matrix(0,nrow=k,ncol=k)
    contador<-matrix(0,nrow=k,ncol=k)
    
    for(i in 1:N){
      for(j in 1:k){
        if(i!=No_evaluados[j]){
          suma_dist[j,cercanos[i]]<-suma_dist[j,cercanos[i]]+Mb[No_evaluados[j],i]
          contador[j,cercanos[i]]<-contador[j,cercanos[i]]+1
          
        }
      }
    }
    dist_promedio<-suma_dist/contador
    silueta<-c()
    for(i in 1:k){
      a<-0
      b<-10
      for(j in 1:k){
        if(i==j){
          a<-dist_promedio[i,j]
        }else{
          b<-min(b,dist_promedio[i,j])
        }
      }
      si<-(b-a)/max(a,b)
      if(is.nan(a)){
        si<-0
      }
      silueta<-c(silueta,si)
    }
    No_evaluados[k+2]<-mean(silueta) #el valor de silueta será el promedio
    Eva<<-Eva+1
    #print(paste("Número de evaluaciones actuales: ",Eva))
    return(No_evaluados)
  }
}



# evaluarA<-function(N,k,No_evaluados,Me,Mb,alpha){
#   filas<-length(No_evaluados[,1])
#   for(f in 1:filas){
#     cercanos<-c()
#     for(i in 1:N){
#       #buscar medoide mas cercano para cada gen
#       grupo<-which.min(alpha*Mb[i,No_evaluados[f,1:k]]+(1-alpha)*Me[i,No_evaluados[f,1:k]]) #0.5*Mb[i,No_evaluados[f,1:k]]+0.5*Me[i,No_evaluados[f,1:k]]
#       cercanos<-c(cercanos, grupo)
#     }
#     
#     #Evaluación Expresión
#     suma_dist<-matrix(0,nrow=k,ncol=k)
#     contador<-matrix(0,nrow=k,ncol=k)
#     
#     for(i in 1:N){
#       for(j in 1:k){
#         if(i!=No_evaluados[f,j]){
#           suma_dist[j,cercanos[i]]<-suma_dist[j,cercanos[i]]+Me[No_evaluados[f,j],i]
#           contador[j,cercanos[i]]<-contador[j,cercanos[i]]+1
# 
#         }
#       }
#     }
# 
#     dist_promedio<-suma_dist/contador
#     silueta<-c()
#     for(i in 1:k){
#       a<-0
#       b<-10
#       for(j in 1:k){
#         if(i==j){
#           a<-dist_promedio[i,j]
#         }else{
#           b<-min(b,dist_promedio[i,j])
#         }
#       }
#       si<-(b-a)/max(a,b)
#       if(is.nan(a)){
#         si<-0
#       }
#       silueta<-c(silueta,si)
#     }
#     No_evaluados[f,k+1]<-mean(silueta) #el valor de silueta será el promedio
#     
#     #Evaluación Biológica
#     suma_dist<-matrix(0,nrow=k,ncol=k)
#     contador<-matrix(0,nrow=k,ncol=k)
#     
#     for(i in 1:N){
#       for(j in 1:k){
#         if(i!=No_evaluados[f,j]){
#           suma_dist[j,cercanos[i]]<-suma_dist[j,cercanos[i]]+Mb[No_evaluados[f,j],i]
#           contador[j,cercanos[i]]<-contador[j,cercanos[i]]+1
#           
#         }
#       }
#     }
#     dist_promedio<-suma_dist/contador
#     silueta<-c()
#     for(i in 1:k){
#       a<-0
#       b<-10
#       for(j in 1:k){
#         if(i==j){
#           a<-dist_promedio[i,j]
#         }else{
#           b<-min(b,dist_promedio[i,j])
#         }
#       }
#       si<-(b-a)/max(a,b)
#       if(is.nan(a)){
#         si<-0
#       }
#       silueta<-c(silueta,si)
#     }
#     No_evaluados[f,k+2]<-mean(silueta) #el valor de silueta será el promedio
#   }
#   Eva<<-Eva+filas
#   return(No_evaluados)
# }



#Agentes que se ordenarán por No dominados
ordenarNoDominados<-function(N,k,Medoids){
  filas<-length(Medoids[,1])
  nivel<-1
  listos<-c(1:filas)
  while(sum(listos)>=1){
    analizar<-c()
    for(i in 1:filas){
      if(listos[i]!=0){
        analizar<-c(analizar,i)
      }
    }
    for(i in analizar){
      flag<-T
      for(j in analizar){
        if(i!=j){
          if(Medoids[i,k+1]<Medoids[j,k+1]){
            if(Medoids[i,k+2]<Medoids[j,k+2]){
              flag<-F
            }
          }
        }
      }
      if(flag==T){
        Medoids[i,k+3]<-nivel
        listos[i]<-0
      }
    }
    nivel<-nivel+1
  }
  Medoids<-Medoids[order(Medoids[,k+3]),]
  return(Medoids)
}






#A partir de 2 soluciones padre se crean 2 soluciones hijo en una matriz de 2 filas.
#fila 1 hijo 1
#fila 2 hijo 2

cruzamiento<-function(N,k,sol1,sol2){
  desordenar<-sample(1:k,k)
  sol1<-sol1[desordenar]
  hijos<-matrix(NA,nrow=2,ncol=k)
  flag<-0
  while(flag==0){
    valores<-sample(1:2, k, replace=T)
    soniguales=sum(valores)
    if(soniguales!=k & soniguales!=2*k){
      flag<-1
    }
  }
  for(i in 1:k){
    if(valores[i]==1){
      hijos[1,i]<-sol1[i]
      hijos[2,i]<-sol2[i]
    }else{
      hijos[1,i]<-sol2[i]
      hijos[2,i]<-sol1[i]
    }
  }
  
  #manejo de restricción (no se puede repetir los medoides)
  sol<-c(sol1,sol2)
  for(h in 1:2){
    for(i in 2:k){
      for(j in (i-1):1){
        if(hijos[h,i]==hijos[h,j]){
          continuar<-T
          comparado<-1
          while(continuar==T & comparado<=(2*k)){
            continuar<-F
            for(m in 1:(i-1)){
              if(hijos[h,m]==sol[comparado]){
                continuar<-T
                comparado<-comparado+1
              }
            }
          }
          if(continuar==F){
            hijos[h,i]<-sol[comparado]
          }else{
            hijos[h,i]<-sample(c(1:N)[-sol],1)
          }
        }
      }
    }
  }
  return(hijos)
}

#realiza una búsqueda local
busquedalocal<-function(N,k,sol,Me,Mb,alpha){
  cambios<-sample(0:1, k, replace=T)
  if(sum(cambios)==0){
    obligado<-sample(1:k,1)
    cambios[obligado]<-cambios[obligado]+1
  }
  for(i in 1:k){
    if(cambios[i]>=1){
      Me[sol[i],sol[i]]=10
      sol[i]<-which.min(c((1-alpha)*Me[sol[i],]+alpha*Mb[sol[i],]))
    }
  }
  #manejo de restricciones, si uno se repite cambia un medoide por uno al azar
  listos<-c(sol[1])
  for(i in 2:k){
    for(j in 1:(i-1)){
      if(sol[i]==sol[j]){
        sol[i]<-sample(c(1:N)[-listos],1)
      }
    }
    listos<-c(listos,sol[i])
  }
  return(sol)
}

#realiza mutaciones
mutacion<-function(N,k,sol){
  cambios<-sample(0:1, k, replace=T)
  if(sum(cambios)==0){
    obligado<-sample(1:k,1)
    cambios[obligado]<-cambios[obligado]+1
  }
  for(i in 1:k){
    if(cambios[i]>=1){
      sol[i]<-sample(c(1:N)[-sol],1)
    }
  }
  #manejo de restricciones, si uno se repite cambia un medoide por uno al azar
  listos<-c(sol[1])
  for(i in 2:k){
    for(j in 1:(i-1)){
      if(sol[i]==sol[j]){
        sol[i]<-sample(c(1:N)[-listos],1)
      }
    }
    listos<-c(listos,sol[i])
  }
  return(sol)
} 

duplicarpoblacion<-function(N,k,Medoids,Cruz,BusL,Me,Mb,alpha){
  largo<-length(Medoids[,1])
  nuevos<-matrix(NA,nrow=largo,ncol=k+3)
  c<-floor(largo*Cruz/2)*2
  b<-ceiling(largo*BusL)
  m<-largo-c-b
  contador<-0
  #cruzamiento
  if(c>2){
    for(i in 0:(c/2-1)){
      rango<-1+i*(((largo/2)-1)/((c/2)-1))
      filaPadreA<-sample(c(1:rango),1)
      opciones<-c(1:(rango*2))[-filaPadreA]
      if(length(opciones)==1){
        filaPadreB<-opciones
      }else{
        filaPadreB<-sample(opciones,1)
      }
      nuevos[(contador+1):(contador+2),1:k]<-cruzamiento(N,k,Medoids[filaPadreA,1:k],Medoids[filaPadreB,1:k])
      contador<-contador+2
    }
  }
  if(c==2){
    nuevos[(contador+1):(contador+2),1:k]<-cruzamiento(N,k,Medoids[1,1:k],Medoids[2,1:k])
    contador<-contador+2
  }
  #busqueda local
  if(b>1){
    for(i in 0:(b-1)){
      rango<-1+i*(((largo/2)-1)/(b-1))
      filaSeleccionado<-sample(c(1:rango),1)
      contador<-contador+1
      nuevos[contador,1:k]<-busquedalocal(N,k,Medoids[filaSeleccionado,1:k],Me,Mb,alpha)
    }
  }
  if(b==1){
    nuevos[contador+1,1:k]<-busquedalocal(N,k,Medoids[1,1:k],Me,Mb,alpha)
    contador<-contador+1
  }
  #mutaciones
  if(m>1){
    for(i in 0:(m-1)){
      rango<-1+i*(((largo/2)-1)/(m-1))
      filaSeleccionado<-sample(c(1:rango),1)
      contador<-contador+1
      nuevos[contador,1:k]<-mutacion(N,k,Medoids[filaSeleccionado,1:k])
    }
  }
  if(m==1){
    nuevos[contador+1,1:k]<-mutacion(N,k,Medoids[1,1:k])
    contador<-contador+1
  }
  return(nuevos)
}

crowdingDistance<-function(N,k,Medoids,duplicadosEliminar){
  max_e<-max(Medoids[,k+1])
  min_e<-min(Medoids[,k+1])
  max_b<-max(Medoids[,k+2])
  min_b<-min(Medoids[,k+2])
  delta_e<-max_e-min_e
  delta_b<-max_b-min_b
  filas<-length(Medoids[,1])
  maxNivel<-Medoids[filas,k+3]
  contador<-matrix(0,nrow=maxNivel,ncol=3)
  contador[1,1]<-1
  contador[maxNivel,2]<-filas
  nivel<-1
  for(i in 1:filas){
    contador[Medoids[i,k+3],3]<-contador[Medoids[i,k+3],3]+1
    if(Medoids[i,k+3]!=nivel){
      contador[nivel,2]<-i-1
      nivel<-nivel+1
      contador[nivel,1]<-i
    }
  }
  ordenados<-matrix(NA,nrow=filas,ncol=k+3)
  for(i in 1:maxNivel){
    if(contador[i,3]!=1){
      grupo<-Medoids[contador[i,1]:contador[i,2],]
      ordenados[contador[i,1]:contador[i,2],]<-grupo[order(grupo[,k+1]),]
    }else{
      ordenados[contador[i,1]:contador[i,2],]<-Medoids[contador[i,1]:contador[i,2],]
    }
  }
  crowding<-replicate(filas,0)
  ordenados<-cbind(ordenados,crowding)
  #https://www.youtube.com/watch?v=Hm2LK4vJzRw&ab_channel=OptimizationGeeksOptimizationGeeks
  for(i in 1:maxNivel){
    if(contador[i,3]<=2){
      ordenados[contador[i,1]:contador[i,2],k+4]<-Inf
    }else{
      ordenados[contador[i,1],k+4]<-Inf
      ordenados[contador[i,2],k+4]<-Inf
      for(j in (contador[i,1]+1):(contador[i,2]-1)){
        ordenados[j,k+4]<-(ordenados[j+1,k+1]-ordenados[j-1,k+1])/delta_e+(ordenados[j-1,k+2]-ordenados[j+1,k+2])/delta_b
      }
      grupo<-ordenados[contador[i,1]:contador[i,2],]
      ordenados[contador[i,1]:contador[i,2],]<-grupo[order(-grupo[,k+4]),]
    }
  }
  
  
  
  
  if(duplicadosEliminar==1){
    nEliminados<-0
    fEliminadas<-c()
    for(i in 1:filas){
      if(ordenados[i,k+4]==0){
        nEliminados<-nEliminados+1
        fEliminadas<-c(fEliminadas,i)
      }
    }
  }
  
  
  
  
  ordenados<-ordenados[,-(k+4)]
  
  

  if(duplicadosEliminar==1){
    if(nEliminados>=1){
      ordenados<-ordenados[-fEliminadas,]
      if(nEliminados>(filas/2)){
        nuevosp<-crearpoblacioninicial(N,k,nEliminados-(filas/2))
        nuevosp<-evaluar(N,k,nuevosp,Me,Mb)
        ordenados<-rbind(ordenados,nuevosp)
      }
    }
  }
  
  
  
  return(ordenados)
}


sonIguales<-function(k,sol1,sol2){
  c<-0
  for(i in 1:k){
    for(j in 1:k){
      if(sol1[i]==sol2[j]){
        c<-c+1
      }
    }
  }
  if(c==k){
    return(T)
  }else{
    return(F)
  }
}


eliminarRepetidos<-function(n,k,Medoids){
  filas=length(Medoids[,1])
  indicesOrd<-order(Medoids[,k+1])
  Eliminar<-c()
  for(i in 1:(filas-1)){
    if(Medoids[indicesOrd[i],k+1]==Medoids[indicesOrd[i+1],k+1]){
      if(Medoids[indicesOrd[i],k+2]==Medoids[indicesOrd[i+1],k+2]){
        if(sonIguales(k,Medoids[indicesOrd[i],1:k],Medoids[indicesOrd[i+1],1:k])){
          Eliminar<-c(Eliminar,indicesOrd[i+1])
        }
      }
    }
  }
  if(length(Eliminar)>0){
    Medoids<-Medoids[-(Eliminar),]
  }
  return(Medoids)
}



crearpoblacioninicial3d<-function(N,k,P_1,P_2,Medoids,Sub,alpha){
  c<-1
  filas<-length(Medoids[,1])
  while(Medoids[c,k+3]==1 & c<filas){
    c<-c+1
  }
  if(c<filas){
    c<-c-1
  }
  nSubpoblacion<<-c
  soluciones<-replicate((P_2*(k+3)*c),NA)
  dim(soluciones)<-c(P_2,k+3,c)
  soluciones[1,,1:c]<-t(Medoids[1:c,])
  for(m in 1:c){
    for(i in 2:P_2){
      decision<-sample(0:1,1,replace=F)
      if(decision==1){
        soluciones[i,1:k,m]<-mutacion(N,k,Medoids[m,1:k])
      }else{
        soluciones[i,1:k,m]<-busquedalocal(N,k,Medoids[m,1:k],Me,Mb,alpha)
      }
    }
  }
  
  ##Eliminar Repetidos y reemplazar por soluciones aleatorias
  for(m in 1:c){
    for(i in 1:(P_2-1)){
      for(j in (i+1):P_2){
        if(sonIguales(k,soluciones[i,1:k,m],soluciones[j,1:k,m])){
          soluciones[j,1:k,m]<-sample(1:N,k,replace=FALSE)
        }
      }
    }
  }
  return(soluciones)
}











evaluar3d<-function(N,k,soluciones_2,Me,Mb,alpha){
  profundidad<-length(soluciones_2[1,1,])
  filas<-length(soluciones_2[,1,1])
  for(i in 1:profundidad){
    soluciones_2[2:filas,,i]<-evaluar(N,k,soluciones_2[2:filas,,i],Me,Mb,alpha)
  }
  return(soluciones_2)
}

ordenarNodominados3d<-function(N,k,soluciones_2){
  profundidad<-length(soluciones_2[1,1,])
  filas<-length(soluciones_2[,1,1])
  for(i in 1:profundidad){
    soluciones_2[,,i]<-ordenarNoDominados(N,k,soluciones_2[,,i])
  }
  return(soluciones_2)
}

subirUnNivel<-function(N,k,Medoids,soluciones_2){
  opcion<-1
  if(opcion==1){ #opcion 1 se comparan todas las soluciones de la capa 1 y 2 quedarán las mejores
    if(length(dim(soluciones_2))==3){#verificar que es una matriz de 3 dimensiones
      profundidad3D<-length(soluciones_2[1,1,])
      filas<-length(Medoids[,1])
      Analizar<-c()
      for(i in 1:profundidad3D){
        Analizar<-rbind(Analizar,soluciones_2[,,i])
      }
      if(profundidad3D<filas){
        Analizar<-rbind(Analizar,Medoids[((profundidad3D+1):filas),])
      }
      Analizar<-eliminarRepetidos(N,k,Analizar)
      Analizar<-ordenarNoDominados(N,k,Analizar)
      Analizar<-crowdingDistance(N,k,Analizar,0)
      filasAnalizar<-length(Analizar[,1])
      Analizar<-Analizar[-c((filas+1):filasAnalizar),]
    }
  }
  return(Analizar)
}


graficarSolucion<-function(Solucion){
  if(length(dim(Solucion))==3){
    maximox<-max(Solucion[,k+1,])
    minimox<-min(Solucion[,k+1,])
    maximoy<-max(Solucion[,k+2,])
    minimoy<-min(Solucion[,k+2,])
    profundidad<-length(Solucion[1,1,])
    for(i in 1:profundidad){
      plot(Solucion[,k+1,i],Solucion[,k+2,i],col=ifelse(Solucion[,k+3,i]==1,2,1),lwd=3,xlim=c(minimox,maximox),ylim=c(minimoy,maximoy),xlab="Similitud en expresión génica",ylab="Similitud en procesos biológicos",pch=16)
      title(c("Finalizando la generación ",i))
    }
  }
  if(length(dim(Solucion))==2){
    maximox<-max(Solucion[,k+1])
    minimox<-min(Solucion[,k+1])
    maximoy<-max(Solucion[,k+2])
    minimoy<-min(Solucion[,k+2])
    plot(Solucion[,k+1],Solucion[,k+2],col=ifelse(Solucion[,k+3]==1,2,1),lwd=3,xlim=c(minimox,maximox),ylim=c(minimoy,maximoy),xlab="Similitud en expresión génica",ylab="Similitud en procesos biológicos",pch=16)
    title(c("Gráfico Solicitado ","***"))
  }
}

hiperVolumen<-function(N,k,Medoids){
  largohv<-length(Medoids[,1])
  todosnivel1<-T
  for(i in 1:largohv){
    if(todosnivel1==T){
      if(Medoids[i,k+3]==2){
        todosnivel1<-F
        comienzanivel2<-i
      }
    }
  }
  if(todosnivel1==F){
    pareto<-Medoids[-(comienzanivel2:largohv),]
  }else{
    pareto<-Medoids
  }
  if(length(dim(pareto))==2){
    pareto<-pareto[order(pareto[,k+1]),]
    nPareto<-length(pareto[,1]) #número de datos de pareto
    xanterior<-(-1)
    yanterior<-(-1)
    suma<-0 #Suma de los rectangulos que forman el hipervolumen
    for(i in 1:nPareto){
      suma<-suma+((pareto[i,k+1]-xanterior)*(pareto[i,k+2]-yanterior))
      xanterior<-pareto[i,k+1]
    }
    return(suma) #suma es el hipervolumen
  }else{
    return((pareto[k+1]+1)*(pareto[k+2]+1))
  }
}

numeroAgentePareto<-function(N,k,P_1,Medoids){
  validador<-F
  for(i in 1:P_1){
    if(validador==F){
      if(Medoids[i,k+3]==2){
        validador<-T
        nivel1<-i-1
      }
    }
  }
  if(validador==F){
    nivel1<-P_1
  }
  return(nivel1)
}

crearpoblacioninicialEvaluadaDesdeSolucion<-function(N,k,P_2,Solucionpadreevaluada,alpha,Me,Mb){
  pob<-matrix(NA,nrow=(P_2-1),ncol=(k+3))
  for(i in 1:(P_2-1)){
    decision<-sample(0:1,1,replace=F)
    if(decision==1){
      pob[i,1:k]<-mutacion(N,k,Solucionpadreevaluada[1:k])
    }else{
      pob[i,1:k]<-busquedalocal(N,k,Solucionpadreevaluada[1:k],Me,Mb,alpha)
    }
  }
  ##Eliminar Repetidos y reemplazar por soluciones aleatorias
  for(i in 1:(P_2-2)){
    for(j in (i+1):P_2-1){
      if(sonIguales(k,pob[i,1:k],pob[j,1:k])){
        pob[j,1:k]<-sample(1:N,k,replace=FALSE)
      }
    }
  }
  #evaluar nuevos, juntar con solución padre y ordenar por no dominados
  pob<-evaluar(N,k,pob,Me,Mb,alpha)
  pob<-rbind(Solucionpadreevaluada,pob)
  pob<-ordenarNoDominados(N,k,pob)
  return(pob)
}

#     nuevapoblacion<-duplicarpoblacion(N,k,soluciones_2[,,a],Cruz,BusL,Me,Mb,alpha)
#     nuevapoblacion<-evaluar(N,k,nuevapoblacion,Me,Mb,alpha)
#     poblacionduplicada<-rbind(soluciones_2[,,a],nuevapoblacion)
#     poblacionduplicada<-ordenarNoDominados(N,k,poblacionduplicada)
#     poblacionduplicada<-crowdingDistance(N,k,poblacionduplicada,0)
#     poblacionduplicada<-eliminarRepetidos(N,k,poblacionduplicada)
#     soluciones_2[,,a]<-poblacionduplicada[-c((P_2+1):(2*P_2)),]

#calculo que equivale a una generación del algoritmo NSGAII
NSGAIInivel2<-function(N,k,Medoidspadres,P_2,Cruz,BusL,Me,Mb,alpha){
  nuevapoblacion<-duplicarpoblacion(N,k,Medoidspadres,Cruz,BusL,Me,Mb,alpha)
  nuevapoblacion<-evaluar(N,k,nuevapoblacion,Me,Mb,alpha)
  poblacionduplicada<-rbind(Medoidspadres,nuevapoblacion)
  poblacionduplicada<-ordenarNoDominados(N,k,poblacionduplicada)
  poblacionduplicada<-crowdingDistance(N,k,poblacionduplicada,0)
  poblacionduplicada<-eliminarRepetidos(N,k,poblacionduplicada)
  poblacionSalida<-poblacionduplicada[-c((P_2+1):(2*P_2)),]
  hv1<-hiperVolumen(N,k,Medoidspadres)
  hv2<-hiperVolumen(N,k,poblacionSalida)
  if(hv2>hv1){
    mejorosiono<<-T
  }
  return(poblacionSalida)
}

busquedalocalconSalto<-function(N,k,sol,Me,Mb,alpha){
  flag<-0
  solVecino<-sol
  while(flag==0){
    cambia<-sample(0:1,k,replace=T)
    if(sum(cambia)!=0){
      flag<-1
    }
  }
  for(i in 1:k){
    if(cambia[i]==1){
      prob=runif(1,min=0,max=1)
      
      if(prob<=0.5){
        dismin<-10
        gen<-0
        for(f in 1:N){
          if(f!=sol[i]){
            distancia<-0.5*Me[sol[i],f]+0.5*Mb[sol[i],f]
            if(distancia<dismin){
              dismin<-distancia
              gen<-f
            }
          }
        }
        solVecino[i]<-gen
      }
      
      if(prob<=0.75 & prob>0.5){
        dismin<-c(10,11)
        gen<-c(0,0)
        for(f in 1:N){
          if(f!=sol[i]){
            distancia<-0.5*Me[sol[i],f]+0.5*Mb[sol[i],f]
            if(distancia<=dismin[2]){
              dismin[2]<-distancia
              gen[2]<-f
              gen<-gen[order(dismin)]
              dismin<-dismin[order(dismin)]
            }
          }
        }
        solVecino[i]<-gen[2]
      }
      
      if(prob<=0.88 & prob>0.75){
        dismin<-c(10,11,12)
        gen<-c(0,0,0)
        for(f in 1:N){
          if(f!=sol[i]){
            distancia<-0.5*Me[sol[i],f]+0.5*Mb[sol[i],f]
            if(distancia<=dismin[3]){
              dismin[3]<-distancia
              gen[3]<-f
              gen<-gen[order(dismin)]
              dismin<-dismin[order(dismin)]
            }
          }
        }
        solVecino[i]<-gen[3]
      }
      
      if(prob<=1 & prob>0.88){
        dismin<-c(10,11,12,13)
        gen<-c(0,0,0,0)
        for(f in 1:N){
          if(f!=sol[i]){
            distancia<-0.5*Me[sol[i],f]+0.5*Mb[sol[i],f]
            if(distancia<=dismin[4]){
              dismin[4]<-distancia
              gen[4]<-f
              gen<-gen[order(dismin)]
              dismin<-dismin[order(dismin)]
            }
          }
        }
        solVecino[i]<-gen[4]
      }
    }
  }
  #analizar si se repiten medoides y reemplazar por unos al azar si ocurre
  for(i in 2:k){
    for(j in 1:(i-1)){
      if(solVecino[i]==solVecino[j]){
        posibles<-(1:N)[-c(solVecino[1:(i-1)])]
        solVecino[i]=sample(posibles,1,replace=T)
      }
    }
  }
  
  #analizar si la solucion encontrada es igual a la original
  flag<-0
  while(flag==0){
    if(sonIguales(k,sol,solVecino)){
      cambiar<-sample(1:k,1,replace=T)
      solVecino[cambiar]<-sample((1:N)[-sol],1,replace=T)
    }else{
      flag<-1
    }
  }
  return(solVecino)
}

busquedaLMOLS<-function(N,k,partida,Me,Mb,P_2,alpha){
  exploracion<-matrix(NA,nrow=(P_2*2),ncol=(k+3))
  exploracion[1,]<-partida
  seleccion<-partida
  for(i in 2:(P_2*2)){
    nuevo<-busquedalocalconSalto(N,k,seleccion,Me,Mb,alpha)
    nuevo<-evaluar(N,k,nuevo,Me,Mb,alpha)
    exploracion[i,]<-nuevo
    if(nuevo[k+1]>seleccion[k+1] | nuevo[k+2]>seleccion[k+2]){
      seleccion<-nuevo
    }
  }
  return(exploracion)
}

busquedaNMOLS<-function(N,k,partida,Me,Mb,P_2,alpha){
  exploracion<-matrix(NA,nrow=(P_2*2),ncol=(k+3))
  exploracion[1,]<-partida
  seleccion<-partida
  for(i in 2:(P_2*2)){
    nuevo<-busquedalocalconSalto(N,k,seleccion,Me,Mb,alpha)
    nuevo<-evaluar(N,k,nuevo,Me,Mb,alpha)
    exploracion[i,]<-nuevo
    if(nuevo[k+1]>seleccion[k+1] & nuevo[k+2]>seleccion[k+2]){
      seleccion<-nuevo
    }
  }
  return(exploracion)
}

corregirTamano<-function(N,k,Medoids,P_2){
  largo<-length(Medoids[,1])
  if(largo<P_2){
    nuevos<-crearpoblacioninicial(N,k,(P_2-largo))
    nuevos<-evaluar(N,k,nuevos,Me,Mb,alpha)
    final<-rbind(Medoids,nuevos)
    final<-ordenarNoDominados(N,k,final)
    final<-crowdingDistance(N,k,final,0)
    return(final)
  }else{
    return(Medoids)
  }
}


compartirInformacion2<-function(N,k,soluciones_2,Com){
  profundidad<-length(soluciones_2[1,1,])
  filas<-length(soluciones_2[,1,1])
  mejoresFilas<-c()
  #se buscan las soluciones que aportan mejor hipervolumen
  for(p in 1:profundidad){
    mejorhV<-0
    filaMejor<-0
    for(f in 1:filas){
      hV<-(soluciones_2[f,k+1,p]+1)*(soluciones_2[f,k+2,p]+1)
      if(mejorhV<hV){
        mejorhV<-hV
        filaMejor<-f
      }
    }
    mejoresFilas<-c(mejoresFilas,filaMejor)
  }
  #anillo
  if(Com==1){
    soluciones_2[filas,,1]<-soluciones_2[mejoresFilas[profundidad],,profundidad]
    for(i in 1:(profundidad-1)){
      soluciones_2[filas,,i+1]<-soluciones_2[mejoresFilas[i],,i]
    }
    return(soluciones_2)
  }
}

compartirInformacion<-function(N,k,memoria,Com){
  memoriasalida<-memoria
  Nagentes<-length(memoria[1,1,])
  Npob2<-length(memoria[,1,1])
  mejorescom<-matrix(NA,nrow=(Nagentes),ncol=(k+3))
  for(a in 1:Nagentes){
    cantidadnivel1<-numeroAgentePareto(N,k,Npob2,memoria[,,a])
    select<-sample(1:cantidadnivel1, 1, replace=T)
    mejorescom[a,]<-memoria[select,,a]
  }
  #anillo
  if(Com==1){
    for(a in 1:(Nagentes-1)){
      memoriasalida[Npob2,,a+1]<-mejorescom[a,]
    }
    memoriasalida[Npob2,,1]<-mejorescom[Nagentes,]
    for(a in 1:Nagentes){
      memoriasalida[,,a]<-ordenarNoDominados(N,k,memoriasalida[,,a])
      memoriasalida[,,a]<-crowdingDistance(N,k,memoriasalida[,,a],0)
    }
    return(memoriasalida)
  }
  #comparacion entre todos
  if(Com==2){
    mejorescom<-ordenarNoDominados(N,k,mejorescom)
    mejorescom<-crowdingDistance(N,k,mejorescom,0)
    cantidadnivel1<-numeroAgentePareto(N,k,Nagentes,mejorescom)
    azarsel<-sample(1:cantidadnivel1,1,replace = T)
    for(a in 1:Nagentes){
      memoriasalida[Npob2,,a]<-mejorescom[azarsel,]
      memoriasalida[,,a]<-ordenarNoDominados(N,k,memoriasalida[,,a])
      memoriasalida[,,a]<-crowdingDistance(N,k,memoriasalida[,,a],0)
    }
    return(memoriasalida)
  }
  #random
  if(com==3){
    selectrandom<-sample(1:Nagentes,1,replace=T)
    for(a in 1:Nagentes){
      memoriasalida[Npob2,,a]<-mejorescom[selectrandom,]
      memoriasalida[,,a]<-ordenarNoDominados(N,k,memoriasalida[,,a])
      memoriasalida[,,a]<-crowdingDistance(N,k,memoriasalida[,,a],0)
    }
    return(memoriasalida)
  }
}