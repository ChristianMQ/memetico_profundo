source("funciones_varias.R")

#N número de genes
#k número de grupos
#P_1 población de agentes capa 1
#G_1 generaciones capa 1
#P_2 población capa 2
#G_2 generaciones capa 2
#Cruz porcentaje de cruzamiento para duplicar agentes
#BusL porcentaje de Mutaciones
#Sub porcentaje de NSGA-II de la capa 2
#Com tipo de comunicación entre poblaciones de la capa 2


deepmemetic<-function(N,k,P_1,G_1,P_2,G_2,Cruz,BusL,Sub,Com,Me,Mb,alpha,EvaM,salida){
  #print(paste("P_1 ",P_1,";P_2 ",P_2,";G_2 ",G_2, ";Cruz ",Cruz,";Com ",Com))
  Eva<<-0
  if(salida==2){
    soluciones<-replicate((P_1*(k+3)*G_1),NA)
    dim(soluciones)<-c(P_1,k+3,G_1)
  }
  estrategiaDeBusqueda<-c(1:3)
  #print(estrategiaDeBusqueda)
  #inicia capa 1
  Medoids<-crearpoblacioninicial(N,k,P_1)
  Medoids<-evaluar(N,k,Medoids,Me,Mb,alpha)
  Medoids<-ordenarNoDominados(N,k,Medoids)
  Medoids<-crowdingDistance(N,k,Medoids,0)
  generacionnivel1<-0
  while(Eva<EvaM){
    generacionnivel1<-generacionnivel1+1
    nuevapoblacion<-duplicarpoblacion(N,k,Medoids,Cruz,BusL,Me,Mb,alpha)
    nuevapoblacion<-evaluar(N,k,nuevapoblacion,Me,Mb,alpha)
    Medoids<-rbind(Medoids,nuevapoblacion)
    Medoids<-ordenarNoDominados(N,k,Medoids)
    Medoids<-crowdingDistance(N,k,Medoids,0)
    Medoids<-eliminarRepetidos(N,k,Medoids)
    Medoids<-Medoids[-c((P_1+1):(2*P_1)),]
    #inicia la capa 2
    if(P_2>0 & Medoids[2,k+3]==1 & Eva<EvaM){
      HVnivel1<-hiperVolumen(N,k,Medoids)
      Nagentepareto<-numeroAgentePareto(N,k,P_1,Medoids)
      memoria<-replicate((P_2*(k+3)*Nagentepareto),NA)
      dim(memoria)<-c(P_2,k+3,Nagentepareto)
      estrategia<-sample(estrategiaDeBusqueda,Nagentepareto,replace=T) #se designa la estrategia de busqueda a cada agente de la capa 2
      for(j in 1:G_2){
        for(a in 1:Nagentepareto){ #paralelizable********
          
          if(estrategia[a]==1){ #NSGAII    #a es el número del agente del nivel 2
            if(j==1){
              memoria[,,a]<-crearpoblacioninicialEvaluadaDesdeSolucion(N,k,P_2,Medoids[a,],alpha,Me,Mb)
            }
            memoria[,,a]<-NSGAIInivel2(N,k,memoria[,,a],P_2,Cruz,BusL,Me,Mb,alpha)
            comparacionconpadre<-rbind(memoria[,,a],Medoids)
            comparacionconpadre<-ordenarNoDominados(N,k,comparacionconpadre)
            comparacionconpadre<-crowdingDistance(N,k,comparacionconpadre,0)
            comparacionconpadre<-eliminarRepetidos(N,k,comparacionconpadre)
            HVnivel2<-hiperVolumen(N,k,comparacionconpadre)
            if(HVnivel2>HVnivel1){
              estrategiaDeBusqueda<-c(estrategiaDeBusqueda,1)
            }
          }
          
          if(estrategia[a]==2){
            if(j==1){
              partida<-Medoids[a,]
            }else{
              Npareto1<-numeroAgentePareto(N,k,P_2,memoria[,,a])
              azar<-sample(1:Npareto1,1,replace=T)
              partida<-memoria[azar,,a]
            }
            exploracion<-busquedaLMOLS(N,k,partida,Me,Mb,P_2,alpha)
            exploracion<-eliminarRepetidos(N,k,exploracion)
            exploracion<-exploracion[-c((P_2+1):(2*P_2)),]
            filasexploracion<-length(exploracion[,1])
            if(filasexploracion<P_2){
              exploracion<-corregirTamano(N,k,exploracion,P_2)
            }else{
              exploracion<-ordenarNoDominados(N,k,exploracion)
              exploracion<-crowdingDistance(N,k,exploracion,0)
            }
            memoria[,,a]<-exploracion
            comparacionconpadre<-rbind(exploracion,Medoids)
            comparacionconpadre<-ordenarNoDominados(N,k,comparacionconpadre)
            comparacionconpadre<-crowdingDistance(N,k,comparacionconpadre,0)
            comparacionconpadre<-eliminarRepetidos(N,k,comparacionconpadre)
            HVnivel2<-hiperVolumen(N,k,comparacionconpadre)
            if(HVnivel2>HVnivel1){
              estrategiaDeBusqueda<-c(estrategiaDeBusqueda,2)
            }
          }
          
          if(estrategia[a]==3){
            if(j==1){
              partida<-Medoids[a,]
            }else{
              Npareto1<-numeroAgentePareto(N,k,P_2,memoria[,,a])
              azar<-sample(1:Npareto1,1,replace=T)
              partida<-memoria[azar,,a]
            }
            exploracion<-busquedaNMOLS(N,k,partida,Me,Mb,P_2,alpha)
            exploracion<-eliminarRepetidos(N,k,exploracion)
            exploracion<-exploracion[-c((P_2+1):(2*P_2)),]
            filasexploracion<-length(exploracion[,1])
            if(filasexploracion<P_2){
              exploracion<-corregirTamano(N,k,exploracion,P_2)
            }else{
              exploracion<-ordenarNoDominados(N,k,exploracion)
              exploracion<-crowdingDistance(N,k,exploracion,0)
            }
            memoria[,,a]<-exploracion
            comparacionconpadre<-rbind(exploracion,Medoids)
            comparacionconpadre<-ordenarNoDominados(N,k,comparacionconpadre)
            comparacionconpadre<-crowdingDistance(N,k,comparacionconpadre,0)
            comparacionconpadre<-eliminarRepetidos(N,k,comparacionconpadre)
            HVnivel2<-hiperVolumen(N,k,comparacionconpadre)
            if(HVnivel2>HVnivel1){
              estrategiaDeBusqueda<-c(estrategiaDeBusqueda,3)
            }
          }
          
          if(estrategia[a]==4){
            
          }
        }
        if(j!=G_2){
          #compartir información
          memoria<-compartirInformacion(N,k,memoria,Com)
        }
        #print(estrategiaDeBusqueda)
      }
      
      
      Medoids<-subirUnNivel(N,k,Medoids,memoria)
    }
    if(salida==2){
      #print("llegue acá")
      soluciones[,,generacionnivel1]<-Medoids
      #print("llegué acá")
    }
    #evaluacionvshipervolumen<<-rbind(evaluacionvshipervolumen,c(Eva,hiperVolumen(N,k,Medoids)))
    #print(paste("Finalizando generación ",generacionnivel1,";Número de evaluaciones actuales ",Eva))
  }
  if(salida==0){
    salidalista<-list(Medoids,hiperVolumen(N,k,Medoids),Eva,generacionnivel1)
    return(salidalista)
  }
  if(salida==1){
    hipervolumensalida<-hiperVolumen(N,k,Medoids)
    return(hipervolumensalida)
  }
  if(salida==2){
    return(soluciones)
  }
}

