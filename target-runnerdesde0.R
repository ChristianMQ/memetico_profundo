library("irace")

targetRunner<-function(experiment, scenario){
  source("memeticoprofundo.R")
  configuration<-experiment$configuration
  P_1<-as.numeric(configuration$P_1)
  P_2<-as.numeric(configuration$P_2)
  G_2<-as.numeric(configuration$G_2)
  Cruz<-as.numeric(configuration$Cruz)
  Comu<-as.numeric(configuration$Comu)
  
  Me<-(read.csv("matrizpearson_100_1_genes_f.csv"))[,-1]
  Mb<-(read.csv("matrizbiologica_100_1_genes_f.csv"))[,-1]
  # Me<-Me[,-1]
  # Mb<-Mb[,-1]
  N<-100       #n�mero de genes
  k<-4         #n�mero de grupos
  G_1<-5      #generaciones capa 1 depende de numero de evaluaciones (finalmente no se toma en cuenta)
  #P_1<-20     #poblaci�n de agentes capa 1 PPPPPPPPPPPPPPPPPPPP
  #G_2<-1       #generaciones capa 2 PPPPPPPPPPPPPPPPPPPP
  #P_2<-5       #poblaci�n capa 2 P PPPPPPPPPPPPPPPPPPPP
  Sub<-4       #porcentaje de NSGA-II de la capa 2 eliminado
  
  #Cruz<-0.8    #porcentaje de cruzamiento para duplicar agentes PPPPPPPPPPPPPPPPPPPP
  BusL<-0    #porcentaje de Busquedas locales (no tiene busqueda local NSGAII, finalmente no se toma en cuenta)
  
  #Comu<-1       #tipo de comunicaci�n entre poblaciones de la capa 2 PPPPPPPPPPPPPPPPPPPP
  
  Eva<-0       #contador de evaluaciones
  EvaM<<-2000   #evaluaciones m�ximas
  alpha<-0.5   #factor de Asociaci�n
  
  guardarSalidas<-1    #0 guarda datos en una lista de la �ltima generaci�n, 1 solo entrega el hipervolumen de la ultima generaci�n, 2 como estaba antes
  
  mejor<-deepmemetic(N,k,P_1,G_1,P_2,G_2,Cruz,BusL,Sub,Comu,Me,Mb,alpha,EvaM,guardarSalidas)
  return(list("cost"=-mejor))
}

main<-function(){
  archivoScenario<-readScenario("scenario.txt")
  archivoParameters<-readParameters("parameters.txt")
  irace(scenario=archivoScenario,parameters = archivoParameters)
  load("irace.Rdata")
  best.configs <- getFinalElites(iraceResults = iraceResults, n = min(length(iraceResults$allElites), 5))
  #iraceResults$allConfigurations esta linea o la anterior
  
}

main()
