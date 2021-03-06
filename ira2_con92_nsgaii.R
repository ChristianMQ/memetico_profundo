#IRACE 2
#CONJUNTO 92
#nsgaii


#leer matriz de expresi�n y biol�gica reducida
Me<-read.csv("matrizexpresiondistancia_4436_92_completa.csv")
Mb<-read.csv("matrizbiologicadistancia_4436_92_completa.csv")
#Me<-read.csv("matrizexpresiondistancia_3444_95_completa.csv")
#Mb<-read.csv("matrizbiologicadistancia_3444_95_completa.csv")
nombresGenes<-names(Mb)
Me<-Me[,-1]
Mb<-Mb[,-1]

source("memeticoprofundo_para100000eva.R")

N<-4436       #n�mero de genes
k<-4         #n�mero de grupos


###########################
#Par�metros parametrizados#
###########################
P_1<-51     #poblaci�n de agentes capa 1 PPPPPPPPPPPPPPPPPPPP
P_2<-0       #poblaci�n capa 2 P PPPPPPPPPPPPPPPPPPPP
G_2<-0       #generaciones capa 2 PPPPPPPPPPPPPPPPPPPP
Comu<-1       #tipo de comunicaci�n entre poblaciones de la capa 2 PPPPPPPPPPPPPPPPPPPP
Cruz<-0.74    #porcentaje de cruzamiento para duplicar agentes PPPPPPPPPPPPPPPPPPPP


Eva<-0       #contador de evaluaciones
EvaM<-100000   #evaluaciones m�ximas
alpha<-0.5   #factor de Asociaci�n

nombresalida<-"ira2_con92_nsgaii"

guardarSalidas<-1    #0 guarda datos en una lista de la �ltima generaci�n, 1 solo guarda el hipervolumen de la ultima generaci�n, 2 como estaba antes

evaluacionvshipervolumen<<-NULL #variable donde se almacenar� evaluaciones vs hipervolumen

##################################
#Par�metros que ya no se utilizan#
##################################

G_1<-5      #generaciones capa 1 depende de numero de evaluaciones
Sub<-0       #porcentaje de NSGA-II de la capa 2 eliminado
BusL<-0    #porcentaje de Busquedas locales (no tiene busqueda local NSGAII)

Solucion<-deepmemetic100000(N,k,P_1,G_1,P_2,G_2,Cruz,BusL,Sub,Comu,Me,Mb,alpha,EvaM,guardarSalidas,nombresalida)


#graficarSolucion(Solucion)