#IRACE 2
#CONJUNTO 95
#nsgaii


#leer matriz de expresión y biológica reducida
#Me<-read.csv("matrizexpresiondistancia_4436_92_completa.csv")
#Mb<-read.csv("matrizbiologicadistancia_4436_92_completa.csv")
Me<-read.csv("matrizexpresiondistancia_3444_95_completa.csv")
Mb<-read.csv("matrizbiologicadistancia_3444_95_completa.csv")
nombresGenes<-names(Mb)
Me<-Me[,-1]
Mb<-Mb[,-1]

source("memeticoprofundo_para100000eva.R")

N<-3444       #número de genes
k<-4         #número de grupos


###########################
#Parámetros parametrizados#
###########################
P_1<-51     #población de agentes capa 1 PPPPPPPPPPPPPPPPPPPP
P_2<-0       #población capa 2 P PPPPPPPPPPPPPPPPPPPP
G_2<-0       #generaciones capa 2 PPPPPPPPPPPPPPPPPPPP
Comu<-1       #tipo de comunicación entre poblaciones de la capa 2 PPPPPPPPPPPPPPPPPPPP
Cruz<-0.74    #porcentaje de cruzamiento para duplicar agentes PPPPPPPPPPPPPPPPPPPP


Eva<-0       #contador de evaluaciones
EvaM<-100000   #evaluaciones máximas
alpha<-0.5   #factor de Asociación

nombresalida<-"ira2_con95_nsgaii"

guardarSalidas<-1    #0 guarda datos en una lista de la última generación, 1 solo guarda el hipervolumen de la ultima generación, 2 como estaba antes

evaluacionvshipervolumen<<-NULL #variable donde se almacenará evaluaciones vs hipervolumen

##################################
#Parámetros que ya no se utilizan#
##################################

G_1<-5      #generaciones capa 1 depende de numero de evaluaciones
Sub<-0       #porcentaje de NSGA-II de la capa 2 eliminado
BusL<-0    #porcentaje de Busquedas locales (no tiene busqueda local NSGAII)

Solucion<-deepmemetic100000(N,k,P_1,G_1,P_2,G_2,Cruz,BusL,Sub,Comu,Me,Mb,alpha,EvaM,guardarSalidas,nombresalida)


#graficarSolucion(Solucion)