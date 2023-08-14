#title       : Script de Ingresos Reales
#author      : Rodrigo Franco Fuentes
#20/09/2016

#######################Paquetes
library(foreign) #para leer .dbf
library(stringr) #para str_c
library(dplyr) #para left_join
library(reldist) #wtd.quantile
library(ggplot2) #Gráficas
library(readODS) #Para leer el archivo del deflactor
library(tidyr) # Para gather

############################Trabajar 10 años de ENOE
#file.rename("SDEMT115.dbf", "sdemt115.dbf") #Renombrar un archivo, para manejo programático delos nombres
setwd("E:/Proyectos R/Gaby/data/Sociodemografico")#Cambiar al directorio donde se encuentran las bases de datos
#En este directorio necesitamos las bases de datos en este formato sdemtqyy.dbf
list.files()

ENOEP <- read.dbf("sdemt415.dbf")
ENOEP <- as.tbl(ENOEP)
ENOEP <- filter(ENOEP, C_RES ==1 | C_RES ==3 )
ENOEP <- filter(ENOEP, as.numeric(EDA) >= 14)
ENOEP <- filter(ENOEP, as.numeric(EDA) <= 98)

ENOEPRUEBA <- filter(ENOEP, FAC == 1290)
sum(filter(ENOEP, INGOCUP >0 , INGOCUP < 1290 )$FAC)

########################## Correr desde aquí
df <- data.frame()  
ENOETRANS<-data.frame()
k=0 #Estableciendo un contador
for(i in 2005:2016) 
{
      for (j in 1:4) 
      {
            k=k+1
            ENOE <-str_c(str_c("sdemt",str_c(j,substr(i,3,4))),".dbf") #substr selecciona los dos ultimos digitos 
            print(ENOE)
            df <- read.dbf(ENOE)
            df <- filter(df, R_DEF == "00") #00 Resultado definitivo de la entrevista, entrevista completa
            df$EDA<-as.numeric(df$EDA) #subsets que se necesitan para utilizar la base...
            df <- filter(df, C_RES == 1 | C_RES == 3)
            df$Ing_Pob = as.numeric(df$INGOCUP * df$FAC)  #Creamos una variable para la masa salarial
            dfh<- filter(df, SEX == 1) #Subset hombres
            dfm<- filter(df, SEX == 2) #Subset mujeres
            ENOETRANS[k,1]<-  str_c(i,j)
            ENOETRANS[k,2] <- df %>% summarise(Pob_Total = sum(FAC))  #Población total
            #quantile
            df <- filter(df, EDA >= 14) # de sociodemográficos
            df <- filter(df, EDA <=98)
            for (l in 1:19){       #19 quantiles v2 a v19
                  ENOETRANS[k, l + 2] <- wtd.quantile(df[df$INGOCUP > 0,]$INGOCUP, q = l * 0.05 , weight = df[df$INGOCUP > 0,]$FAC) 
            } #acaba en linea 21
            ENOETRANS[k,22] <- wtd.quantile(df[df$INGOCUP > 0,]$INGOCUP, q =0.99 , weight = df[df$INGOCUP > 0,]$FAC) # el 1 %, ojo ingresos por ocupacion
            ENOETRANS[k,23] <- df %>% 
                  summarise(Pob_mayor_14 = sum(FAC))  #Poblacion total (mayor de 14 menor de 98), después del filtro
            ENOETRANS[k,24] <- df %>%
                  filter(SEX %in% c(1)) %>%  #Hombres
                  summarise(Hombres = sum(FAC))
            ENOETRANS[k,25] <- df %>%
                  filter(SEX %in% c(2)) %>%  #Mujeres
                  summarise(Mujeres = sum(FAC))
            ENOETRANS[k,26] <- df %>%
                  summarise(Masa_salarial = sum((df$Ing_Pob))) 
            ENOETRANS[k,27] <- df %>% 
                  summarise(Masa_salarial_hombres = sum(dfh$Ing_Pob))
            ENOETRANS[k,28] <- df %>% 
                  summarise(Masa_salarial_mujeres = sum(dfm$Ing_Pob))
            ENOETRANS[k,29]<- sum(filter(df, INGOCUP >0 & INGOCUP <= ENOETRANS[k,4])$Ing_Pob) #Masa salarial de la pob con el 10% o menos
            
            ENOETRANS[k,30]<- sum(filter(df, INGOCUP > ENOETRANS[k,4] & INGOCUP <= ENOETRANS[k,6])$Ing_Pob) #Masa salarial de la pob con el 20% o menos
            ENOETRANS[k,31]<- sum(filter(df, INGOCUP > ENOETRANS[k,6] & INGOCUP <= ENOETRANS[k,8])$Ing_Pob) #Masa salarial de la pob con el 30% o menos
            ENOETRANS[k,32]<- sum(filter(df, INGOCUP > ENOETRANS[k,8] & INGOCUP <= ENOETRANS[k,10])$Ing_Pob) #Masa salarial de la pob con el 40% o menos
            ENOETRANS[k,33]<- sum(filter(df, INGOCUP > ENOETRANS[k,10] & INGOCUP <= ENOETRANS[k,12])$Ing_Pob) #Masa salarial de la pob con el 50% o menos
            ENOETRANS[k,34]<- sum(filter(df, INGOCUP > ENOETRANS[k,12] & INGOCUP <= ENOETRANS[k,14])$Ing_Pob) #Masa salarial de la pob con el 60% o menos
            ENOETRANS[k,35]<- sum(filter(df, INGOCUP > ENOETRANS[k,14] &INGOCUP <= ENOETRANS[k,16])$Ing_Pob) #Masa salarial de la pob con el 70% o menos
            ENOETRANS[k,36]<- sum(filter(df, INGOCUP > ENOETRANS[k,16] & INGOCUP <= ENOETRANS[k,18])$Ing_Pob) #Masa salarial de la pob con el 80% o menos
            ENOETRANS[k,37]<- sum(filter(df, INGOCUP > ENOETRANS[k,18] & INGOCUP <= ENOETRANS[k,20])$Ing_Pob) #Masa salarial de la pob con el 90% o menos
            ENOETRANS[k,38]<- sum(filter(df, INGOCUP > ENOETRANS[k,20])$Ing_Pob) #Masa salarial de la pob con el 90% o menos
            ###Población, que deben de ser igualles
            ENOETRANS[k,39]<- sum(filter(df, INGOCUP >0 & INGOCUP <= ENOETRANS[k,4])$FAC) #Masa salarial de la pob con el 10% o menos
            ENOETRANS[k,40]<- sum(filter(df, INGOCUP > ENOETRANS[k,4] & INGOCUP <= ENOETRANS[k,6])$FAC) #10% de la pob
            ENOETRANS[k,41]<- sum(filter(df, INGOCUP > ENOETRANS[k,6] & INGOCUP <= ENOETRANS[k,8])$FAC) #Masa salarial de la pob con el 30% o menos
            ENOETRANS[k,42]<- sum(filter(df, INGOCUP > ENOETRANS[k,8] & INGOCUP <= ENOETRANS[k,10])$FAC) #Masa salarial de la pob con el 40% o menos
            ENOETRANS[k,43]<- sum(filter(df, INGOCUP > ENOETRANS[k,10] & INGOCUP <= ENOETRANS[k,12])$FAC) #Masa salarial de la pob con el 50% o menos
            ENOETRANS[k,44]<- sum(filter(df, INGOCUP > ENOETRANS[k,12] &INGOCUP <= ENOETRANS[k,14])$FAC) #Masa salarial de la pob con el 60% o menos
            ENOETRANS[k,45]<- sum(filter(df, INGOCUP > ENOETRANS[k,14] & INGOCUP <= ENOETRANS[k,16])$FAC) #Masa salarial de la pob con el 70% o menos
            ENOETRANS[k,46]<- sum(filter(df, INGOCUP > ENOETRANS[k,16] &INGOCUP <= ENOETRANS[k,18])$FAC) #Masa salarial de la pob con el 80% o menos
            ENOETRANS[k,47]<- sum(filter(df, INGOCUP > ENOETRANS[k,18] & INGOCUP <= ENOETRANS[k,20])$FAC) #Masa salarial de la pob con el 90% o menos
            ENOETRANS[k,48]<- sum(filter(df, INGOCUP > ENOETRANS[k,20])$FAC) #Masa salarial de la pob con el 90% o menos
            ENOETRANS[k,49]<- sum(filter(df, INGOCUP >0 & SEX == 1)$FAC) #Hombres con ingresos positivos
            ENOETRANS[k,50]<- sum(filter(df, INGOCUP >0 & SEX == 2)$FAC) #Mujeres con ingresos positivos
            ENOETRANS[k,51]<- sum(filter(df,INGOCUP >0)$FAC) #Población mayor de 14 años con ingresos positivos
      }
}

#######Sección con los nombres de las variables 

colnames(ENOETRANS)[29] <- "MaSal10p"
colnames(ENOETRANS)[30] <- "MaSal20p"
colnames(ENOETRANS)[31] <- "MaSal30p"
colnames(ENOETRANS)[32] <- "MaSal40p"
colnames(ENOETRANS)[33] <- "MaSal50p"
colnames(ENOETRANS)[34] <- "MaSal60p"
colnames(ENOETRANS)[35] <- "MaSal70p"
colnames(ENOETRANS)[36] <- "MaSal80p"
colnames(ENOETRANS)[37] <- "MaSal90p"
colnames(ENOETRANS)[38] <- "MaSal100p"
##### Abundancia de personas que ganan 1290 pesos
colnames(ENOETRANS)[39] <- "Pob10p"####
colnames(ENOETRANS)[40] <- "Pob20p"####
colnames(ENOETRANS)[41] <- "Pob30p"####
colnames(ENOETRANS)[42] <- "Pob40p"####
colnames(ENOETRANS)[43] <- "Pob50p"####
colnames(ENOETRANS)[44] <- "Pob60p"####
colnames(ENOETRANS)[45] <- "Pob70p"####
colnames(ENOETRANS)[46] <- "Pob80p"####
colnames(ENOETRANS)[47] <- "Pob90p"####
colnames(ENOETRANS)[48] <- "Pob100p"####
#####
colnames(ENOETRANS)[49] <- "HomInPos"
colnames(ENOETRANS)[50] <- "MujIngPos"
colnames(ENOETRANS)[51] <- "Pob_Ing_Pos"
colnames(ENOETRANS)[1] <- "Año_Trimestre"
colnames(ENOETRANS)[3] <- "q05"
colnames(ENOETRANS)[4] <- "q10"
colnames(ENOETRANS)[5] <- "q15"
colnames(ENOETRANS)[6] <- "q20"
colnames(ENOETRANS)[7] <- "q25"
colnames(ENOETRANS)[8] <- "q30"
colnames(ENOETRANS)[9] <- "q35"
colnames(ENOETRANS)[10] <- "q40"
colnames(ENOETRANS)[11] <- "q45"
colnames(ENOETRANS)[12] <- "q50"
colnames(ENOETRANS)[13] <- "q55"
colnames(ENOETRANS)[14] <- "q60"
colnames(ENOETRANS)[15] <- "q65"
colnames(ENOETRANS)[16] <- "q70"
colnames(ENOETRANS)[17] <- "q75"
colnames(ENOETRANS)[18] <- "q80"
colnames(ENOETRANS)[19] <- "q85"
colnames(ENOETRANS)[20] <- "q90"
colnames(ENOETRANS)[21] <- "q95"
colnames(ENOETRANS)[22] <- "q99"
colnames(ENOETRANS)[1] <- "Año"

####################Nuevas Variables
ENOETRANS<-mutate(ENOETRANS, IngPromGen = Masa_salarial/Pob_Ing_Pos)
ENOETRANS<-mutate(ENOETRANS, IngPromHom = Masa_salarial_hombres/HomInPos)
ENOETRANS<-mutate(ENOETRANS, IngPromMuj = Masa_salarial_mujeres/MujIngPos)
ENOETRANS<-mutate(ENOETRANS, IngProm10p = MaSal10p/Pob10p)
ENOETRANS<-mutate(ENOETRANS, IngProm20p = MaSal20p/Pob20p)
ENOETRANS<-mutate(ENOETRANS, IngProm30p = MaSal30p/Pob30p)
ENOETRANS<-mutate(ENOETRANS, IngProm40p = MaSal40p/Pob40p)
ENOETRANS<-mutate(ENOETRANS, IngProm50p = MaSal50p/Pob50p)
ENOETRANS<-mutate(ENOETRANS, IngProm60p = MaSal60p/Pob60p)
ENOETRANS<-mutate(ENOETRANS, IngProm70p = MaSal70p/Pob70p)
ENOETRANS<-mutate(ENOETRANS, IngProm80p = MaSal80p/Pob80p)
ENOETRANS<-mutate(ENOETRANS, IngProm90p = MaSal90p/Pob90p)
ENOETRANS<-mutate(ENOETRANS, IngProm100p = MaSal100p/Pob100p)

#####################Deflactor 
Deflactor <- read.csv( "E:/Proyectos R/Gaby/data/Deflactor INPC.csv")
Deflactor<- Deflactor[,2]
Deflactorts <- ts(Deflactor, start = c(2005,1), frequency = 12)
Trimestral <- aggregate(Deflactorts, nfrequency = 4)/3
Trimestraldf<- as.data.frame(Trimestral)
colnames(Trimestraldf) <- "Deflactor"
#head(Trimestraldf)
ENOETRANS<- cbind(ENOETRANS,Trimestraldf)

ENOETRANS<- mutate(ENOETRANS, IngPromGenR = (IngPromGen*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngPromHomR = (IngPromHom*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngPromMujR = (IngPromMuj*100)/Deflactor)
#####Resultados promedio por decil
ENOETRANS<- mutate(ENOETRANS, IngProm10pR = (IngProm10p*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngProm20pR = (IngProm20p*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngProm30pR = (IngProm30p*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngProm40pR = (IngProm40p*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngProm50pR = (IngProm50p*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngProm60pR = (IngProm60p*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngProm70pR = (IngProm70p*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngProm80pR = (IngProm80p*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngProm90pR = (IngProm90p*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngProm100pR = (IngProm100p*100)/Deflactor)
##################Resultados por decil 

ENOETRANS<- mutate(ENOETRANS, IngDec10pR = (q10*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngDec20pR = (q20*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngDec30pR = (q30*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngDec40pR = (q40*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngDec50pR = (q50*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngDec60pR = (q60*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngDec70pR = (q70*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngDec80pR = (q80*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngDec90pR = (q90*100)/Deflactor)
ENOETRANS<- mutate(ENOETRANS, IngDec99pR = (q99*100)/Deflactor)


#######################Guardar y leer los archivos
write.csv(ENOETRANS, "E:/Proyectos R/Gaby/Ingreso2.csv") #para guardar el archivo
ENOETRANS<- read.csv("E:/Proyectos R/Gaby/Ingreso2.csv") #para leer el archivo
setwd("E:/Proyectos R/Ingresos Reales")
#################################Proporción de disminución del salario
Prop<-select(ENOETRANS, Año, IngProm10pR:IngProm100pR)
Prop
Prop <- filter(Prop, Año == 20053 | Año == 20162 )
write.csv(Prop, "E:/Proyectos R/Gaby/Ingreso2.csv")

########################Gráficas de los resultados, Generales
P1 <- ggplot(ENOETRANS, aes( x = Año_Trimestre, y = IngPromGenR))
P1 <- P1 + geom_line(aes(color = "red", size = "1.5")) + theme(legend.position="none")
P1 <- P1 + xlab("Año-Trimestre") + ylab("Ingreso Promedio") + ggtitle("Ingreso Promedio en términos Reales")
P1 
ggsave("graphs/Ingreso_real.png", plot = P1, dpi = 500, width = 14, height = 11)


dbG2 <-select(ENOETRANS, Año,IngProm10pR:IngProm90pR)
dbG2 <- gather(dbG2, "Año")
colnames(dbG2)[2] <- "Tipo"
G2 <- ggplot(dbG2, aes(x = Año, y = value, group = Tipo, colour = Tipo)) + geom_line()
G2 <- G2 + ylab("Ingreso mensual promedio") + xlab("Año Trimestre (AAAAT)")
G2 <- G2 +  theme(axis.text.x=element_text(angle=-45, hjust=0.001))
G2

ggsave("graphs/Ingreso_rp_deciles.png", plot = G2, dpi = 1000, width = 7, height = 5.5)
head(dbG2)

##################Resultados por género
colnames(ENOETRANS)

dbG1 <- ENOETRANS[c("Año_Trimestre", "IngPromGenR", "IngPromHomR", "IngPromMujR")]
dbG1<-gather(dbG1, "Año_Trimestre")
colnames(dbG1)[2] <- "Tipo"
#######
P2 <- ggplot(dbG1, aes( x = Año_Trimestre, y = value)) + geom_line(aes(colour = Tipo))
P2 <- P2 + ylab("Ingreso Mensual Promedio") + ggtitle("Ingreso  promedio mensual en términos reales")
P2
ggsave("graphs/Ingreso_real_género.png", plot = P2, dpi = 2000, width = 7, height = 5.5)

head(dbG1)




p2 <-ggplot(ENOETRANS, aes(x = Año_Trimestre, y = "Tipo"))

      
