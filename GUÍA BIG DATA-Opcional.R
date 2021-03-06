#instalcion del pacmkages
install.packages(c("readxl", "dplyr"))
library(readxl)
library(dplyr)
library(readxl)

# importar la base de dato (X246544_Data_Nomofobia)
datos <- read_excel("C:/Users/nicol/OneDrive/Escritorio/r/246544_Data_Nomofobia.xlsx")
View(datos)

#renombrar variable 

names (datos) # nombres de las variables y su posicion
names (datos)[5] ="Hab_blandas"       # renombrar variables 
names (datos)[6] ="Re_conflictos"     # renombrar variables 
names (datos)[7] ="Tie_celular"     # renombrar variables 
dummy <- as.numeric(G�nero == Mujer)

# ############ 1 Realice una estad�stica descriptiva de los datos e interprete percentiles, media, moda, mediana.###########
summary(datos)

############# 2 Realice un test de normalidad, curtosis y asimetr�a. ###########
#instalcion del pacmkages
install.packages(datos)
library(moments)
library(nortest)
library(normtest)

head(datos)
attach(Nomofobia)
ad.test (Nomofobia) #test de normalidad (Prueba de Anderson-Darling)
ad.test (Ansiedad)  #test de normalidad (Prueba de Anderson-Darling)


curtosis ( Nomofobia $ Ansiedad )
kurtosis(datos)
Kurtosis (Nomofobia)
asimetr�a (Nomofobia)

###########3 Utilizando la materia vista en clases, obtenga una regresi�n simple e imprima susresultados.

regresion <- lm(Nomofobia ~ Ansiedad, data = datos)
regresion <- lm(Nomofobia ~ Ansiedad + Estr�s + Compulsividad +Hab_blandas + Re_conflictos + Tie_celular + G�nero + Edad, data = datos)
summary(regresion)
########## 4 Interprete los resultados del modelo anterior.#####

########### 5. Del modelo anterior, obtenga un an�lisis de varianza (ANOVA) e interprete#######
anova(regresion)
#interpretar 
########### Genere 3 gr�ficos e interprete (histograma, boxplot y barras). Recuerde establecer nombres a sus variables dentro del gr�fico.

hist(x = datos$Nomofobia,main="Nomofobia por individuo",col="#996600",border="#ff9900",breaks=15,xlab="Nomofobia",ylab="N� individuos")
boxplot(x = datos$Nomofobia, y = datos$Tie_celular, main = "Nomofobia por tiempo en el celular", xlab = "Nomofobia", ylab = "tiempo en el celular")
tab <- table(datos$Estr�s, datos$G�nero)

ggplot(tab, main = "Nomofobia por tiempo en el celular", xlab = "Nomofobia", ylab = "tiempo en el celular")


#########   7. Determine los outliers de cada variable, si es que tiene
boxplot(datos$Estr�s, main = "estr�s")
boxplot(datos$Ansiedad , main = "Ansiedad")
boxplot(datos$Compulsividad, main = "Compulsividad")
boxplot(datos$Hab_blandas , main = "Hab_blandas")
boxplot(datos$Re_conflictos, main = "Re_conflictos")
boxplot(datos$Tie_celular , main = "Tie_celular")
boxplot(datos$Nomofobia , main = "nomofobia")
boxplot(datos$Edad , main = "edad")

############8
library(tsoutliers)
outliers <- tso(Re_conflictos) 
plot(outliers)
