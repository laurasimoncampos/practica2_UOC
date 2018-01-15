
#######################################################################
# PRACTICA 2 - TIPOLOGIA DE LOS DATOS
# LAURA SIMON CAMPOS lsimonca
#######################################################################


datos<-read.csv("C:\\UOC\\tipologia\\Practica 2\\athletes.csv")
View(datos)

########## LIMPIEZA DE LOS DATOS ###########

id<-datos$id
summary(id)



# --- Nacionality-----
# La nacionalidad es una abreviacion de tres caracteres
# Visualizamos los datos para comprobar:
#    su formato (mayusculas/minusculas, < o > de 3 caracteres)
#    o para comprobar si hay registros vacios


nationality<-table(datos$nationality)
print(nationality)

nac<-datos$nationality
vectorIdNac<-c()
for (j in 1:length(nac)) {
  if(nac[j]=="" || is.na(nac[j]))
  {
    print("nacionalidades vacias")
    vectorIdNac<-c(vectorIdNac,id[j])
    
  }
}

print(length(vectorIdNac))
#No se observan nacionalidades mal escritas o registros vacios




# --- Sex-----
# El sexo se representa por: female o male
# Visualizamos los datos agrupados para ver si hay strings mal escritos o registros vacios
# Comprobamos que el total de hombres y mujeres coincide con el total de registros
# Por ultimo cambiamos el nombre male por "M" y female por "F" 

sex<-table(datos$sex)
print(sex)

sexo<-datos$sex
vectorSexo<-c()
for(j in 1:length(sexo)){
  
  if(sexo[j]=="male"){
    vectorSexo[j]<-"M"
  }
  else{
    vectorSexo[j]<-"F"
  }
}
print(length(vectorSexo))
#No se observan palabras mal escritas en el genero



# --- Sport-----
# Comprobamos que las no existas disciplinas iguales escritas de diferente manera
# Comrpobamos si hay registros vacios
sports<-table(datos$sport)
print(sports)
deportes<-datos$sport
vectorIdSpot<-c()

for (j in 1:length(deportes)) {
  if(deportes[j]=="" || is.na(deportes[j]))
  {
    print("deportes vacios")
    vectorIdSpot<-c(vectorIdSpot,id[j])
    
  }
}
print(length(vectorIdSpot))

#no se observan disciplinas mal escritas ni vacias





# --- dob-----
# Se usa la fecha de nacimiento para calcular la edad de los atletas
# Comprobamos si existen fechas vacias
# La fecha de nacimiento tiene formato m/d/y (año con dos cifras). Modificamos a 4 cifras
# Comprobamos si se pasa de los limites de edad

fechaNacimiento<-datos$dob

id<-datos$id
print(id)
vectorId<-c()
for (j in 1:length(fechaNacimiento)) {
  if(fechaNacimiento[j]=="" || is.na(fechaNacimiento[j]))
  {
    print(" fecha vacias")
    vectorId<-c(vectorId,id[j])
    
  }
}
print(vectorId[1])
#Como solo hay un registro con valor vacio y conocemos el nombre del atleta,
#buscamos en Internet la fecha de nacimiento y lo completamos
#El atleta tiene es Pavel Sozykin y ha nacido el 12/25/87


fechOlim<-2016
vectorEdad<-c()
vectorIdFecha<-cbind(vectorId[1],"12/25/87")
print(vectorIdFecha)

for (j in 1:length(fechaNacimiento)) {
  if(id[j]==vectorIdFecha[,1])
  {
    fechaNacimiento[j]<-vectorIdFecha[,2]
    print(" fecha vacias")
    fechasVacias<-fechasVacias+1
    edad<-edad<-fechOlim-fecNac
    
  }

    ano<-unlist(strsplit(sub("/(..$)", "/-\\1",fechaNacimiento[j]) , "-"))

    if(as.numeric(ano[2]) > 5)
    {
      f<- paste("19",ano[2],sep="") 
    }
    else
    {
      f<- paste("20",ano[2],sep="") 
    }

    fecNac<-as.numeric(f)
    print(fecNac)
    
    if(fecNac>2002 || fecNac<1954){
      print("fechas fuera de limites")
      print("---------------------------------------------")
    }
    else{
      edad<-fechOlim-fecNac
      
    }
    
  vectorEdad<-c(vectorEdad,edad)
  
}

print(length(vectorEdad))
# Acabamos de generar un vector con las edades de los atletas





# --- medal-----
# Miramos si existen campos vacios
# Agrupamos los valores de gold, silver y bronce para especificar si tiene medalla o no

print(table(datos$gold))
print(table(datos$silver))
print(table(datos$bronze))

#Hay valor para todos los registros

medallaOro<-datos$gold
medallaSilver<-datos$silver
medallaBronze<-datos$bronze

medal<-c()
for(j in 1:length(datos$id)){

  if(medallaOro[j]!=0 || medallaSilver[j]!=0 || medallaBronze[j]!=0){
    print("tiene medalla")
    medal[j]<-"T"
  }
  else{
    print("no tiene medalla")
    medal[j]<-"F"
  }
}





# --- Generamos el CSV con lo datos resultantes ---
nacionality<-datos$nationality
sex<-vectorSexo
sport<-datos$sport
age<-vectorEdad

atletas<-data.frame(nacionality,sex,age,sport,medal)


write.csv(atletas,file="C:\\UOC\\tipologia\\Practica 2\\datasetAtletas.csv",row.names = FALSE, col.names = FALSE)





#---- CARGAMOS EL NUEVO CSV ---

datosNuevos<-read.csv("C:\\UOC\\tipologia\\Practica 2\\datasetAtletas.csv")
View(datosNuevos)
summary(datosNuevos)


########## ANALISIS DE LOS DATOS ###########


# --- edad 
edad<-datosNuevos$age
length(edad)
mean(edad)
median(edad)
sd(edad)
var(edad)
quantile(edad,c(0.25,0.5,0.75))
summary(edad)

hist(edad)
plot(edad)
boxplot(edad)

# --- sex
sexo<-datosNuevos$sex
summary(sexo)
plot(sexo)
plot(sport,sexo)
plot(sport,edad)

# --- nacionality
nac<-datosNuevos$nacionality
summary(nac)
plot(nac)


# --- medal
medal<-datosNuevos$medal
summary(medal)
plot(medal)

plot(sport,medal)

# --- deporte
sport<-datosNuevos$sport
summary(sport)
plot(sport)
plot(sport,edad)



#---Varianza edad - sexo
t.test(edad~sexo)
var.test(edad~sexo)

rnorm(edad, mean=0, sd=1)

t.test(edad~datosNuevos$medal)
var.test(edad~datosNuevos$medal)


reglin<-lm(edad~sex)
reglin
plot(edad,sex)
abline(reglin,col=2)


reglin<-lm(edad~medal)
reglin
plot(edad,medal)
abline(reglin,col=2)


# --- REPRESENTACIONES GRAFICAS, TABLAS
tabla<-table(sexo,sport) 
tabla 

tabla1<-table(edad,sport) 
tabla1

tabla2<-table(sexo,edad) 
tabla2



tabla5<-table(medal,nac) 
tabla5


