library("dplyr")
library("plyr")
library("ggcorrplot")
library("ggplot2")
library("caret")
library("factoextra")
library("dbscan")
library("cluster")
pacman::p_load(dbscan, tidyverse, Rtsne)
library("NbClust")
library("mclust")
#---------------------------------------------------------------
#Preprocesamiento:

tsne_results = read.csv("data_tsne.csv", header = TRUE, sep = ",", row.names = NULL)
colnames(tsne_results) = c("V1", "V2") 

#Aquí se importa la base de datos (Tambíen debe estar guardado en la carpeta de trabajo)
beats = readRDS("beats.rds")

#Valores repetidos

beats = distinct(beats) #Efectivamente hay datos repetidos, se eliminan

#Ahora vemos si hay canciones que son iguales en valores pero difieren en otros valores
beats = beats[!duplicated(beats[c("track_name", "track_id")]),]
beats = beats[!duplicated(beats[c("danceability","energy","loudness","acousticness","instrumentalness","liveness","valence")]),]

#Con esto vemos cuantas canciones con el mismo nombre existen. En este punto si bien se llaman igual y pueden ser del mismo artista, no son iguales, difieren en valores esenciales que hacen que no sean la misma.
A=as.data.frame(sort(table(beats$track_name),decreasing = TRUE))
A=A[A$Freq != 1,]
example = beats[beats$track_name == "Intro",] #Está linea muestra los datos de las canciones con el nombre dado

data <- beats #Guardamos un data frame con el nombre data, que será utilizado durante todo el código

#Veemos si hay valores nulos y de que tipo son
as.matrix(sapply(data,class)) #Veemos en que formato están las variables del dataframa
as.matrix(sapply(data, function(x) sum(is.na(x)))) # Datos NA hay: album_release_year = 419 # track_preview_url= 80015
as.matrix(sapply(data, function(x) sum(is.null(x)))) #NINGUN DATO NULO

#De aquí se puede determinar que hay solo 2 variables que necesitan un preprocesamiento especifico, pero se determinan que no son de interes, por lo que no son tomadas en cuenta a la hora del siguiente analisis

#Dejamos solo las columnas que interesan. En este caso se eliminan datos no númericos, excepto los que son llaves primarias que identifican las canciones
data <- data[, ! names(data) %in% c("artist_name", "album_id", "album_type", "album_release_date", "album_release_year", "album_release_date_precision", "analysis_url", "disc_number", "explicit", "track_href", "is_local", "track_name", "track_preview_url", "track_number", "type", "track_uri", "external_urls.spotify", "album_name", "key_name", "mode_name"), drop = F]

#Se crea una variable solo con las variables númericas, esto para analizar correlaciones y normalizar correctamente 
data_num = select_if(data,is.numeric)

#Veemos las correlaciones de los datos numericos
ggcorrplot((round(cor(data_num),2)), type = "lower",lab = TRUE) #En este caso se identifican 7 variables con correlaciones interesantes

#Ahora se escogen solamente los datos con correlaciones fuertes (tanto negativas como positivas)
data_num <- data_num[, ! names(data_num) %in% c("key", "mode", "speechiness", "tempo", "time_signature", "duration_ms"), drop = F]

#Se normalizan los datos
preproc <- preProcess(data_num, method=c("range"))
preproc <- predict(preproc, data_num)

#Se hace reducción de dimensionalidad para los datos normalizados y sin normalizar. Se compara TSNE con PCA

#PCA
set.seed(123)
PCA_data_num_norm = prcomp(preproc)
PCA_data_num_norm = as.data.frame(predict(PCA_data_num_norm))

#TSNE
#set.seed(123)
#tsne_results <- Rtsne(preproc)
#tsne_results = as.data.frame(tsne_results$Y)

#Se guarda en un csv el TSNE de los datos ya que se demora mucho en correr cada vez el código
#write.csv(tsne_results,"C:/Users/drago/OneDrive/Escritorio/UAI/5to semestre/Mineria de datos/Proyecto 1/data_tsne.csv", row.names = FALSE)

#----------------------------------------------------------------------------
#Visualización

#Grafica los datos con reducción de dimensión

#PCA
ggplot(PCA_data_num_norm[,(1:2)], aes(PC1, PC2)) + geom_point()

#TSNE
plot(tsne_results$V1,tsne_results$V2)

#En este caso con TSNE se puede visualizar de mejor manera, por lo que en temas de reducción de dimensionalidad es mejor opción, pero el proceso es demasiado complejo, dura más de 10 minutos y no es óptimo para el cálculo. PCA es rápido pero no muy eficaz a la hora de visualizarse, ya que el gráfico muestra una mancha con los datos amontonados.
#---------------------------------------------------------------------------

#Cluesterización

#Muestra cuantos clusters usar. En este caso se muestra un gráfico que indica la distancia entre los centroides y el número de clusters(k). Cuando se estabilice la linea o haya una bajada notoria es un buen indicio para elegir el número inicial de clusters
#____________

#Con PCA_data KMEANS
set.seed(123)
# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:25,  function(k){
  model <- kmeans(x = PCA_data_num_norm, centers = k)
  model$tot.withinss
})
# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:25,
  tot_withinss = tot_withinss
)
# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() + geom_point()+
  scale_x_continuous(breaks = 1:25)

#6 clusters con 4 iter y 11 clusters con 10 iter

#Con TSNE_data KMEANS
set.seed(123)
# Use map_dbl to run many models with varying value of k (centers)
tot_withinss_tsne <- map_dbl(1:25,  function(k){
  model <- kmeans(x = tsne_results, centers = k)
  model$tot.withinss
})
# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:25,
  tot_withinss = tot_withinss_tsne
)
# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() + geom_point()+
  scale_x_continuous(breaks = 1:25)

#10 clusters y 5 clusters

#En este caso se eligen la mayor cantidad de datos posibles, ya que al ser tantos datos, conviene que se dividan de la mayor manera posible

#____________

#Se hace Kmeans 
#..........................................................................
#Con PCA
set.seed(123)
modelo_kmeans_norm <- kmeans(PCA_data_num_norm[,(1:2)], 11, iter.max  = 10) #En este caso se eligen 11 clusters y 10 iteraciones

#Con TSNE
set.seed(123)
modelo_kmeans_tsne <- kmeans(tsne_results, 10, iter.max  = 10) #10 Clusters con 10 iteraciones

#table(modelo_kmeans_tsne$cluster) Con esta función se va viendo cuantos datos hay por cluster, se va variando el número de iteraciones para ver cuando convergen los valores. En este caso con 10 iteraciones es suficiente.

#Visualización de Clusters
set.seed(123)
plot(PCA_data_num_norm[,(1:2)],col=modelo_kmeans_norm$cluster) #Kmeans con PCA
set.seed(123)
plot(tsne_results,col=modelo_kmeans_tsne$cluster) #Kmeans con TSNE

#En ambos casos hay una buena clusterización.

#EVALUACIONES

# calculamos las distancias de los datos
(modelo_kmeans_norm$withinss)
(modelo_kmeans_tsne$withinss)

#En este caso la sumatoria total de la distancia que hay entre el centroide y los datos del cluster son menores cuando se usa PCA, por lo que con Kmeans es mejor usar este modelo.

#...........................................................................

#Se hace GMM

#PCA
set.seed(123)
modelo_gmm_pca <- Mclust(PCA_data_num_norm[,(1:2)], G=11, modelNames = "EII")

#TSNE
set.seed(123)
modelo_gmm_tsne <- Mclust(tsne_results, G=10, modelNames = "EII")

#Visualización de Clusters
set.seed(123)
plot(PCA_data_num_norm[,(1:2)],col=modelo_gmm_pca$classification) #GMM con PCA

set.seed(123)
plot(tsne_results,col=modelo_gmm_tsne$classification) #GMM con TSNE

#En este caso con PCA quedan mucho mejor los clusters

#Evaluación de clusters

#En este caso no hay una forma simple de evaluar GMM.

#...........................................................................

#--------------------------------------------------------------------------
#PROCESO PARA LA CREACIÓN DE LA LISTA

#Se crea un data frame con los datos iniciales, pero cada uno pertenece a un cluster

#Aquí se van probando cuatro combinaciones iniciales para crear la lista. (GMM/Kmeans con TSNE, o GMM/Kmeans con PCA. En cualquier caso el código luego sigue con el modelo especificado pero con PCA) )

#Aquí hay que elegir si usar el modelo con GMM (a=0) o si con Kmeans (a!=0). Empezar con TSNE(b=0) o PCA(b!=0)
#Dentro del ciclo se usa PCA dado que es más eficiente.

id = "6ayeqYtOtwVhqVB6k6MKoh"
nombre = "California Love"

decision <- function(a,b){
  #Está función hace un merge entre los datos y la columna con el número correspondiente de cada dato a su cluster perteneciente
  if(a==0){
    if(b==0){
      #GMM con TSNE
      df_final<-data.frame(beats,modelo_gmm_tsne$classification)
    }
    else{
      #GMM con PCA
      df_final<-data.frame(beats,modelo_gmm_pca$classification)
    }
  }
  if(a==1){
    if(b==0){
      #Kmeans con TSNE
      df_final<-data.frame(beats,modelo_kmeans_tsne$cluster)
    }
    else{
      #Kmeans con PCA
      df_final<-data.frame(beats,modelo_kmeans_norm$cluster)
    }
  }
  return(df_final)
}

ciclo <- function(df_final, id, nombre, a, b){
  
  while(TRUE){ #Ciclo que va ir haciendo clusters de clusters
    
    #Aquí se ve el número de cluster al que pertenece la canción, y luego se filtra solo para dejar datos que pertenezcan a ese cluster
    
    #numero = filter(df_final,track_name==nombre)$modelo_kmeans_norm.cluster
    
    if(a==0){
      if(b==0){
        #GMM con TSNE
        numero = filter(df_final,track_id==id)$modelo_gmm_tsne.classification
        musica = filter(df_final,modelo_gmm_tsne.classification==numero)
        completo <- musica[, ! names(musica) %in% c("modelo_gmm_tsne.classification"), drop = F]
        b=1
      }
      else{
        #GMM con PCA
        numero = filter(df_final,track_id==id)$modelo_gmm_pca.classification
        musica = filter(df_final,modelo_gmm_pca.classification==numero)
        completo <- musica[, ! names(musica) %in% c("modelo_gmm_pca.classification"), drop = F]
      }
    }
    else{
      if(b==0){
        #Kmeans con TSNE
        numero = filter(df_final,track_id==id)$modelo_kmeans_tsne.cluster
        musica = filter(df_final,modelo_kmeans_tsne.cluster==numero)
        completo <- musica[, ! names(musica) %in% c("modelo_kmeans_tsne.cluster"), drop = F]
        b=1
      }
      else{
        #Kmeans con PCA
        numero = filter(df_final,track_id==id)$modelo_kmeans_norm.cluster
        musica = filter(df_final,modelo_kmeans_norm.cluster==numero)
        completo <- musica[, ! names(musica) %in% c("modelo_kmeans_norm.cluster"), drop = F]
      }
      #Este dataframe va a guardar los datos originales antes del proceso, luego se indicarán en que cluster pertenecen
    }
    
    
    data <- musica #Se actualiza la data con solo los datos que están en el cluster de la canción solicitada
    
    #Ahora se hace lo hecho previamente con la data
    #++++++++++++++++++++++++++++++++++++
    data <- data[, ! names(data) %in% c("artist_name", "album_id", "album_type", "album_release_date", "album_release_year", "album_release_date_precision", "analysis_url", "disc_number", "explicit", "track_href", "is_local", "track_name", "track_preview_url", "track_number", "type", "track_uri", "external_urls.spotify", "album_name", "key_name", "mode_name", "modelo_kmeans_norm.cluster"), drop = F] #Dejamos solo las columnas que interesan 
    data_num = select_if(data,is.numeric)#Se crea una variable solo con las variables númericas
    data_num <- data_num[, ! names(data_num) %in% c("key", "mode", "speechiness", "tempo", "time_signature", "duration_ms"), drop = F] #Ahora se escogen solamente los datos con correlaciones fuertes
    
    preproc <- preProcess(data_num, method=c("range")) #Se normalizan los datos
    preproc <- predict(preproc, data_num)
    
    PCA_data_num_norm = prcomp(preproc) #Se hace reducción de dimensionalidad para los datos normalizados y sin normalizar
    PCA_data_num_norm = as.data.frame(predict(PCA_data_num_norm))
    
    #Se hace Kmeans o GMM con una semilla para que de valores constantes
    set.seed(123)
    if(a==0){
      modelo_gmm_pca <- Mclust(PCA_data_num_norm, G=3, modelNames = "EII") #En este caso se elige arbitrariamente 3 clusters
    }
    else{
      modelo_kmeans_norm <- kmeans(PCA_data_num_norm, 3, iter.max  = 10)
    }
    #+++++++++++++++++++++++++++++++++++
    
    #Se ponen ciertas condiciones para que los clusters finales contengas datos que cumplan con crear una lista de 3 horas de música
    
    if(sum(df_final[, 'duration_ms']) <= 15000000){ #Si la suma del data frame de los datos de los clusters de clusters es menor o igual a 4.1667 horas, se rompe el ciclo
      break
    }
    
    if(nrow(df_final)<=500){ #Si el número de datos del data frame de los clusters de clusters es menor o igual a 500, se rompe el ciclo
      break
    }
    
    #Se va creando un data frame con los datos con sus respectivos clusters, los cuales despues van siendo filtrados
    
    if(a==0){
      df_final<-data.frame(completo,modelo_gmm_pca$classification)
    }
    else{
      df_final<-data.frame(completo,modelo_kmeans_norm$cluster)
    }
  }
  df_final = df_final[!duplicated(df_final[c("track_name", "artist_name")]),]
  return(df_final)
}

#Aquí se guardan los dataframes con las canciones más parecidas a las solicitadas con los distintos criterios
df_final = ciclo(decision(0,0),id, nombre, 0, 0) #GMM con TSNE y luego GMM con PCA
df_final2 = ciclo(decision(0,1), id, nombre, 0, 1) #GMM con PCA y luego GMM con PCA
df_final3 = ciclo(decision(1,0),id, nombre, 1, 0) #Kmeans con TSNE y luego Kmeans con PCA
df_final4 = ciclo(decision(1,1),id, nombre, 1, 1) #Kmeans con PCA y luego Kmeans con PCA
#--------------------------------------------------------------------------

#CUMPLIR TIEMPO

playlist <- function(df_final, id, nombre){
  #Lo más probable es que el data frame luego de hacer clusters de clusters sea mayor a lo solicitado, esto debido a las condiciones puestas arriba. En lo siguiente se acortará esta lista para que cumpla.
  
  #Se guarda la fila de datos de la canción solicitada
  nro = filter(df_final,track_id==id)
  
  posicion = which(df_final$track_id == id)
  
  distance <- vector(mode = "list", length = nrow(df_final)) #Se crea un vector con datos nulos para luego ir rellenandolos
  
  to_calculate = df_final[,c('energy','loudness')]
  
  #Con este ciclo se va calculando la distancia entre la canción y las demás canciones. X = danceability Y = valence, debido a que poseen una correlación de 0.63 y son datos con valores en el mismo rango. Se van guardando los valores en el vector vacío.
  xy <- preProcess(to_calculate, method=c("range"))
  xy <- predict(xy, to_calculate)
  
  for(i in 1:nrow(df_final)){
    if(df_final$track_id[i] != nro$track_id){
      calculate = sqrt(((xy$energy[posicion[1]] - xy$energy[i])^2)+((xy$loudness[posicion[1]] - xy$loudness[i])^2))
      distance[i] = calculate
    }
    else{
      distance[i] = 0
    }
  }
  
  #Luego se pasa el vector distancia a dataframe y se une con el dataframe de las canciones más parecidas a la solicitada
  distance = t(as.data.frame(distance))
  rownames(distance) = NULL
  df_final <- cbind(df_final, distance)
  
  #Luego se ordenan de menor a mayor en distancia
  df_final = df_final[(order(df_final$distance)),]
  rownames(df_final) = NULL
  
  #Luego se crea una playlist con las canciones más parecidad que dure aproximadamente 3 horas.
  suma = 0
  cont = 0
  for(i in 1:nrow(df_final)){
    suma = sum(df_final[1:i, 'duration_ms'])
    cont = cont+1
    if (suma>=10800000){
      playlist = df_final[1:cont,]
      break
    }
  }
  
  playlist = playlist[,c('track_name','artist_name', 'duration_ms')]
  return(playlist)
}

#Aquí se guardan las playlist que cumple con los requisitos solicitados
playlist1 = playlist(df_final, id, nombre)
playlist2 = playlist(df_final2, id, nombre)
playlist3 = playlist(df_final3, id, nombre)
playlist4 = playlist(df_final4, id, nombre)

direccion = "C:/Users/drago/OneDrive/Escritorio/UAI/5to semestre/Mineria de datos/Proyecto 1/"

#Aquí se guardan para ser almacenadas y poder ser vistas de mejor manera. (No es necesario correr esta parte)
for(i in 1:4){
  if(i==1){
    write.csv(playlist1, paste(direccion, "playlist1.csv", sep=""), row.names = FALSE)
  }
  if(i==2){
    write.csv(playlist1,paste(direccion, "playlist2.csv", sep=""), row.names = FALSE)
  }
  if(i==3){
    write.csv(playlist1,paste(direccion, "playlist3.csv", sep=""), row.names = FALSE)
  }
  if(i==4){
    write.csv(playlist1,paste(direccion, "playlist4.csv", sep=""), row.names = FALSE)
  }
}

#Conclusión: Las playlists quedan de manera similar, ninguna queda con canciones a simple vista parecidas, pero si cuentan con valores y esencias parecidas. De esto no se puede determinar cual usar.
#Los gráficos hechos con los datos clusterizados, muestran que los datos con Kmeans (TSNE y PCA) y GMM (PCA) quedan de buena manera, por lo que se descarte usar GMM con TSNE.
#Luego al evaluar las distancias en Kmeans con los datos y los centroides, se ve que PCA es mejor opción que TSNE.
#Por ultimo se determina usar Kmeans con PCA en vez de GMM, debido a su eficiencia y facilidad de uso. Los gráficos de GMM y Kmeans con PCA son similares, por lo que es una decisión coherente.