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

#Aquí se importa los datos con reducción con TSNE (debe estar guardado el csv en la carpeta de trabajo)
tsne_results = read.csv("data_tsne.csv", header = TRUE, sep = ",", row.names = NULL)
colnames(tsne_results) = c("V1", "V2") 

#Aquí se importa la base de datos (Tambíen debe estar guardado en la carpeta de trabajo)
beats = readRDS("beats.rds")

#Se hace limpieza de datos
beats = beats[!duplicated(beats[c("danceability","energy","loudness","acousticness","instrumentalness","liveness","valence")]),]

#Aquí se genera la reducción de dimensionalidad con los datos con PCA 
PCA <- function(beats){
  #Valores repetidos
  beats = beats[!duplicated(beats[c("danceability","energy","loudness","acousticness","instrumentalness","liveness","valence")]),]
  
  data <- beats #Guardamos un data frame con el nombre data, que será utilizado durante todo el código
  
  #Dejamos solo las columnas que interesan. En este caso se eliminan datos no númericos, excepto los que son llaves primarias que identifican las canciones
  data <- data[, ! names(data) %in% c("artist_name", "album_id", "album_type", "album_release_date", "album_release_year", "album_release_date_precision", "analysis_url", "disc_number", "explicit", "track_href", "is_local", "track_name", "track_preview_url", "track_number", "type", "track_uri", "external_urls.spotify", "album_name", "key_name", "mode_name"), drop = F]
  
  #Se crea una variable solo con las variables númericas, esto para analizar correlaciones y normalizar correctamente 
  data_num = select_if(data,is.numeric)
  
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
  
  return(PCA_data_num_norm)
}
PCA_data_num_norm = PCA(beats)
#---------------------------------------------------------------------------

#Cluesterización

#Se hace Kmeans 
#..........................................................................
#Con PCA
set.seed(123)
modelo_kmeans_norm <- kmeans(PCA_data_num_norm[,(1:2)], 11, iter.max  = 10) #En este caso se eligen 11 clusters y 10 iteraciones

#--------------------------------------------------------------------------
#PROCESO PARA LA CREACIÓN DE LA LISTA

#Se crea un data frame con los datos iniciales, pero cada uno pertenece a un cluster

#Aquí hay que elegir si usar el modelo con GMM (a=0) o si con Kmeans (a!=0). Empezar con TSNE(b=0) o PCA(b!=0)
#Dentro del ciclo se usa PCA dado que es más eficiente.

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
      modelo_gmm_pca <- Mclust(PCA_data_num_norm, G=3, modelNames = "EII")
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

################################################################################

id <- readline(prompt="ID de la canción: ")
nombre <- readline(prompt="Nombre de la canción: ")

df_final = ciclo(decision(1,1),id, nombre, 1, 1)
playlist = playlist(df_final, id, nombre)

direccion = "C:/Users/drago/OneDrive/Escritorio/UAI/5to semestre/Mineria de datos/Proyecto 1/"
write.csv(playlist1, paste(direccion, "playlist.csv", sep=""), row.names = FALSE)

