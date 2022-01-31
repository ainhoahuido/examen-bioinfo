library(nycflights13)
library(tidyverse)
weather <- nycflights13::weather

# 2
#¿Cuántas columnas y cuántos registros tiene este dataset?
ncol(weather)
nrow(weather)

# origin distintos y para cada uno registros
unique(weather$origin)
sum(weather$origin=="JFK")
sum(weather$origin=="EWR")
sum(weather$origin=="LGA")

#LGA mediana wind_speed y media de pressure
LGA <-weather[weather$origin=="LGA", ]
median(LGA$wind_speed)
mean((LGA$pressure), na.rm = T)
table(is.na(LGA$pressure))

# quitar NA de wind_gust, calcula para cada mes la media wind_speed y wind_gust y el numero de casos
table(is.na(weather$wind_gust))

weather2 <- weather[!is.na(weather$wind_gust),]
mediames <- 0
mes <- 0



media_mes <- 0
media <- function(columna)
{
  for(a in 1:12)
  { 
    x <- filter(columna, month == a)
    media_mes <-mean(columna, month == a)
    print(paste0("En ", a, " la media es ", media_mes))
  }
}


valoresmax_temp <- 0
max_originyhora <- function(data)
{
  for(a in 0:23)
  {
    x <- filter(data, hour == a)
    valoresmax_temp[a+1] <- max(x$temp_humid, na.rm = T)
    print(paste0("A las ", a, " el máximo valor es ", valoresmax_temp[a+1]))
  }



numero_casos <- function(columna)
{
  b <- sum(is.na(columna)==FALSE)
  return(b)
}
numero_casos(weather2$wind_gust)


media <- function(data,columna)
{
  a<-mean(data$columna)
  return(a)
}

media(weather2$wind_gust)

media_mes <- 0
media4 <- function(data,columna)
{
  for(a in 1:12)
  { 
    x <- filter(data$columna, month == a)
    media_mes <-mean(data$columna, month == a)
    print(paste0("En ", a, " la media es ", media_mes))
  }
}

weather_completo <- weather2[complete.cases(weather2),]
media_wind_speed <- aggregate(weather_completo$wind_speed, list(weather_completo$month), mean)
names(media_wind_speed) <- c("mes", "media wind_speed")
media_wind_speed

media_wind_gust <- aggregate(weather_completo$wind_gust, list(weather_completo$month), mean)
names(media_wind_gust) <- c("mes", "media wind_gust")
media_wind_gust


# 3. recrea el plot

EWR <- weather2[weather2$origin == "EWR",]
JFK <- weather2[weather2$origin == "JFK",]
LGA <- weather2[weather2$origin == "LGA",]


par(mfrow = c(1,3))
boxplot(EWR$temp ~ EWR$month, ylab = "ºC", xlab= "Months",  col="red", main = "EWR")
boxplot(JFK$temp ~ JFK$month, ylab = "ºC", xlab= "Months",  col="green", main = "JFK")
boxplot(LGA$temp ~ LGA$month, ylab = "ºC", xlab= "Months",  col="blue", main = "LGA")


plot_meteo <- function(data,columna1, columna2, titulo, unidades)
{
  dat <- data.frame(data)
  x <- dat[,columna1]
  y <- dat [, columna2]
  boxplot(x ~ y, ylab = unidades, xlab= "Months",  col="pink", main = titulo)
  media <- c(mean(EWR$temp,na.rm=T),mean(LGA$temp,na.rm=T),mean(JFK$temp,na.rm=T))
  
  return(media)
  print(media)
  
  
}

plot_meteo(weather, "temp", "month", "Punto de rocio", "ºC")
plot_meteo(weather, "humid", "month", "Humedad", "Relative humidity")


# 4. Qué correlación tuvieron la temperatura y humedad en cada uno de los origin? Plotealo mediante puntos con ggplot.

cor.test(EWR$temp, EWR$humid)
library(ggplot2)
ggplot(data = EWR) +
  geom_point(mapping = aes(x = temp, y = humid))

cor.test(JFK$temp, JFK$humid)
ggplot(data = JFK) +
  geom_point(mapping = aes(x = temp, y = humid))

cor.test(LGA$temp, LGA$humid)
ggplot(data = LGA) +
  geom_point(mapping = aes(x = temp, y = humid))

#temperatura JFK y LGA p value y boxplot
jfk_temp <- data.frame(JFK$origin, JFK$temp)
lga_temp <- data.frame(LGA$origin, LGA$temp)
jun <- data.frame(LGA, JFK)
j <- data.frame()

t.test(weather$temp ~ weather$origin == "LGA")
t.test(weather$temp ~ weather$origin == "JFK")
par(mfrow = c(1,2))
boxplot(weather$temp ~ weather$origin == "LGA")
boxplot(weather$temp ~ weather$origin == "JFK")

par(mfrow = c(1,1))
boxplot(weather$temp ~weather$origin, xlab = "Origen", 
        ylab = "Temperatura", 
        main = "Temperatura vs Origen")


