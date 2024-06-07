#selección de mi dataset
futbol <- read.csv("/Users/aliochaterzer/Library/Mobile Documents/com~apple~CloudDocs/Documents/IFTS18/Desarrollo IA/footballteam.csv")
futbol

#Analisis de mi dataset
View(futbol)
colSums(is.na(futbol))
summary(futbol)

#Limpieza de la columna Market_Value
futbol$Market_value <- gsub("€|m"," ", futbol$Market_value)
futbol$Market_value <- as.numeric(gsub("bn", "", futbol$Market_value))
futbol$Market_value <- as.numeric(futbol$Market_value)
# Realiza la sustitución y la multiplicación por 1000 solo para la fila 1
indice_fila <- 1
futbol$Market_value[indice_fila] <- as.numeric(gsub("€|m|bn", "", futbol$Market_value[indice_fila])) * 1000
#Cambio el nombre de la columna
names(futbol)[names(futbol) == "Market_value"] <- "Market_Value_in_millions"

#Limpieza de la columna Market_Value of Players
futbol$Market_value_of_players <- as.numeric(gsub("€|m","",futbol$Market_value_of_players))

#Limpieza de la columna MV_Top_18_players
futbol$MV_Top_18_players <- as.numeric(gsub("€|m","",futbol$MV_Top_18_players))

#Limpieza de la comlumna Share_of_MV
futbol$Share_of_MV <- as.numeric(gsub("%", "", futbol$Share_of_MV))

#Agrago una nueva columnua (Country)
library(dplyr)
futbol$Country <- "Other"

#Y hago un programa que según el nombre de la competición, me indica el país
futbol$Country <- case_when(
  grepl("Premier League", futbol$Competition) ~ "England",
  grepl("Serie A", futbol$Competition) ~ "Italy",
  grepl("LaLiga", futbol$Competition) ~ "Spain",
  grepl("Ligue 1", futbol$Competition) ~ "France",
  grepl("Süper Lig", futbol$Competition) ~ "Turkey",
  TRUE ~ futbol$Country
)

#Modifico el orden de las columnas
indice_competition <- which(names(futbol) == "Competition")
# Insertar la columna "Country" justo después de la columna "Competition"
futbol <- cbind(futbol[, 1:indice_competition], futbol$Country, futbol[, (indice_competition + 1):ncol(futbol)])
# Elimino la columna "Country" original para que no esté 2 veces
futbol <- futbol[, -which(names(futbol) == "Country")]

#Cambio el nombre de la columna
names(futbol)[names(futbol) == "futbol$Country"] <- "Country"


summary(futbol)

View(futbol)
head(futbol)

futbolBI <- "/Users/aliochaterzer/Library/Mobile Documents/com~apple~CloudDocs/Documents/IFTS18/Desarrollo IA/futbolBI.csv"

# Guarda el dataframe modificado en un nuevo archivo CSV
write.csv(futbol, file = futbolBI, row.names = FALSE)

futbolparaarbol <- subset(futbol, select = -c(Club, Competition))
View(futbolparaarbol)




