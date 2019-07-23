library(readr)
library(dplyr)

#Alles im Ordner Fahrzeuge in 1 tibble bringen

#4 Fahrzeuge Dateien 
coltypes <- cols("_","_", "c","D","n","n","l", "D","n")

filename <- "Data\\Fahrzeug\\Fahrzeuge_OEM1_Typ11.csv"
table <- read_csv(filename, col_types = coltypes)

filename2 <- "Data\\Fahrzeug\\Fahrzeuge_OEM1_Typ12.csv"
table2 <- read_csv2(filename2, col_types = coltypes)


coltypes2 <- cols("_", "_", "c","n","n","l","D","n","n","c")
filename3 <- "Data\\Fahrzeug\\Fahrzeuge_OEM2_Typ21.csv"
table3 <- read_csv(filename3, col_types = coltypes2)
table3$origin <- as.Date.character(table3$origin, "%d-%m-%Y")
table3 <- table3 %>% mutate(Produktionsdatum = origin + Produktionsdatum_Origin_01011970) %>% select(-contains("rigin"))

filename4 <- "Data\\Fahrzeug\\Fahrzeuge_OEM2_Typ22.csv"
table4 <- read_csv2(filename4, col_types = coltypes2)
table4$origin <- as.Date.character(table4$origin, "%d-%m-%Y")
table4 <- table4 %>% mutate(Produktionsdatum = origin + Produktionsdatum_Origin_01011970) %>% select(-contains("rigin"))
table4 

cars <- table %>% bind_rows(table2) %>% bind_rows(table3) %>% bind_rows(table4)
summary(cars)
str(cars)

#4 Bestandteile Dateien 
coltypes_parts <- cols("_","c","c","c","c","c")
filename <- "Data\\Fahrzeug\\Bestandteile_Fahrzeuge_OEM1_Typ11.csv"
table <- read_csv2(filename, col_types = coltypes_parts)

filename2 <- "Data\\Fahrzeug\\Bestandteile_Fahrzeuge_OEM1_Typ12.csv"
table2 <- read_csv2(filename2, col_types = coltypes_parts)

filename3 <- "Data\\Fahrzeug\\Bestandteile_Fahrzeuge_OEM2_Typ21.csv"
table3 <- read_csv2(filename3, col_types = coltypes_parts)

filename4 <- "Data\\Fahrzeug\\Bestandteile_Fahrzeuge_OEM2_Typ22.csv"
table4 <- read_csv2(filename4, col_types = coltypes_parts)

parts <- table %>% bind_rows(table2) %>% bind_rows(table3) %>% bind_rows(table4)
summary(parts)
str(parts)

cars_and_parts <- left_join(cars, parts, by="ID_Fahrzeug")
summary(cars_and_parts)
head(cars_and_parts)

