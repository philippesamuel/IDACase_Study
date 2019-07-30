library(readr)

#Geodaten reinigen
colnames <- c("PLZ", "ORT", "Werk", "Breitengrad", "Laengengrad")
coltypes <- cols("c","c","c","n","n")

filename <- "Data\\Geodaten\\OEM_Werke_2017-07-04_TrR.csv"
table <- read_csv2(filename,col_names = colnames, col_types = coltypes, skip=1)

filename2 <- "Data\\Geodaten\\Tier1_Werke_2017-07-11_v1.2_TrR.csv"
table2 <- read_csv2(filename2, col_names = colnames, col_types = coltypes, skip=1)
table2 <- filter(table2, !is.na(PLZ))

filename3 <- "Data\\Geodaten\\Tier2_Werke_2017-07-11_v1.2_TrR.csv"
table3 <- read_csv2(filename3, col_names = colnames, col_types = coltypes, skip=1)
table3 <- select(table3,1:5)

whole_table <- bind_rows(table, table2)
whole_table <- bind_rows(whole_table, table3)
str(whole_table)
summary(whole_table)

#Deutschland liegt zwischen 47° und 55° Breite und 5° und 15° Länge. Korrigiere vergessene Stellen mit zusätzlichen Nullen.
corrected_table <- whole_table %>% mutate(new_breite = Breitengrad / 1000000) 
corrected_table$new_breite[corrected_table$new_breite < 47] <- corrected_table$new_breite[corrected_table$new_breite < 47] * 10
corrected_table$new_breite[corrected_table$new_breite < 47] <- corrected_table$new_breite[corrected_table$new_breite < 47] * 10

corrected_table <- corrected_table %>% mutate(new_laenge = Laengengrad / 1000000) 
corrected_table$new_laenge[corrected_table$new_laenge < 5] <- corrected_table$new_laenge[corrected_table$new_laenge < 5] * 10
corrected_table$new_laenge[corrected_table$new_laenge < 5] <- corrected_table$new_laenge[corrected_table$new_laenge < 5] * 10

Werke <- corrected_table %>% select(c(-4,-5)) %>% rename(Breitengrad = new_breite, Laengengrad = new_laenge)

#Importiere Gemeinden-Koordinaten
coltypes2 <- cols("_","_","c","c","d","d")
filename4 <- "Data\\Geodaten\\Geodaten_Gemeinden_v1.2_2017-08-22_TrR.csv"
table4 <- read_csv2(filename4, col_types = coltypes2)

#Füge fehlende Nullen bei PLZ hinzu
digit4 <- table4 %>% filter(nchar(Postleitzahl) < 5) %>% mutate(PLZ=paste0("0", Postleitzahl)) %>% select(-1)
digit5 <- table4 %>% filter(nchar(Postleitzahl) == 5) %>% rename(PLZ = Postleitzahl)

Gemeinden <- bind_rows(digit4, digit5)
head(Gemeinden,10)

#2 Tabelle: Werke & Gemeinden. Wie kombinieren?
summary(Werke)
summary(Gemeinden)



