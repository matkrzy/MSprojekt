########### ZADANIE 5 ################


# hipoteza H0 - m1994<m1995
hipoteza <- "Srednia rentownosc firmy poprawila sie w 1995 roku"

library(readxl)
data <- read_excel("data.xlsx")


hipoteza

#przypisanie kolumn danych do zmiennych
col1 <-data[,1]
col2 <-data[,2]

alfa = 0.05

srednia_1994 <- mean(col1$`1994r`)  #srednie
srednia_1995 <- mean(col2$`1995r`)

odchyl_1994 <- sd(col1$`1994r`) #odchylenia standardowe
odychl_1995 <- sd(col2$`1995r`)

wariancja_1994 <- var(col1$`1994r`) #wariancje
wariancja_1995 <- var(col2$`1995r`)
  
n = 25

statystyka <- (srednia_1994-srednia_1995)/sqrt(wariancja_1994/n + wariancja_1995/n)
#statystyka testowa

obsz_kryt1 <- -statystyka*(1-alfa/2) #jedna granica obszaru
obsz_kryt2 <- statystyka*(1-alfa/2) #druga granica obszaru

if (statystyka < obsz_kryt1 || statystyka > obsz_kryt2){
  wynik_hipotezy <- "odrzucamy hipoteze zerowa na poziomie istotnosci 0.05"
}else{
  wynik_hipotezy <- "stwierdzamy brak podstaw do odrzucenia hipotezy"
}

wynik_hipotezy

