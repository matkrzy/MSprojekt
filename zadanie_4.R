library(readxl)

#pobranie danych z pliku

dane <- read_excel("data.xlsx")

dane_do_zad_4 <- dane[,2]

#stale potrzebne do obliczen

dokladnosc <- 98
alfa_pol <- (1 - dokladnosc / 100) / 2
jeden_minus_alfa_pol <- 1 - alfa_pol

liczebnosc_proby <- 25
stopnie_swobody <- liczebnosc_proby - 1

#c1, c2 - stale wynikajace z przyjetego modelu obliczen - stosujemy rozklad chi - kwadrat
#X(c1) = 1 - alfa / 2
#X(c2) = alfa / 2
#do funkcji qchisq przekazujemy odwrotnie, poniewaz jako argument nalezy przekazac wartosc (1 - rzad) i ilosc stopni swobody

c1 <- qchisq(alfa_pol, stopnie_swobody)
c2 <- qchisq(jeden_minus_alfa_pol, stopnie_swobody)

#wariancja z proby

wariancja_z_proby <- var(dane_do_zad_4$`1995r`)

#granice przedzialu ufnosci

granica_przedzialu_dolna <- (liczebnosc_proby * wariancja_z_proby) / c2
granica_przedzialu_gorna <- (liczebnosc_proby * wariancja_z_proby) / c1

#precyzja oszacowania - interesuje nas wzgledna, ale do jej obliczenia trzeba obliczyc bezwzgledna

bezwzgledna_precyzja_oszacowania <- (granica_przedzialu_gorna - granica_przedzialu_dolna) / 2
wzgledna_precyzja_oszacowania <- (bezwzgledna_precyzja_oszacowania / wariancja_z_proby) * 100

#czy mozna uogolnic przedzial ufnosci na cala populacje?

if(wzgledna_precyzja_oszacowania <= 5){
  czy_mozna_uogolnic_na_populacje <- "tak"
}else if(wzgledna_precyzja_oszacowania > 10){
  czy_mozna_uogolnic_na_populacje <- "nie"
} else{
  czy_mozna_uogolnic_na_populacje <- "tak, ale z zachowaniem ostroznosci"
}