library(readxl)
dane_z_pliku <- read_excel("data.xlsx")

dane_do_zad3 <- dane_z_pliku[,1]

wartosc_krytyczna_t_studenta <- qt(0.99,24) # Wartość krytyczna z rozkładu t-studenta  (współczynni ufności 0.98, próba 25)
            # próba -1 bo tak sie to po prostu liczy
            # a 99 a nie 98 bo:
            # "Kilku słów wymaga wartość 0.995 (nie 0.99) w wywołaniu funkcji rozkładu t-Studenta. Rozkład ten jest zwykle stosowany w kontekście dwustronnym, dlatego obszar krytyczny dzielimy
            # równomiernie na obu „końcach” rozkładu. 99% ufności oznacza, że krytyczny 1% jest podzielony
            # na 2 końce i zawiera się w przedziałach (0, 0.05) oraz (0.995, 1). Wartość tablicowa jest kwantylem
            # obliczonym dla takiego właśnie prawdopodobieństwa. Analogicznie np. dla 95% będzie to 0.975,
            # a dla 99.9% — 0.9995."
            #cytując z internetów

srednia_z_proby <- mean(dane_do_zad3$`1994r`) #logiczne 
odchylenie_standardowe_z_proby <- sd(dane_do_zad3$`1994r`) # logiczne

pier <- sqrt(24) # potrzebne mi do wzoru :: pierwiastek z wielkosci proby pomniejszonej o jeden
skladnik <- wartosc_krytyczna_t_studenta * odchylenie_standardowe_z_proby / pier #takie cos co sie powtarza we wzorze

lewa_granica_przedialu <- srednia_z_proby - skladnik
prawa_granica_przedialu <- srednia_z_proby + skladnik
#Szacowana średnia zawiera się między lewą granicą a prawą granicą przediału (początek, koniec przedziału)

#Względna precyzja oszacowania
#Wzór mówi, że jest to bezwzględny bład szacunku (czyli to co we wzorze na granice przeniału raz się dodaje a raz odejmuje)
#Przez średnią z próby

Wzgledna_precyzja_oszacowania <- skladnik / srednia_z_proby * 100 # mnożenie przez 100 tylko po to, żebyh mieć w procentach

# Sprawdzić czy mamy podstawy to uogulnić 
if(Wzgledna_precyzja_oszacowania <= 5)
{
  czy_mozna_uogulnic_na_populacje_zad3 <- "tak"
}else if (Wzgledna_precyzja_oszacowania > 10)
{
  czy_mozna_uogulnic_na_populacje_zad3 <- "nie"
}else 
{
  czy_mozna_uogulnic_na_populacje_zad3 <- "tak, ale z zachowaniem ostroznosci"
}
