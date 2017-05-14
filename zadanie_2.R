######################################
########### ZADANIE 2 ################
######################################
## alfa = 0.05
## hipoteza H0 - rozk?ad normalny

library(readxl)
data <- read_excel("~/MSprojekt/data.xlsx")

#przypisanie kolumn danych do zmiennych
col1 <-data[,1]
col2 <-data[,2]

#sortowanie kolumn
col1=sort(col1)
col2=sort(col2)

#wyswietlenie
col1
col2

###########################################
#wartosci oszacowanych parametrow rozkladu normalnego
#na podstawie wzorow podanych w ksiazce str 132.
m1=0
for(i in 1:25)
	m1=m1+col1[i]
m1=(1/25)*m1

sigma1=0
for(i in 1:25)
	sigma1=sigma1+((col1[i]-m1)^2)
sigma1=sqrt((1/25)*sigma1)

m2=0
for(i in 1:25)
	m2=m2+col2[i]
m2=(1/25)*m2

sigma2=0
for(i in 1:25)
	sigma2=sigma2+((col2[i]-m2)^2)
sigma2=sqrt((1/25)*sigma2)

#wyswietlenie
m1
sigma1
m2
sigma2

########################################
#Standaryzacja x dla kazdej z wartosci oraz kolumn
#tworzymy funkcje oparte na wzorach

stX1=function(x)
{
(x-m1)/sigma1
}

stX2=function(x)
{
(x-m2)/sigma2
}

#tworzymy kolumny standaryzowanych wartosci x
sX1=col1
sX2=col2

for(i in 1:25)
	sX1[i]=stX1(col1[i])

for(i in 1:25)
	sX2[i]=stX2(col2[i])

#wywietlenie
sX1
sX2

##########################################
#stworzenie kolumny i/n 
#dla obu kolumn bedzie dokladny taka sama
i_n=col1
for(i in 1:25)
	i_n[i]=i/25
#wyswietlenie
i_n

###########################################
#pobranie z tablic Ko?mogorowa warto?ci F0 dla warto?ci standaryzowanych x
F01=col1
F02=col2

F01[1]=0.02
F01[2]=0.04
F01[3]=0.05
F01[4]=0.10
F01[5]=0.25
F01[6]=0.29
F01[7]=0.32
F01[8]=0.38
F01[9]=0.40
F01[10]=0.44
F01[11]=0.46
F01[12]=0.47
F01[13]=0.48
F01[14]=0.48
F01[15]=0.53
F01[16]=0.59
F01[17]=0.63
F01[18]=0.63
F01[19]=0.63
F01[20]=0.80
F01[21]=0.82
F01[22]=0.87
F01[23]=0.90
F01[24]=0.91
F01[25]=0.99

F02[1]=0.00
F02[2]=0.14
F02[3]=0.16
F02[4]=0.17
F02[5]=0.17
F02[6]=0.22
F02[7]=0.26
F02[8]=0.28
F02[9]=0.29
F02[10]=0.30
F02[11]=0.32
F02[12]=0.40
F02[13]=0.48
F02[14]=0.57
F02[15]=0.65
F02[16]=0.65
F02[17]=0.75
F02[18]=0.75
F02[19]=0.77
F02[20]=0.77
F02[21]=0.83
F02[22]=0.85
F02[23]=0.91
F02[24]=0.95
F02[25]=0.96

##################################
# i/n - F(0)

i_nF01=col1
i_nF02=col2

for(i in 1:25)
	i_nF01[i]=abs(i_n[i]-F01[i])

for(i in 1:25)
	i_nF02[i]=abs(i_n[i]-F02[i])

##################################
# (i-1)/n - F(0)

i1_nF01=col1
i1_nF02=col2

i1_nF01[1]=0
i1_nF02[1]=0

for(i in 2:25)
	i1_nF01[i]=abs(i_n[i-1]-F01[i])

for(i in 2:25)
	i1_nF02[i]=abs(i_n[i-1]-F02[i])

#################################
#stworzenie tabeli podsumowujacej

tabela1=data.frame(x=col1, standX=sX1, "i/n"=i_n, "F0(x)"=F01, "i/n-F0(x)"=i_nF01, "i-1/n-F0(x)"=i1_nF01)
tabela2=data.frame(x=col2, standX=sX2, "i/n"=i_n, "F0(x)"=F02, "i/n-F0(x)"=i_nF02, "i-1/n-F0(x)"=i1_nF02)

tabela1
tabela2
##################################
#wartosc krytyczna dla poziomu istotnosci 0.05 z tablic wynosi d=0.264
#Tabela 1:
#najwi?ksze wartosci z dw?ch ostatnich kolumn to: d1=0.13 oraz d2=0.10
#Tabela 2:
#najwi?ksze wartosci z dw?ch ostatnich kolumn to: d1=0.12 oraz d2=0.11
################################# 
#Wybieramy wi?ksz? warto?? z naszych obu kolumn dla ka?dej z tablic d11=0.13 d22=0.12
#Poniewa? 0.13 oraz 0.12 <0.264, wi?c test Ko?mogorowa nie odrzuca hipotezy zerowej , m?wi?cej
#o tym, ?e jest to rok?ad normalny

######################################
############# KONIEC #################
######################################