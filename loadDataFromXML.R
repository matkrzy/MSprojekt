library(readxl)
data <- read_excel("~/MSprojekt/data.xlsx")

#mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#show data
View(data)

#quartile type for calculations
q_type<- 5
col1 <-data[,1]
col2 <-data[,2]

#display summary for first column
sumCol1<-summary(col1)
#display summary for sec column
sumCol2<-summary(col2)

mode1 = Mode(col1)
mode2 = Mode(col2)

print("Dominanta dla 1994")
mode1

print("Dominanta dla 1995")
mode2

print("Podsumowanie 1994")
sumCol1

print("Podsumowanie 1995")
sumCol2

#standard deviation for 1994
std_dev_col1<-sd(col1, na.rm = FALSE)

#standard deviation for 1995
std_dev_col2<-sd(col2, na.rm = FALSE)


#variance for 1994
var_col1 = var(col1)

#variance for 1995
var_col2 = var(col2)

describe(col1)
