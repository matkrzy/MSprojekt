library(readxl)
data <- read_excel("~/MSprojekt/data.xlsx")

col1 <-data[,1]
col2 <-data[,2]

#mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#min
min_col1<-min(col1)
min_col2<-min(col2)

#max
max_col1<-max(col1)
max_col2<-max(col2)

#median
median_col1<-median(col1)
median_col2<-median(col2)

#average
average_col1 <-mean(col1)
average_col2 <-mean(col2)

#mode - dominanta
mode1 = Mode(col1)
mode2 = Mode(col2)


#standard deviation for 1994
std_dev_col1<-sd(col1)

#standard deviation for 1995
std_dev_col2<-sd(col2)


#variance for 1994
var_col1 = var(col1)

#variance for 1995
var_col2 = var(col2)

#quartiles
