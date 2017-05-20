library(readxl)
data <- read_excel("~/MSprojekt/data.xlsx")
options(digits=3)



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
quantile_col1 <- quantile(col1,0.75)
quantile_col2 <- quantile(col2,0.75)

#wspolczynnik zmiennosci
variability_point_col1 = std_dev_col1/average_col1
variability_point_col2 = std_dev_col2/average_col2

#skosnosc
skewnes<-function(std_dev,average,column){
  sum=0
  elements = NROW(column)
  for(i in 1:elements){
    sum=sum+(column[i]-average)^3
  }
  output = sum/elements
  return(output/std_dev^3)
}

#kurtoza
kurtosis<-function(std_dev,average,column){
  sum=0
  elements = NROW(column)
  for(i in 1:elements){
    sum=sum+(column[i]-average)^4
  }
  output = sum/elements
  return(output/std_dev^4)
}

skewnes_col1<-skewnes(std_dev_col1,average_col1,col1)
skewnes_col2<-skewnes(std_dev_col2,average_col2,col2)

kurtosis_col1<-kurtosis(std_dev_col1,average_col1,col1)
kurtosis_col2<-kurtosis(std_dev_col2,average_col2,col2)

summary1=data.frame(
  min = min_col1,
  max = max_col1,
  average = average_col1,
  media = median_col1,
  mode = mode1,
  std_dev = std_dev_col1,
  varianc = var_col1,
  quantile_0.75 = quantile_col1,
  variability_point = variability_point_col1,
  skewnes = skewnes_col1,
  kurtosis = kurtosis_col1
)

summary2=data.frame(
  min = min_col2,
  max = max_col2,
  average = average_col2,
  media = median_col2,
  mode = mode2,
  std_dev = std_dev_col2,
  varianc = var_col2,
  quantile_0.75 = quantile_col2,
  variability_point = variability_point_col2,
  skewnes = skewnes_col2,
  kurtosis = kurtosis_col2
)

View(summary1)
View(summary2)
#szereg
k<-sqrt(NROW(col1));

count_range_width<-function(column){
  (max_col1-min_col1)/k
}


make_string_from_data<-function(column){
  #make sequence based on range and range width
  column_seq =seq(min(column), max(column), by = count_range_width(column)) ;
  #make sequence to 3 numbers after dot
  output<-cut(column,breaks=column_seq,dig.lab = 5,include.lowest = TRUE)
  return(output);
}

draw_plot<-function(data){
  p<-plot(data,col="lightgray",xlab="przedziały", ylab="częstość");
}

col1_string=make_string_from_data(col1);
col2_string=make_string_from_data(col2);


draw_plot(col1_string);
draw_plot(col2_string);