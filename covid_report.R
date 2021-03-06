covid_report <- function(country,...){

#Install reshape2 package if is necessary
if(!require(reshape2)){
    install.packages("reshape2")
    library(reshape2)
}

#Data
input   <- read.csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", stringsAsFactors = TRUE, check.names = F, header = T)

#Preparing data
input$"Province/State" <- ifelse(nchar(as.character(input$"Province/State")) == 0, as.character(input$"Country/Region"), as.character(input$"Province/State"))
input   <- input[, !(names(input) %in% c("Lat", "Long"))]
dates   <- names(input)[-c(1, 2)] 
start.D <- as.Date(dates[1], format = "%m/%d/%y")
end.D   <- as.Date(tail(dates, 1), format = "%m/%d/%y")


data <- NULL
for(i in country){
  out  <- as.numeric(input[input$"Province/State" == i, ][-c(1, 2)]) 
  data <- cbind(data, out)
}
  
for(i in seq_along(country)) {
  colnames(data)[i] <- country[i]
}
  
data1 <- data.frame(Date = seq(from = start.D, to = end.D, by = "day"), data)

output <- reshape2::melt(data1, id = "Date")
colnames(output) <- c("Date", "Country", "Confirmed cases")
return(output)

}

#Report of confirmed cases by country by day
dat <- covid_report(country = c("Mexico", "Peru", "Chile"))
head(dat) 
