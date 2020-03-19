
covid_series <- function(country, ...){
  #Data
  input <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", check.names = F, stringsAsFactors = T, header = T)
  
  #Preparing data
  input <- input[, !(names(input) %in% c("Province/State", "Lat", "Long"))]
  dates <- names(input)[-1] 
  
  data <- NULL
  for(i in country){
    out  <- as.numeric(input[input$`Country/Region` == i, ][-1]) 
    data <- rbind(data, out)
  }
  
  #Section plot
  plot(data[1, ], type = "n", bg = "blue", pch = 21, lwd = 2, axes = FALSE, xlab = "", ylab = "Cases", ylim = c(0, 1.1*max(data)))
  axis(1, at = seq(1, length(dates)), labels = dates, las = 2)
  axis(2, las = 2)
  box()
  grid()
  
  for(i in 1:length(country)){
    lines(data[i, ], lwd = 2, col = i)
  }
  
  legend("topleft", c(country), fill = c(seq_along(country)), bty = "n")
}

covid_series(country = c("Mexico","Argentina","Chile","Venezuela","Colombia", "Peru"))


