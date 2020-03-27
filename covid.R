
covid_series <- function(country, ...){
  #Data
  input <- read.csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", stringsAsFactors = TRUE, check.names = F, header = T)
  input$`Province/State` <- ifelse(nchar(as.character(input$`Province/State`)) == 0, as.character(input$`Country/Region`), as.character(input$`Province/State`))
  
  
  #Preparing data
  input <- input[, !(names(input) %in% c("Lat", "Long"))]
  dates <- names(input)[-c(1, 2)] 
  
  data <- NULL
  for(i in country){
    out  <- as.numeric(input[input$`Province/State` == i, ][-c(1, 2)]) 
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
  
  
  report <- data[, ncol(data)]
  names(report) <- country
  cat("Reported positive cases:\n\n" )
  print(sort(report))
}


#RUN FUNCTION
covid_series(country = c("Mexico", "Peru", "Oregon", "California", "France", "Washington"))
# You will have a plot and a little table with positive confirmed cases by country/state.
