set.seed(1234567890)
library(geosphere)
# Importing Data
stations_data <- read.csv("stations.csv")
temps_data <- read.csv("temps50k.csv")
input_data <- merge(stations_data,temps_data,by="station_number")

# Function to return the gaussian keranal value
gaussian_KernalValue <- function(distance, h)
{
  return(exp(-((distance^2)/(2*(h^2)))))
}

# Function to computer distance between stations and calculate gaussain kernal value based on computed distance
stationDistance_KeralValue <- function(input_data_locations, prediction_location, h)
{
  distance = distHaversine(input_data_locations, prediction_location)
  print(distance)
  kernalValue = gaussian_KernalValue(distance, h)
  plot(x=(distance/1000), y=kernalValue, type="o", ylab = "kernel Value", xlab="Distance(Kilometers)", 
       main = "Station Distance - Kernal Value Plot")
  return(kernalValue) 
}

# Function to computer distance between days and calculate gaussain kernal value based on computed distance
dayDistance_KernalValue <- function(input_data_dates, prediction_date, h)
{
  distance <- abs(as.numeric(as.Date(input_data_dates) - as.Date(prediction_date)))
  kernalValue = gaussian_KernalValue(distance, h)
  plot(x=(distance/360), y=kernalValue, type="p", ylab = "kernel Value", xlab="Distance(Years)", 
       main = "Day Distance - Kernal Value Plot", xlim=c(4,15))
  return(kernalValue) 
}

# Function to computer distance between time and calculate gaussain kernal value based on computed distance
timeDistance_KernalValue <- function(input_data_time, prediction_time, h)
{
  distance <- difftime(strptime(input_data_time , format = "%H:%M:%S"), 
                        strptime(prediction_time , format = "%H:%M:%S"), units = "hours")
  distance <- as.numeric(abs(distance))
  kernalValue = gaussian_KernalValue(distance, h)
  plot(x=distance, y=kernalValue, type="p", ylab = "kernel Value", xlab="Distance(Hours)", 
       main = "Time Distance - Kernal Value Plot")
  return(kernalValue) 
}

input_data_locations = data.frame(input_data$longitude, input_data$latitude)

# Adding prediction date and time
prediction_location = c(15.6333, 58.4166)
prediction_date = "2021-08-14" 
plot_time = "04:00:00"
# Removing posterior data
input_data = input_data[which(as.Date(input_data$date) < as.Date(prediction_date)),]

# Approximating the values for smoothing coefficent based on graphs of distance and kernal values
h_distance = 40000
h_date = 750
h_time = 2

times = c("04:00:00", "06:00:00","08:00:00","10:00:00","12:00:00","14:00:00",
          "16:00:00","18:00:00","20:00:00","22:00:00", "24:00:00")

kernal_stationDistance = stationDistance_KeralValue(input_data_locations, prediction_location, h_distance)
kernal_dayDistance = dayDistance_KernalValue(input_data$date,prediction_date,h_date)
kernal_timeDistance = timeDistance_KernalValue(input_data$time,plot_time,h_time)

predictions_sum_kernal = list()
predictions_product_kernal = list()
count = 1

# Predicting based on sum of kernals and product of kernals
for(time in times)
{
  kernal_timeDistance = timeDistance_KernalValue(input_data$time, time, h_time)
  sum_kernal = kernal_stationDistance + kernal_dayDistance + kernal_timeDistance
  product_kernal = kernal_stationDistance * kernal_dayDistance * kernal_timeDistance
  
  predictions_sum_kernal[count] = sum(sum_kernal * input_data$air_temperature)/sum(sum_kernal)
  predictions_product_kernal[count] = sum(product_kernal * input_data$air_temperature)/sum(product_kernal)
  count = count + 1
}

# Plotting graph of sum of kernals and product of kernals
plot(x = c(1:11), y = predictions_sum_kernal, col = "red", type = "o", xlab = "Time", ylab = "Temperature", 
     xaxt="n", ylim = c(2,10), main ="Predicted temperature")
axis(1, at=1:length(predictions_sum_kernal), labels=times)
points(x = c(1:11), predictions_product_kernal, type="o", col="blue")
legend("topleft",fill=c("red","blue"), legend=c("Sum Kernal Predictions","Product Kernal Predictions"),bty="n")






