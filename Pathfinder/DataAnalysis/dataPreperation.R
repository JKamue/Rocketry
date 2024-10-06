library(ggplot2)
library(pracma)
library(dplyr)
library(plotly)
library(htmlwidgets)
library(webshot2)

csv_einlesen <- function(name) {
  dateiname <- paste("data/",name,".csv" , sep = "")
  dateiname
  csv_inhalt <- read.csv(dateiname, sep=",", encoding='UTF-8',header=TRUE)
  return(csv_inhalt)
}

seconds_milliseconds <- function(x) {
  s <- floor(x %% 60)  # Seconds part (within 60 seconds)
  ms <- round((x - floor(x)) * 1000)  # Milliseconds part
  sprintf("%02d:%03d", s, ms)  # Format as "seconds:milliseconds"
}

track <- csv_einlesen('pathfinder-launch-1');
colnames(track) <- c("Milliseconds",
                          "Temperature",
                          "Pressure",
                          "Altitude",
                          "Acceleration_X",
                          "Acceleration_Y",
                          "Acceleration_Z",
                          "Gyro_X",
                          "Gyro_Y",
                          "Gyro_Z",
                          "GPS_Count",
                          "GPS_HDOP",
                          "GPS_Latitude",
                          "GPS_Longitude",
                          "GPS_Altitude",
                          "GPS_Course",
                          "GPS_Speed",
                          "GPS_Time")

## Add relevant features and information

# Convert track time
track$Milliseconds <- track$Milliseconds - min(track$Milliseconds)
track$Seconds <- track$Milliseconds / 1000

# Calculate Tick Rate
track$TimeDiff <-  c(0, diff(track$Milliseconds))

# Calculate relative altitude
track$Altitude <- track$Altitude - min(track$Altitude)

# Add smoothened altitude curve
track$Altitude.Lowess <- lowess(track$Altitude, f = 0.1)$y


# Calculate information regarding peak
max_alt_index <- which.max(track$Altitude)
max_alt_value <- track$Altitude[max_alt_index]
max_seconds <- track$Seconds[max_alt_index] 

# Calculate the derivative (velocity) for raw altitude and LOWESS-smoothed altitude
track$Altitude.Velocity.Pracma <- gradient(track$Altitude, track$Seconds)
track$Altitude.Velocity.Lowess.Pracma <- gradient(track$Altitude.Lowess, track$Seconds)

# Calculate acceleration as the derivative of velocity
track$Altitude.Acceleration.Pracma <- gradient(track$Altitude.Velocity.Pracma, track$Seconds)
track$Altitude.Acceleration.Lowess.Pracma <- gradient(track$Altitude.Velocity.Lowess.Pracma, track$Seconds)

# Set the first acceleration to zero to avoid issues (optional, depending on your needs)
track$Altitude.Acceleration.Pracma[1] <- 0
track$Altitude.Acceleration.Lowess.Pracma[1] <- 0

# Summe Accerlation Gyro
track$Acceleration_Total <- sqrt(track$Acceleration_X^2 + track$Acceleration_Y^2 + track$Acceleration_Z^2)
track$Acceleration_Total <- track$Acceleration_Total - track$Acceleration_Total[1]

# Velocity from GPS
track$GPS_Speed_ms <- track$GPS_Speed * 1000 / (60*60)


track_unique <- track %>%
  distinct(GPS_Longitude, GPS_Latitude, .keep_all = TRUE)

ggplot(track, aes(x = Seconds)) +  # Map the converted seconds to the x-axis
  geom_line(aes(y = GPS_HDOP, color = "Speed"), size = 0.7)  +
  #ylim(-15,60) +
  
  scale_x_continuous(breaks = pretty(track$Seconds, n = 10), labels = seconds_milliseconds) +
  labs(x = "Time (s:ms)", y = "Speed (inm/s)", color = NULL) +  # Add x-axis label as "Time"
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = c(0.85, 0.1)) + # Adjust position inside plot 
  labs(title = "Rocket total speed", 
       subtitle = "Based on BE-220 GPS measurements",
       caption = "Pathfinder launch 2024-09-29")


ggplot(track, aes(x = Seconds)) +  # Map the converted seconds to the x-axis
  geom_line(aes(y = GPS_Speed_ms, color = "Speed"), size = 0.7)  +
  #ylim(-15,60) +
  
  scale_x_continuous(breaks = pretty(track$Seconds, n = 10), labels = seconds_milliseconds) +
  labs(x = "Time (s:ms)", y = "Speed (inm/s)", color = NULL) +  # Add x-axis label as "Time"
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = c(0.85, 0.1)) + # Adjust position inside plot 
  labs(title = "Rocket total speed", 
       subtitle = "Based on BE-220 GPS measurements",
       caption = "Pathfinder launch 2024-09-29")


library(dplyr)
library(geosphere)

calculate_distances_no_altitude <- function(data) {
  # Initialize a vector to hold the distances
  distances <- numeric(nrow(data) - 1)
  
  # Calculate the distance for each pair of consecutive points
  for (i in 1:(nrow(data) - 1)) {
    # Get the current point
    point1_lat <- data$GPS_Latitude[i]
    point1_lon <- data$GPS_Longitude[i]
    
    # Get the next point
    point2_lat <- data$GPS_Latitude[i + 1]
    point2_lon <- data$GPS_Longitude[i + 1]
    
    # Calculate the horizontal distance using Haversine
    horizontal_distance <- distHaversine(c(point1_lon, point1_lat), c(point2_lon, point2_lat))
    
    # Calculate the total distance
    distances[i] <- horizontal_distance
  }
  
  # Return the distances with an additional 0 for the first row
  return(c(0, distances))  # First point has no previous point
}



calculate_distances <- function(data) {
  # Initialize a vector to hold the distances
  distances <- numeric(nrow(data) - 1)
  
  # Calculate the distance for each pair of consecutive points
  for (i in 1:(nrow(data) - 1)) {
    # Get the current point
    point1_lat <- data$GPS_Latitude[i]
    point1_lon <- data$GPS_Longitude[i]
    point1_alt <- data$Altitude[i]
    
    # Get the next point
    point2_lat <- data$GPS_Latitude[i + 1]
    point2_lon <- data$GPS_Longitude[i + 1]
    point2_alt <- data$Altitude[i + 1]
    
    # Calculate the horizontal distance using Haversine
    horizontal_distance <- distHaversine(c(point1_lon, point1_lat), c(point2_lon, point2_lat))
    
    # Calculate the vertical distance
    vertical_distance <- point2_alt - point1_alt
    
    # Calculate the total distance
    distances[i] <- sqrt(horizontal_distance^2 + vertical_distance^2)
  }
  
  # Return the distances with an additional 0 for the first row
  return(c(0, distances))  # First point has no previous point
}

# Add the distances to the data frame
track$Distance <- calculate_distances(track)
track$Distance.Horizontal <- calculate_distances_no_altitude(track)

track$Distance.Lowess <- lowess(track$Distance, f = 0.1)$y
track$Distance.Horizontal.Lowess <- lowess(track$Distance.Horizontal, f = 0.1)$y

ggplot(track, aes(x = Seconds)) +  # Map the converted seconds to the x-axis
  geom_line(aes(y = Distance.Lowess, color = "Speed LOWESS"), size = 1, linetype = "solid") +
  geom_line(aes(y = Distance, color = "Speed"), size = 0.7)  +
  #ylim(-15,60) +
  
  scale_x_continuous(breaks = pretty(track$Seconds, n = 10), labels = seconds_milliseconds) +
  labs(x = "Time (s:ms)", y = "Speed (inm/s)", color = NULL) +  # Add x-axis label as "Time"
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = c(0.85, 0.1)) + # Adjust position inside plot 
  labs(title = "Rocket total speed", 
       subtitle = "Based on BE-220 GPS measurements",
       caption = "Pathfinder launch 2024-09-29")


track$Total.Distance <- cumsum(track$Distance)
track$Total.Distance.Lowess <- cumsum(track$Distance.Lowess)
track$Total.Distance.Horizontal <- cumsum(track$Distance.Horizontal)
track$Total.Distance.Horizontal.Lowess <- cumsum(track$Distance.Horizontal.Lowess)




track$GPS_Baro_Total_Speed <- gradient(track$Total.Distance.Lowess, track$Seconds)
track$GPS_Total_Horizontal_Speed <- gradient(track$Total.Distance.Horizontal.Lowess, track$Seconds)



track$Altitude.Velocity.Lowess.Pracma.Abs <- abs(track$Altitude.Velocity.Lowess.Pracma)

track$speed_deviation <- track$GPS_Baro_Total_Speed - track$Altitude.Velocity.Lowess.Pracma.Abs

