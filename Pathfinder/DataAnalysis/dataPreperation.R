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
#track$Acceleration_Total <- track$Acceleration_Total - track$Acceleration_Total[1]

# Velocity from GPS
track$GPS_Speed_ms <- track$GPS_Speed * 1000 / (60*60)


track_unique <- track %>%
  distinct(GPS_Longitude, GPS_Latitude, .keep_all = TRUE)


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

# Smoothen for both vertical and horizontal
track$Distance.Lowess <- lowess(track$Distance, f = 0.1)$y
track$Distance.Horizontal.Lowess <- lowess(track$Distance.Horizontal, f = 0.1)$y

# Calculate the total distances from the distance between each point
track$Total.Distance <- cumsum(track$Distance)
track$Total.Distance.Lowess <- cumsum(track$Distance.Lowess)
track$Total.Distance.Horizontal <- cumsum(track$Distance.Horizontal)
track$Total.Distance.Horizontal.Lowess <- cumsum(track$Distance.Horizontal.Lowess)

# Derive speed from distance
track$GPS_Baro_Total_Speed <- gradient(track$Total.Distance.Lowess, track$Seconds)
track$GPS_Total_Horizontal_Speed <- gradient(track$Total.Distance.Horizontal.Lowess, track$Seconds)

track$Altitude.Velocity.Lowess.Pracma.Abs <- abs(track$Altitude.Velocity.Lowess.Pracma)
track$speed_deviation <- track$GPS_Baro_Total_Speed - track$Altitude.Velocity.Lowess.Pracma.Abs

# Calculate altitude relative to start
track$GPS_Altitude.Adjusted <- track$GPS_Altitude - min(track$GPS_Altitude)

# Calculate timeframe of high HDOP
high_hdop <- track[track$GPS_HDOP > 10,]
high_hdop_start <- min(high_hdop$Seconds)
high_hdop_end <- max(high_hdop$Seconds)


# Calculate GPS max altitude data
max_alt_index_gps <- which.max(track$GPS_Altitude.Adjusted)
max_alt_value_gps <- track$GPS_Altitude.Adjusted[max_alt_index_gps]
max_seconds_gps <- track$Seconds[max_alt_index_gps] 

# Delay adjust GPS
gps_time_delay <- max_alt_index_gps - max_alt_index
track$GPS_Altitude.Adjusted.NoDelay <- c(track$GPS_Altitude.Adjusted[(gps_time_delay+1):nrow(track)], rep(NA, gps_time_delay))

# Calculate orientation of rocket
track <- track %>%
  mutate(
    angle_X = cumsum(Gyro_X * TimeDiff/1000),  
    angle_Y = cumsum(Gyro_Y * TimeDiff/1000), 
    angle_Z = cumsum(Gyro_Z * TimeDiff/1000)  
  )

# Rocket was at an angle on the pole and moving around
track$angle_X = track$angle_X + 15 * (pi/180)

# Calculate own rotation in degrees
track$Gyro_Y_deg <- track$Gyro_Y * 180 / pi

# Calculate amount of rotations
track$angle_Y_rotations <- track$angle_Y / (2*pi)

# Load simulated data
simulated_track <- csv_einlesen('pathfinder-simulated-launch')
colnames(simulated_track) <- c("Seconds",
                               "SimulatedAltitude",
                               "SimulatedVerticalVelocity",
                               "SimulatedVerticalAcceleration")

simulated_track$Seconds_Rounded <- round(simulated_track$Seconds,4) + 1.2
track$Seconds_Rounded <- round(track$Seconds,2)
comparison <- inner_join(simulated_track, track, by = "Seconds_Rounded")
