
copied_dataset <- Solar_flare_RHESSI_2004_05

# Update the 'month' column for rows where 'year' is 2005
copied_dataset$month[ copied_dataset$year == 2005 ] <- copied_dataset$month[ copied_dataset$year == 2005 ] + 12

#Task 1 

#Method 1

method_one <- function(dataset) {
  # Calculate the intensity based on the "total.counts" attribute
  intensity <- scale(dataset$total.counts)
  dataset$intensity_one <- intensity 
  
  return(dataset)
}

# Call the function to calculate and add "intensity_one" to the dataset
copied_dataset <- method_one(copied_dataset)


#Method 2
method_two <- function(dataset) {
  # Define energy band weights (these can be customized)
  energy_band_weights <- c(
    '6-12' = 1,
    '12-25' = 2,
    '25-50' = 3,
    '50-100' = 4,
    '100-300' = 5,
    '300-800' = 6,
    '800-7000' = 7,
    '7000-20000' = 8
  )
  
  # Calculate the intensity for each row based on energy.kev and total.counts
  intensity <- dataset$total.counts * energy_band_weights[as.character(dataset$energy.kev)]
  
  # Add the calculated intensities as a new column called "intensity_two" to the dataset
  dataset$intensity_two <- intensity
  
  return(dataset)
}

# Call the function to calculate and add "intensity_two" to the dataset
copied_dataset <- method_two(copied_dataset)

#Creating Batches 

# Define the total number of months and batch size
total_months <- 24
batch_size <- 4
overlap <- 2
num_batches <- 11

# Initialize an empty list to store the batches
batches <- list()

# Loop to create batches
for (i in 1:num_batches) {
  start_month <- 1 + (i - 1) * (batch_size - overlap)
  end_month <- start_month + batch_size - 1
  batch <- copied_dataset[copied_dataset$month >= start_month & copied_dataset$month <= end_month, ]
  batches[[i]] <- batch
}

#create intensity maps for batch 1 w/ method 1 & 2

library(ggplot2)


sample_data_batch_one <- data.frame(batches[[1]])

# Create an intensity map based on intensity one (Method One)
ggplot(sample_data_batch_one, aes(x = x.pos.asec, y = y.pos.asec, fill = intensity_one, size = intensity_one)) +
  geom_point(shape = 21) +
  scale_fill_gradient(low = "blue", high = "red") +
  scale_size_continuous(range = c(0.5, 10)) +
  labs(title = "Intensity Map (Method 1 - Batch 1)", x = "X Position (arcseconds)", y = "Y Position (arcseconds)") +
  theme_minimal()



# Create an intensity map based on intensity two (Method Two)
ggplot(sample_data_batch_one, aes(x = x.pos.asec, y = y.pos.asec, fill = intensity_two, size = intensity_two)) +
  geom_point(shape = 21) +  # Adjust alpha as needed
  scale_fill_gradient(low = "blue", high = "red") +
  scale_size_continuous(range = c(0.5, 10)) +  # Adjust the point size range
  labs(title = "Intensity Map (Method 2 - Batch 1)", x = "X Position (arcseconds)", y = "Y Position (arcseconds)") +
  theme_minimal()


sample_data_batch_11 <- data.frame(batches[[11]])


ggplot(sample_data_batch_11, aes(x = x.pos.asec, y = y.pos.asec, fill = intensity_one, size = intensity_one)) +
  geom_point(shape = 21) +  # Adjust alpha as needed
  scale_fill_gradient(low = "blue", high = "red") +
  scale_size_continuous(range = c(0.2, 8)) +  # Adjust the point size range
  labs(title = "Intensity Map (Method 1 - Batch 11)", x = "X Position (arcseconds)", y = "Y Position (arcseconds)") +
  theme_minimal()


# Create an intensity map based on intensity two (Method Two)
ggplot(sample_data_batch_11, aes(x = x.pos.asec, y = y.pos.asec, fill = intensity_two, size = intensity_two)) +
  geom_point(shape = 21) +  # Adjust alpha as needed
  scale_fill_gradient(low = "blue", high = "red") +
  scale_size_continuous(range = c(0.5, 10)) +  # Adjust the point size range
  labs(title = "Intensity Map (Method 2 - Batch 11)", x = "X Position (arcseconds)", y = "Y Position (arcseconds)") +
  theme_minimal()

# Task 2: Hotspot discovery (Subtask2a)
library(raster)
library(ggplot2)

# Function to discover hotspots based on intensity thresholds
discover_hotspots <- function(x, y, intensity, threshold) {
  # Create a binary vector indicating if values are above the threshold
  binary_vector <- intensity > threshold
  
  # Create a data frame with x, y, and intensity columns
  data <- data.frame(x = x, y = y, intensity = intensity)
  
  # Filter data above threshold
  data_above_threshold <- data[binary_vector, ]
  
  # Convert data frame to SpatialPointsDataFrame
  coordinates(data_above_threshold) <- c("x", "y")
  proj4string(data_above_threshold) <- CRS(as.character(NA))  # Set projection information
  
  return(data_above_threshold)
}

# Subtask2b determineing the intensity threashold d1 and d2 

d1_threshold <- 0.85
d2_threshold <- 0.5

# subtask 2c

# Discover hotspots for d1 (adjust the threshold value)
hotspots_sample <- discover_hotspots(sample_data_batch_one$x.pos.asec, sample_data_batch_one$y.pos.asec, 
                                        sample_data_batch_one$intensity_one, d1_threshold)

hotspots_df <- as.data.frame(hotspots_sample)

ggplot() +
  geom_density2d_filled(data = hotspots_df, aes(x = x, y = y), alpha = 0.75) +
  labs(title = "Hotspots Density Plot with Intensity", x = "X Position (arcseconds)", y = "Y Position (arcseconds)") +
  scale_fill_viridis_d(option = "turbo")

#subtask 2d

# Create an empty list to store the hotspot polygons
hotspot_time_series_d1 <- list()

# Loop through the batches and discover hotspots for each batch
for (i in 1:11) {
  current_batch <- batches[[i]]  # Access the batch from the 'batches' list
  
  # Discover hotspots for d1
  hotspots <- discover_hotspots(current_batch$x.pos.asec, current_batch$y.pos.asec,
                                current_batch$intensity_one, d1_threshold)
  
  # Store the discovered hotspots for the current batch
  hotspot_time_series_d1[[i]] <- hotspots
}


# Loop over different batches to create density plots
for (i in 1:length(hotspot_time_series_d1)) {
  plot <- ggplot() +
    geom_density2d_filled(data = as.data.frame(hotspot_time_series_d1[[i]]), aes(x = x, y = y), alpha = 0.75) +
    labs(title = paste("Hotspots Density Plot D1 - Batch", i), x = "X Position (arcseconds)", y = "Y Position (arcseconds)") +
    scale_fill_viridis_d(option = "turbo")
  # You might want to print or save each plot here
  
  filename <- paste("Hotspot_Density_Plot_D1_Batch", i, ".png", sep = "")
  ggsave(filename, plot)
}

#subtask 2e

# Create an empty list to store the hotspot polygons
hotspot_time_series_d2 <- list()

# Loop through the batches and discover hotspots for each batch
for (i in 1:11) {
  current_batch <- batches[[i]]  # Access the batch from the 'batches' list
  
  # Discover hotspots for d1
  hotspots <- discover_hotspots(current_batch$x.pos.asec, current_batch$y.pos.asec,
                                current_batch$intensity_one, d2_threshold)
  
  # Store the discovered hotspots for the current batch
  hotspot_time_series_d2[[i]] <- hotspots
}


# Loop over different batches to create density plots
for (i in 1:length(hotspot_time_series_d2)) {
  plot <- ggplot() +
    geom_density2d_filled(data = as.data.frame(hotspot_time_series_d2[[i]]), aes(x = x, y = y), alpha = 0.75) +
    labs(title = paste("Hotspots Density Plot D2 - Batch", i), x = "X Position (arcseconds)", y = "Y Position (arcseconds)") +
    scale_fill_viridis_d(option = "turbo")
  # You might want to print or save each plot here
  
  filename <- paste("Hotspot_Density_Plot_D2_Batch", i, ".png", sep = "")
  ggsave(filename, plot)
}

#Task 3

Solar_flare_RHESSI_2015_16$month[Solar_flare_RHESSI_2015_16$year == 2016] <- Solar_flare_RHESSI_2015_16$month[Solar_flare_RHESSI_2015_16$year == 2016] + 12

# Call the function to calculate and add "intensity_one" to the dataset
Solar_flare_RHESSI_2015_16 <- method_one(Solar_flare_RHESSI_2015_16)

# Call the function to calculate and add "intensity_two" to the dataset
Solar_flare_RHESSI_2015_16 <- method_two(Solar_flare_RHESSI_2015_16)


# Initialize an empty list to store the batches
batches_set_2 <- list()


total_months <- 24
batch_size <- 4
overlap <- 2
num_batches <- 11

# Loop to create batches
for (i in 1:num_batches) {
  start_month <- 1 + (i - 1) * (batch_size - overlap)
  end_month <- start_month + batch_size - 1
  batch_set_2 <- Solar_flare_RHESSI_2015_16[Solar_flare_RHESSI_2015_16$month >= start_month & Solar_flare_RHESSI_2015_16$month <= end_month, ]
  batches_set_2[[i]] <- batch_set_2
}


batch_one_set_two <- data.frame(batches_set_2[[1]])

# Create an intensity map based on intensity one (Method One)
ggplot(batch_one_set_two, aes(x = x.pos.asec, y = y.pos.asec, fill = intensity_one, size = intensity_one)) +
  geom_point(shape = 21) +
  scale_fill_gradient(low = "blue", high = "red") +
  scale_size_continuous(range = c(0.5, 10)) +
  labs(title = "Intensity Map (Method 1 - Batch 1 - Set 2)", x = "X Position (arcseconds)", y = "Y Position (arcseconds)") +
  theme_minimal()



# Create an intensity map based on intensity two (Method Two)
ggplot(batch_one_set_two, aes(x = x.pos.asec, y = y.pos.asec, fill = intensity_two, size = intensity_two)) +
  geom_point(shape = 21) +  # Adjust alpha as needed
  scale_fill_gradient(low = "blue", high = "red") +
  scale_size_continuous(range = c(0.5, 10)) +  # Adjust the point size range
  labs(title = "Intensity Map (Method 2 - Batch 1 - Set 2)", x = "X Position (arcseconds)", y = "Y Position (arcseconds)") +
  theme_minimal()


batch_11_set_two <- data.frame(batches_set_2[[11]])


ggplot(batch_11_set_two, aes(x = x.pos.asec, y = y.pos.asec, fill = intensity_one, size = intensity_one)) +
  geom_point(shape = 21) +  # Adjust alpha as needed
  scale_fill_gradient(low = "blue", high = "red") +
  scale_size_continuous(range = c(0.2, 8)) +  # Adjust the point size range
  labs(title = "Intensity Map (Method 1 - Batch 11 - Set 2)", x = "X Position (arcseconds)", y = "Y Position (arcseconds)") +
  theme_minimal()


# Create an intensity map based on intensity two (Method Two)
ggplot(batch_11_set_two, aes(x = x.pos.asec, y = y.pos.asec, fill = intensity_two, size = intensity_two)) +
  geom_point(shape = 21) +  # Adjust alpha as needed
  scale_fill_gradient(low = "blue", high = "red") +
  scale_size_continuous(range = c(0.5, 10)) +  # Adjust the point size range
  labs(title = "Intensity Map (Method 2 - Batch 11 - Set 2)", x = "X Position (arcseconds)", y = "Y Position (arcseconds)") +
  theme_minimal()



# Discover hotspots for d1 (adjust the threshold value)

# Create an empty list to store the hotspot polygons
hotspot_time_series_d1_s2 <- list()

# Loop through the batches and discover hotspots for each batch
for (i in 1:11) {
  current_batch <- batches_set_2[[i]]  # Access the batch from the 'batches' list
  
  # Discover hotspots for d1
  hotspots <- discover_hotspots(current_batch$x.pos.asec, current_batch$y.pos.asec,
                                current_batch$intensity_one, d1_threshold)
  
  # Store the discovered hotspots for the current batch
  hotspot_time_series_d1_s2[[i]] <- hotspots
}


# Loop over different batches to create density plots
for (i in 1:length(hotspot_time_series_d1_s2)) {
  plot <- ggplot() +
    geom_density2d_filled(data = as.data.frame(hotspot_time_series_d1_s2[[i]]), aes(x = x, y = y), alpha = 0.75) +
    labs(title = paste("Hotspots Density Plot D1 S2 - Batch", i), x = "X Position (arcseconds)", y = "Y Position (arcseconds)") +
    scale_fill_viridis_d(option = "turbo")
  # You might want to print or save each plot here
  
  filename <- paste("Hotspot_Density_Plot_D1_S2_Batch", i, ".png", sep = "")
  ggsave(filename, plot)
}


# Create an empty list to store the hotspot polygons
hotspot_time_series_d2_s2 <- list()

# Loop through the batches and discover hotspots for each batch
for (i in 1:11) {
  current_batch <- batches_set_2[[i]]  # Access the batch from the 'batches' list
  
  # Discover hotspots for d1
  hotspots <- discover_hotspots(current_batch$x.pos.asec, current_batch$y.pos.asec,
                                current_batch$intensity_one, d2_threshold)
  
  # Store the discovered hotspots for the current batch
  hotspot_time_series_d2_s2[[i]] <- hotspots
}


# Loop over different batches to create density plots
for (i in 1:length(hotspot_time_series_d2_s2)) {
  plot <- ggplot() +
    geom_density2d_filled(data = as.data.frame(hotspot_time_series_d2_s2[[i]]), aes(x = x, y = y), alpha = 0.75) +
    labs(title = paste("Hotspots Density Plot D2 S2 - Batch", i), x = "X Position (arcseconds)", y = "Y Position (arcseconds)") +
    scale_fill_viridis_d(option = "turbo")
  # You might want to print or save each plot here
  
  filename <- paste("Hotspot_Density_Plot_D2_S2_Batch", i, ".png", sep = "")
  ggsave(filename, plot)
}


#Basic statistic calculation 

# List to store basic statistics for each batch in batches_set_2
basic_stats_S2_intensity1 <- list()

for (i in 1:length(batches_set_2)) {
  current_batch <- batches_set_2[[i]]$intensity_one  # Access the 'intensity_one' column
  stats <- list(
    mean = round(mean(current_batch, na.rm = TRUE), 5),
    median = round(median(current_batch, na.rm = TRUE), 5),
    sd = round(sd(current_batch, na.rm = TRUE), 5)
  )
  # Calculate mode using a custom function
  get_mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  stats$mode <- round(get_mode(current_batch), 5)
  basic_stats_S2_intensity1[[i]] <- stats  # Store stats for each batch
}


# Convert list to data frame
basic_stats_S2_intensity1 <- as.data.frame(do.call(rbind, basic_stats_S2_intensity1))

# Rename the columns
colnames(basic_stats_S2_intensity1) <- c("mean", "median", "mode", "sd_of_mean")





# List to store basic statistics for each batch in batches_set_2
basic_stats_S2_intensity2 <- list()

for (i in 1:length(batches_set_2)) {
  current_batch <- batches_set_2[[i]]$intensity_two  # Access the 'intensity_two' column
  stats <- list(
    mean = mean(current_batch, na.rm = TRUE),
    median = median(current_batch, na.rm = TRUE),
    sd = sd(current_batch, na.rm = TRUE)
  )
  # Calculate mode using a custom function
  get_mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  stats$mode <- get_mode(current_batch)
  basic_stats_S2_intensity2[[i]] <- stats  # Store stats for each batch
}

# Convert list to data frame
basic_stats_S2_intensity2 <- as.data.frame(do.call(rbind, basic_stats_S2_intensity2))

# Rename the columns
colnames(basic_stats_S2_intensity2) <- c("mean", "median", "mode", "sd_of_mean")



# List to store basic statistics for each batch in batches_set_1
basic_stats_S1_intensity1 <- list()

for (i in 1:length(batches)) {
  current_batch <- batches[[i]]$intensity_one  # Access the 'intensity_one' column
  stats <- list(
    mean = round(mean(current_batch, na.rm = TRUE), 5),
    median = round(median(current_batch, na.rm = TRUE), 5),
    sd = round(sd(current_batch, na.rm = TRUE), 5)
  )
  # Calculate mode using a custom function
  get_mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  stats$mode <- round(get_mode(current_batch), 5)
  basic_stats_S1_intensity1[[i]] <- stats  # Store stats for each batch
}


# Convert list to data frame
basic_stats_S1_intensity1 <- as.data.frame(do.call(rbind, basic_stats_S1_intensity1))

# Rename the columns
colnames(basic_stats_S1_intensity1) <- c("mean", "median", "mode", "sd_of_mean")




# List to store basic statistics for each batch in batches_set_2
basic_stats_S1_intensity2 <- list()

for (i in 1:length(batches)) {
  current_batch <- batches[[i]]$intensity_two  # Access the 'intensity_two' column
  stats <- list(
    mean = mean(current_batch, na.rm = TRUE),
    median = median(current_batch, na.rm = TRUE),
    sd = sd(current_batch, na.rm = TRUE)
  )
  # Calculate mode using a custom function
  get_mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  stats$mode <- get_mode(current_batch)
  basic_stats_S1_intensity2[[i]] <- stats  # Store stats for each batch
}

# Convert list to data frame
basic_stats_S1_intensity2 <- as.data.frame(do.call(rbind, basic_stats_S1_intensity2))

# Rename the columns
colnames(basic_stats_S1_intensity2) <- c("mean", "median", "mode", "sd_of_mean")


