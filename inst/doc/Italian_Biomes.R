## ----knitr-setup, include = FALSE---------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----initialize, message = FALSE----------------------------------------------
library("climenv")
library("terra")

# Lets begin by loading the data

data("it_py")
data("it_pt")

## ----italian-polygons-map, fig.height = 5, fig.width = 5, fig.align = "center", fig.cap = "**Fig 1**. Mediterranean and Nemoral Biomes of Italy (Mucina et al., 2023). The Mediterranean Biome is shaded green, while the Nemoral Biome is shaded in orange."----
# Make a map of the Italian polygons data set
plot(it_py, key.pos = NULL, border = NA, main = "")

## ----italian-points-map, fig.height = 5, fig.width = 5, fig.align = "center", fig.cap = "**Fig 2**. Mediterranean and Nemoral Biomes of Italy (Mucina et al., 2023). The Mediterranean Biome is coded as green points, while the Nemoral Biome is coded as orange points."----
# Make a map of the Italian points data set
plot(it_pt, pch = 19, cex = 0.5, key.pos = NULL, main = "")

## ----extract-data, message = FALSE, eval = FALSE------------------------------
#  # Do not execute this code if you are in a hurry as it will take time depending
#  # on the strength and speed of your internet connection.
#  
#  # This part downloads the data and takes time
#  ce_download(
#    "C/example_output..",
#    location = it_py
#  )
#  
#  # Once the data is downloaded, this function is quick.
#  it_data <- ce_extract(
#    "C/example_output..",
#    location = it_py,
#    location_g = "GB"
#  )

## ----load-data, message = FALSE-----------------------------------------------
# Fortunately you can avoid the above steps by simply loading the data
data("it_data")

## ----holdridge-diagram, fig.show = 'hold', fig.height = 5, fig.width = 5, fig.align = "center", fig.cap = "**Fig 3**. Position of the Mediterranean (MED) and Nemoral (NEM) Biomes within Italy derived using WorldClim climate within Holdridge's (1967) life zone classification. The surface shading in the background is new addition to the original life zone classification and helps interpretation by converting a point in evapotranspiration-precipitation space to an appropriate cross-blended hypsometric colour – in this intuitive instance colours tending towards the red spectrum feature higher temperatures blended with lower precipitation compared while colours tending towards the blue colour spectrum have lower temperatures and higher precipitation."----
# Make Holdridge's (1967) life zone classification diagram
plot_h(data = it_data, "MED")
plot_h(data = it_data, "NEM")

## ----walter-leigh-diagram, fig.show = 'hold', fig.height = 5, fig.width = 5, fig.align = "center", fig.cap = "**Fig 4**. Walter-Lieth’s climatic diagram (1960) of the Mediterranean (MED) and Nemoral (NEM) Biome within Italy derived using WorldClim. When precipitation is > 100 mm, the scale increases from 2mm C-1 to 20 mm  C-1 (as indicated by the black horizontal line) to avoid too high diagrams in very wet locations. This change is indicated by a black horizontal line, and the graph over is filled in solid blue. When the precipitation graph lies under the temperature graph (P < 2T) we have an arid period (filled in dotted red vertical lines). Otherwise the period is considered humid (filled in light blue). Daily maximum average temperature of the hottest month and daily minimum average temperature of the coldest month are labeled in black on the left margin of the diagram."----
# Make Walter-Leigh's (1960) climate diagram
plot_wl(data = it_data, "MED")
plot_wl(data = it_data, "NEM")

## ----custom-climate-diagram, fig.show = 'hold', fig.height = 4, fig.width = 7, fig.align = "center", fig.cap = "**Fig 5**. Custom diagram showing the climatic envelope of the Italian Mediterranean (MED) and Nemoral (NEM) Biomes. The abbreviations used are as follow: biotemperture (BioT), isothermality (ISO), mean annual temperature (MAT), temperature seasonality (TS), number of dry months with < 50 mm rainfall during the month (Dry mo), mean annual precipitation (MAP), potential evapotranspiration (PET), precipitation seasonality (PS), seasonal rainfall percentage in Summer (S), Autumn (A), Winter (W), Vernal (V), elevation (Elv) and latitude (Lat)."----
# Make the custom climate diagram
oldpar <- par(mar = c(1.5, 2.8, 2, 17))
plot_c(data = it_data, "MED")
plot_c(data = it_data, "NEM")
par(oldpar)

## ----extract-extended-data, message = FALSE, eval = FALSE---------------------
#  # Lets start by extracting the climate data for all points in the
#  # italy_pt data.
#  
#  it_data_extended <- ce_extract(
#    path = "C:/Users/jamie/OneDrive/Desktop/temp",
#    location = it_pt
#  )

## ----load-extended-data, message = FALSE--------------------------------------
# If you haven't downloaded the climate and elevation data by following the code
# above you could simply load the data()

data("it_data_extended")

# But this data needs to be reformatted a bit

# Lets start by calculating the bioclim variables

bioclim <- data.frame(dismo::biovars(
  prec = as.matrix(it_data_extended$prec[, 1:12]),
  tmin = as.matrix(it_data_extended$tmin[, 1:12]),
  tmax = as.matrix(it_data_extended$tmax[, 1:12])
))

# Lets make some more intuitive names for these variables
colnames(bioclim) <- c(
  "Mean_annual_temperature",
  "Mean_diurnal_range",
  "Isothermality",
  "Temperature_seasonality",
  "Max_temperature_of_warmest_month",
  "Min_temperature_of_coldest_month",
  "Temperature_annual_range",
  "Mean_temperature_of_the_wettest_quarter",
  "Mean_temperature_of_driest_quarter",
  "Mean_temperature_of_warmest_quarter",
  "Mean_temperature_of_coldest_quarter",
  "Total_annual_precipitation",
  "Precipitation_of_wettest_month",
  "Precipitation_of_driest_month",
  "Precipitation_seasonality",
  "Precipitation_of_wettest_quarter",
  "Precipitation_of_driest_quarter",
  "Precipitation_of_warmest_quarter",
  "Precipitation_of_coldest_quarter"
)

# Now get the Holdridge variables

hold <- bioclimate(
  as.matrix(it_data_extended$tavg[, 1:12]),
  as.matrix(it_data_extended$prec[, 1:12])
)

# Make some more intuitive names for these variables
colnames(hold)[1:3] <- c(
  "Mean_Annual_Biotemperature",
  "Total_Annual_Precipitation",
  "Potential_Evapotranspiration_Ratio"
)

# Join them together along with elevation and altitude

dat <- cbind(
  bioclim, hold[, c("Mean_Annual_Biotemperature",
                    "Potential_Evapotranspiration_Ratio")],
  elev = it_data_extended$elev,
  lat = it_data_extended$lat
)

# Lets just fix the last two column names
colnames(dat)[c(22, 23)] <- c("Elevation", "Latitude")

# Now lets add the Biome type as a column

dat$class <- as.factor(it_pt$GB)


## ----random-forests, message = FALSE------------------------------------------
# Now we are about ready to start the random forests analyses

# Load the required package
library("randomForest")

# Now we need to split the dataset into training and testing sets
set.seed(123)  # For reproducibility

train_indices <- sample(
  seq_len(dim(dat)[1]), dim(dat)[1] * 0.8
)  # 80% for training
train_data <- dat[train_indices, ]
test_data <- dat[-train_indices, ]

# Train the Random Forest model
model <- randomForest(formula = class ~ ., data = train_data)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data)

# Evaluate the model's performance
confusion_matrix <- table(predictions, test_data$class)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
print(paste("Accuracy:", accuracy))

## ----importance-measures, message = FALSE, fig.height = 5, fig.width = 7, fig.align = "center", fig.cap = "**Fig 6**. Gini Coefficient Plot illustrating the variable importance measures derived from a Random Forest model."----
# Get variable importance measures
importance <- importance(model)

# Print the importance measures
print(importance)

# Plot variable importance
oldpar <- par(mar = c(4, 3, 0, 1) + 0.1)
varImpPlot(model, main = "")
par(oldpar)

