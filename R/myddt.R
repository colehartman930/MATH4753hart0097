#' Title DDT Curve Maker
#'
#' @param df The Data Frame you are using
#' @param species What species of fish you are interested in making data of
#'
#' @return A plot of weight vs length for a specific species, as well as this information being output in a csv file. In the command line you can also see the data used
#' @export
#'
#' @examples
#' \dontrun{myddt(ddt, species = "CCATFISH")}
myddt = function(df, species){

  library(ggplot2)
  library(dplyr)

  ddt2 <- ddt[ddt$SPECIES == species,]  #Subsets our data frame so we can work with the correct species.

  fishcol=with(ddt2,ifelse(RIVER=="FCM","Red", ifelse(RIVER=="LCM","Blue", ifelse(RIVER=="TRM", "Purple", "Green"))))  # Colors the river something unique for each instance.

  write.csv(ddt2, paste0("LvsWfor", species, ".csv")) # Creates our csv output file

  g <- ggplot(ddt2, aes_string(x = ddt2$LENGTH, y = ddt2$WEIGHT)) # Creates our plot and gives values to the x and y axis

  g = g + geom_point(x = ddt2$LENGTH, y = ddt2$WEIGHT) # Adds points

  g = g + geom_point(aes(fill = RIVER), color = fishcol) # Colors our points in accordance to which river they were found in and makes our legend

  g = g + geom_smooth(formula = y~x +I(x^2), method = "lm") # Adds a curve to the plot

  g = g + labs(title = paste0("COLE HARTMAN PLOT FOR ", species), x = "LENGTH", y = "WEIGHT") #Adds labels to our plot
  print(g) #Prints the plot
  a <- list(data_frame = df, subset_dataframe = ddt2) #Lists all the data, as well as a subsetted list for the species we are interested in.
  print(a)
  table(ddt$RIVER) / length(ddt$RIVER) #Gives a relative frequency table for the river data.
}
