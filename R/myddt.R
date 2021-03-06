#' Title DDT Curve Maker
#'
#' @param df The Data Frame you are using
#' @param species What species of fish you are interested in making data of
#'
#' @return A plot of weight vs length for a specific species, as well as this information being output in a csv file. In the command line you can also see the data used
#' @export
#'
#' @examples
#' \dontrun{myddt(df = ddt, species = "CCATFISH")}
myddt = function(df, species){

  newdf <- df[df$SPECIES == species,]    #Subsets our data frame so we can work with the correct species.

  fishcol=with(newdf,ifelse(RIVER=="FCM","Red", ifelse(RIVER=="LCM","Blue", ifelse(RIVER=="TRM", "Purple", "Green"))))  # Colors the river something unique for each instance.

  write.csv(newdf, paste0("LvsWfor", species, ".csv")) # Creates our csv output file

  g <- ggplot(newdf, aes_string(x = newdf$WEIGHT, y = newdf$LENGTH)) # Creates our plot and gives values to the x and y axis

  g = g + geom_point(x = newdf$WEIGHT, y = newdf$LENGTH) # Adds points

  g = g + geom_point(aes(fill = RIVER), color = fishcol) # Colors our points in accordance to which river they were found in and makes our legend

  g = g + geom_smooth(formula = y~x +I(x^2), method = "lm") # Adds a curve to the plot

  g = g + labs(title = paste0("COLE HARTMAN PLOT FOR ", species), x = "WEIGHT", y = "LENGTH") #Adds labels to our plot
  print(g) #Prints the plot
  a <- list(data_frame = df, subset_dataframe = newdf) #Lists all the data, as well as a subsetted list for the species we are interested in.
  print(a)
  table(df$RIVER) / length(df$RIVER) #Gives a relative frequency table for the river data.
}

