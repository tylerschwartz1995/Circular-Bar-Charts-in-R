

circular_bar_no_group = function(data){
  # library
  library(tidyverse)

  # Set a number of 'empty bar'
  empty_bar <- 0
  
  # Add lines to the initial dataset
  to_add <- matrix(NA, empty_bar, ncol(data))
  colnames(to_add) <- colnames(data)
  data <- rbind(data, to_add)
  data$id <- seq(1, nrow(data))
  
  # Get the name and the y position of each label
  label_data <- data
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  # Make the plot
  p <- ggplot(data, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    geom_bar(stat="identity", fill=alpha("turquoise2", 0.3)) +
    ylim(-70,120) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          plot.margin = unit(rep(-1,4), "cm")) +
    coord_polar(start = 0) + 
    geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), 
              color="black", fontface="bold",alpha=0.6, size=4.5, angle= label_data$angle, 
              inherit.aes = FALSE ) 
  
  p
}
