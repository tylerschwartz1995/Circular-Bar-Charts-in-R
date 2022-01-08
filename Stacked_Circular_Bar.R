

stacked_circular_bar = function(data){
  # library
  library(tidyverse)
  library(viridis)
  
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 0
  nObsType <- nlevels(as.factor(data$observation))
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$group <- rep(levels(data$group), each=empty_bar*nObsType )
  data <- rbind(data, to_add)
  #data <- data %>% arrange(group, individual)
  data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)
  
  # Get the name and the y position of each label
  label_data <- data %>% group_by(id, individual) %>% summarize(tot=sum(value))
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     
  # I substract 0.5 because the letter must have the angle of the center of the bars. 
  #Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  base_data <- data %>% 
    group_by(group) %>% 
    summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]
  
  # Make the plot
  p <- ggplot(data) +      
    
    # Add the stacked bar
    geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=0.5) +
    scale_fill_viridis(discrete=TRUE,name = "Band") +
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey",
                 alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey",
                 alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", 
                 alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey",
                 alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", 
                 alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    # Add text showing the value of each 100/75/50/25 lines
    #ggplot2::annotate("text", x = rep(max(data$id),5), y = c(0, 50, 100, 150, 200), 
    #label = c("0", "50", "100", "150", "200") , color="grey", size=6 , angle=0, 
    #fontface="bold", hjust=1) +
    
    ylim(-200,max(label_data$tot, na.rm=T)) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),legend.position = c(0.5, 0.5),
          plot.margin = unit(rep(-1,4), "cm"),
          legend.key.size = unit(0.75, "cm"),
          legend.text = element_text(colour="black", size = 11, face = "bold"),
          legend.title = element_text(face = "bold", size = 12)) +
    #scale_fill_discrete(name = "Band") +
    coord_polar() +
    
    # Add labels on top of each bar
    geom_text(data=label_data, aes(x=id, y=tot+10, label=individual, hjust=hjust), 
              color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
    
    # Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), 
                 colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
    geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,1,1,0,0,0),
              colour = "black", alpha=0.8, size=5, fontface="bold", inherit.aes = FALSE)
  
  p
}
