library(FactoMineR)
library(ggplot2)
library(factoextra)
library(magrittr)
library(dplyr)

mca_analysis <- function(data, plots = T){
  mca_data <- data[,c("disengaged","looking","talking","technology","resources","external")]
  
  #Turnining the columns into factors
  mca_data$disengaged <- factor(mca_data$disengaged)
  mca_data$looking <- factor(mca_data$looking)
  mca_data$talking <- factor(mca_data$talking)
  mca_data$technology <- factor(mca_data$technology)
  mca_data$resources <- factor(mca_data$resources)
  mca_data$external <- factor(mca_data$external)
  
  mca1 <- MCA(mca_data, graph = F)
  
  #Gives various plots if desired
  if(plots){
    print(fviz_screeplot(mca1, addlabels = TRUE, ylim = c(0, 45)))
  
    print(fviz_mca_var(mca1, col.var="orange", shape.var = 15, repel = TRUE))
  
    print(fviz_mca_var(mca1, choice = "mca.cor", 
                  repel = F, # Avoid text overlapping (slow)
                  ggtheme = theme_minimal()))
  
    factors <- c("disengaged","looking","talking","technology","resources","external")
  
    for (column in factors) {
      print(fviz_ellipses(mca1, column, geom = "point"))
    }
  }
  
  mca1
}

add_mca_dimensions <- function(dataframe, mca = NULL, dimensions = 3){
  
  #MCA can be passed manually if one wants to inspect the MCA output first. Otherwise created here.
  if(is.null(mca)){mca <- mca_analysis(dataframe, plots = F)}
  
  #Gets the extra columns from the MCA object and merges with original data
  extra_columns <- mca$ind$coord[,1:dimensions]
  dataframe <- data.frame(dataframe, extra_columns)
  
  for(i in (ncol(dataframe)-dimensions+1):ncol(dataframe)){
    names(dataframe)[i] <- paste(c("MCAdim", (i+dimensions-ncol(dataframe))), collapse = "")
  }
  
  dataframe
}


### ALL OF THE REMAINING FUNCTIONS TAKE IN DATA FRAMES WITH MCA DIMENSIONS ADDED ###

create_dimension_histograms <- function(dataframe, dimensions = 3){
  for(i in 1:dimensions){
    column <- paste(c("MCAdim",i), collapse = "")
    hist(dataframe[,column], xlab = column, main = paste(c("Histogram of MCA dimension ", i)))
  }
}


create_date_histograms <- function(dataframe, dimension = 1){
  dataframe$date <- factor(dataframe$date)
  column <- paste(c("MCAdim",dimension), collapse = "")
  
  for(fact in levels(dataframe$date)){
    title <- paste(c("Histogram of MCA dimension ", dimension, " on ", fact), collapse = "")
    hist(dataframe[dataframe$date == fact, column], xlab = column,
               main = title)
  }
}

#Used by violin plot function
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  
  c(y=m,ymin=ymin,ymax=ymax)
}


create_violin_plot <- function(dataframe, dimension = 1){
  #Preprocesses the data first
  dataframe$date <- factor(dataframe$date)
  
  column <- paste(c("MCAdim",dimension), collapse = "")
  title <- paste(c("MCA dimension ", dimension, " violin plot for all days"), collapse = "")
  
  dataframe <- dataframe[, c("date", column)]
  names(dataframe) <- c("date", "MCAdim")
  
  #Then creates and adapts the violin plot
  p <- ggplot(dataframe, aes(x = date, y = MCAdim)) + geom_violin(trim = F)
  p <- p + stat_summary(fun.data=data_summary)
  p <- p + coord_flip()
  p <- p + ggtitle(title)

  p
}

first_element <- function(data){
  data[1]
}

aggregate_by_groups <- function(dataframe){
  
  return_data <- dataframe %>%
    group_by(timestamp, group) %>%
    summarize(MCAdim1 = mean(MCAdim1, na.rm = TRUE)*4, MCAdim2 = mean(MCAdim2, na.rm = TRUE)*4,
              MCAdim3 = mean(MCAdim3, na.rm = TRUE)*4, activity = first_element(activity),
              observer = first_element(observer), project = first_element(project),
              date = first_element(date), comments = first_element(comments))
  
  
  return_data
}

