# clear all
rm(list=ls())

# set directory
setwd("/Users/guylu/Downloads/")

# all libs:
library(ggplot2)
library(plotly)
library("tidyverse")
library(stringr)
library(dplyr)
library(reshape2)
library(gplots)
library(gridGraphics)
library(grid)
library(gplots)
library(pheatmap)
library(gridExtra)

#                                            abit of preprocessing:


#read data:
data <- read.csv("MyExpt_IdentifySecondaryObjectsBlue.csv")
# add id column
data <- tibble::rowid_to_column(data, "id")
# change values for readability 1 - sick, 2- healthy
data$ImageNumber <- sub(1, "AD", data$ImageNumber)
data$ImageNumber <- sub(2, "WT", data$ImageNumber)

# change column names for markers in image to gene names
names(data)[names(data) == 'Children_IdentifyPrimaryObjectsCyon_Count'] <- 'cyan_Mfge8'
names(data)[names(data) == 'Children_IdentifyPrimaryObjectsGreen_Count'] <- 'green_Slc38a1'
names(data)[names(data) == 'Children_IdentifyPrimaryObjectsRed_Count'] <- 'red_Myoc'

#                                            Graph One:


data$sum = data$cyan_Mfge8+data$green_Slc38a1+data$red_Myoc
temp<-data[data$sum > 2,]

# split table for sick samples and healthy ones
data1<-temp[temp$ImageNumber=="AD",]
data2<-temp[temp$ImageNumber=="WT",]
# nice palatte:
my_palette<-colorRampPalette(c("white","blue","green","yellow","red"))(n = 367)

# adjust the data as matrux for heatmap
s1 = as.matrix(data1[,c("cyan_Mfge8","green_Slc38a1","red_Myoc")])
s2 = as.matrix(data2[,c("cyan_Mfge8","green_Slc38a1","red_Myoc")])

# creating heat map for sick and one for healthy


a <- list(pheatmap(s1,
                   main = "HeatMap of Gene Expression of\n Proteins that are Related to Alzheimer:\nAlzheimer Brain", # heat map title
                   notecol="black",      # change font color of cell labels to black
                   #density.info="none",  # turns off density plot inside color legend
                   trace="none",         # turns off trace lines inside the heat map
                   margins =c(14,2),     # widens margins around plot
                   col=my_palette,       # use on color palette defined earlier
                   #breaks=col_breaks,    # enable color transition at specified limits
                   dendrogram="row",     # only draw a row dendrogram
                   cluster_cols = FALSE,
                   show_rownames = FALSE,
                   Colv="NA")            # turn off column clustering
          [[4]])

a[[2]] <- pheatmap(s2,
                   main = "HeatMap of Gene Expression of\n Proteins that are Related to Alzheimer:\nHealthy Brain", # heat map title
                   notecol="black",      # change font color of cell labels to black
                   #density.info="none",  # turns off density plot inside color legend
                   trace="none",         # turns off trace lines inside the heat map
                   margins =c(14,2),     # widens margins around plot
                   col=my_palette,       # use on color palette defined earlier
                   #breaks=col_breaks,    # enable color transition at specified limits
                   dendrogram="row",     # only draw a row dendrogram
                   cluster_cols = FALSE,
                   show_rownames = FALSE,
                   Colv="NA")[[4]]            # turn off column clustering
        

merged_heatmaps <- grid.arrange(arrangeGrob(grobs= a,nrow=1))
plot(merged_heatmaps)


#                                            Graph Two:


# meltd data to tranfroms it, thusly it will be easier to graph later
melted_data<-melt(data = data, id.vars = c("id","ImageNumber"), measure.vars = c("cyan_Mfge8","green_Slc38a1","red_Myoc"))
# adding column for graph
melted_data$new <- paste(melted_data$variable,melted_data$ImageNumber)


#title="Expression in Cells")
#type = "log")

# creating the plotly graph

jittered_vals<-jitter(as.numeric(melted_data$value), factor=1, amount = NULL)

p2<-melted_data %>%
  group_by(ImageNumber) %>%
  plot_ly(x=~new ,y=jittered_vals,color = ~ImageNumber,text = ~paste(" Id:",id,"<br>",
                                                                           "State:",ImageNumber,"<br>",
                                                                           "Value:",value,"<br>"),
          hoverinfo="text",
          boxpoints = "all",pointpos = 0,jitter=0.7,marker = list(size = 4))%>%
  add_boxplot() %>%
  layout(xaxis= list(title="Genes + WT/FAD"),yaxis= list(title="Expression in Cells"),title="Distribution of Proteins in WT/AD Tissue")
  
p2
htmlwidgets::saveWidget(p2, "density_unique_lengths.html")


