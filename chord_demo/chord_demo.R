

#### Exploring conversion with a chord diagram ####
# approach typically used to look at migration
# more informative than a grouped bar chart for example as you can look at 'per place' (e.g., acre or county) 'replacement'
# confuses some people so make sure to include text information
# this data is for broad ecosystem types within the Great Lakes watershed boundary


# load packages ------
# we will first install pacman, then use it to install and load packages

install.packages('pacman')
library(pacman)

p_load(chorddiag, htmlwidgets, tidyverse)



# read in and wrangle data ------
# read in data
chord_df <- read_csv("chord_demo/bps2evt_chord.csv")
#view(histFireGVchord)

#convert data to matrix
matrix_df <- as.matrix(as_adjacency_matrix(as_tbl_graph(chord_df),
                                           attr = "ACRES"))

#clean up matrix to remove empty rows.  Specifics will change depending on inputs.
matrix_df <- matrix_df[-c(8:16), -c(1:7)]

#make a custom color pallet ------

groupColors <-c( "#1d4220", # conifer 
                 "#fc9d03", # grassland
                 "#56bf5f", # hardwood
                 "#397d3f", # hardwood-conifer 
                 "#7db7c7", # riparian 
                 "#f57fdf", # savannah
                 "#6e4f1e", # shrubland
                 "#f5e942", # cur ag
                 "#1d4220", # cur conifer
                 "#397d3f", # cur hdw-con
                 "#b0af9e", # developed
                 "#eb4034", # exotics
                 "#fc9d03", # grassland
                 "#56bf5f", # hardwood
                 "#7db7c7",
                 "#6e4f1e"# shrubland
)



# make chord diagram ------
chord<-chorddiag(data = matrix_df,
                 type = "bipartite",
                 groupColors = groupColors,
                 groupnamePadding = 10,
                 groupPadding = 3,
                 groupnameFontsize = 12 ,
                 showTicks = FALSE,
                 margin=130,
                 tooltipGroupConnector = "    &#x25B6;    ",
                 chordedgeColor = "#363533"
)
chord 

#save then print to have white background
htmlwidgets::saveWidget(chord,
                        "chord.html",
                        background = "white",
                        selfcontained = TRUE
)