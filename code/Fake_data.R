
#Beetle dataset creation, plotting and saving demo
# Michelle Moczulski
#January 23, 2023
#Base R

#create dataset

#set a seed for reproducibility
set.seed(123)

#set seed is a random number generator. when you use runif or sample, the numbers are based on set seed. it ensures that the random numbers are similar when you run functions in this script

#create vector of beetle names
beetle_names <- c("Ladybug", "Stag Beetle", "Firefly Beetle", "Dung Beetle", "Jewel Beetle")


#Create vector of beetle lengths
beetle_lengths <- runif(20, 1, 5) #Random length between 1 and 5

#the runiff is a random number generator from a uniform distribution. 'n' is numbers you want generated, min is the minumum number and max is the highest number

#Create vector of beetle colors
beetle_colors <- sample(c("Red", "Black", "Green", "Yellow", "Blue"), 20, replace = TRUE)

#sample generates random samples from a specific set of elements. 'x' is a vector specifying what samples to choose from. 'size' is the number of samples to generate. 'Replace' means if TRUE it's done with replacement and FALSE means sampling is done without replacements

#create dataframe
beetle_df <- data.frame(Name = sample(beetle_names, 20, replace = TRUE), Length = beetle_lengths, Color = beetle_colors)

#Data frame function means XXX

#save this data to directory
write.csv(beetle_df, file = "data/fake_data.csv")

#write.csv means Common seperated values. 'dataframe' is the name of the frame, 'file' is the file connection, 'row.names' is if you want the row names included (if not, it's FALSE)

#MAKE a quick chart

length_chart <- barplot(height = beetle_df$Length, 
                        names = beetle_df$Name)

#Used to create bar charts and graphs. height is the values representing the height of the bar, 'names.arg' is the name for the bar labels, 'besides' is the value the bars should be put in (TRUE is default), col is the color of the bars and main is the main title of the plot
