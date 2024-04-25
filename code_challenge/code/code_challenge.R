## loading packages

library(readr)
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(janitor)
library(writexl)

## importing data

bps_aoi_attributes <- read_csv("code_challenge/code_challenge/input_data/bps_aoi_attributes.csv")
View(bps_aoi_attributes)

bps_model_number_name <- read_csv("code_challenge/code_challenge/input_data/bps_model_number_name.csv")
View(bps_model_number_name)

combine_raw <- read_csv("code_challenge/code_challenge/input_data/combine_raw.csv")
View(combine_raw)

LF16_BPS_200 <- read_csv("code_challenge/code_challenge/input_data/LF16_BPS_200.csv")
View(LF16_BPS_200)

ref_con_modified <- read_csv("code_challenge/code_challenge/input_data/ref_con_modified.csv")
View(ref_con_modified)

scls_aoi_attributes <- read_csv("code_challenge/code_challenge/input_data/scls_aoi_attributes.csv")
View(scls_aoi_attributes)
## Renaming column to model code

bps_aoi_attributes$Model_Code <- bps_aoi_attributes$BPS_MODEL ## I'm changing the column name in bps_aoi_attributes from BPS_MODEL to Model_Code

## merging datasets for past 

merged_df <- merge(bps_aoi_attributes, bps_model_number_name, by = "Model_Code") ##this line of code is subsetting the following into merged_df...merge() means that I'm combining 2 datasets into one. By = means that "Model_Code" is the common denominator between the 2 and it will be combined by that name

merged_df_all <- merge(merged_df, ref_con_modified, by = "Model_Code") %>% 
  subset(select= -c(B.x)) %>% 
  rename(B = B.y)

merged_df_all

## past data fixed column

past_long <- pivot_longer(merged_df_all, 
                          cols = c(A, B, C, D, E, Agriculture, Developed, Water, 
                                   UN, UE), 
                          names_to = "Past",
                          values_to = "Past_Value") ## here I am making a subset called past_long that uses the pivot_longer function to change the following columns (A, B, C, etc...) to be in one column and the following column will be the value associated with that category

## cleaning past data

past_cleaned <- past_long[, !(names(past_long) %in% c("ZONE", "GROUPVEG", "R", "G",
                                                      "B.x", "FRI_REPLAC", "FRI_MIXED", "FRI_SURFAC", "FRI_ALLFIR", "PRC_REPLAC", "PRC_MIXED", "PRC_SURFAC", "FRG_NEW", "RED", "GREEN", "BLUE", "BPS_MODEL", "BPS_CODE", "ACRES", "REL_PERCENT", "BPS_NAME", "VALUE", "Freq", "...1"))]

## filtering data to just be desired values

filtered_past <- past_cleaned %>% filter(Model_Code %in% c("13040_32_43_44_49", "13670_32_44", "15070_44")) %>%
  unite(model_label, c("Model_Code", "Past_Value")) #google how to not get rid of category

### present data

## making data long

ref_long <- pivot_longer(ref_con_modified, 
                         cols = c(A, B, C, D, E, Agriculture, Developed, Water, 
                                  UN, UE), 
                         names_to = "Category_Past",
                         values_to = "refPercent")

## combining labels to data

ref_long <- merge(bps_model_number_name, ref_long, by = "Model_Code")

## 

## 

combine <- left_join(combine_raw, 
                     scls_aoi_attributes %>%
                       dplyr::select(2, 4),  
                     by = c("Var2" = "VALUE"))

#bring in bps labels
combine <- left_join(combine, 
                     LF16_BPS_200 %>%
                       dplyr::select(1:4),
                     by = c("Var1" = "VALUE"))

combine_filtered <- combine %>% filter(BPS_MODEL %in% c("13040_32_43_44_49", "13670_32_44", "15070_44"))

combine_ <- combine_filtered %>%
  group_by(Var1, BPS_MODEL) %>%
  mutate(total_count = sum(Freq)) %>%
  mutate(currentPercent = as.integer((Freq/total_count)*100)) %>%
  unite(model_label, c("BPS_MODEL", "LABEL"))

## cleaning data

filtered_present <- combine_[, !(names(combine_) %in% c("ZONE", "Freq", "...1", "Var1", "Var2", "total_count"))]

## combining past and present

merged_data <- read_excel("code_challenge/code_challenge/input_data/merged_data.xlsx")
View(merged_data)


write.csv(merged_data, "merged_data.csv")

## seperating columns

seperated_data <- separate(merged_data, col=model_label, into = c('model_label', 'Category'), sep = '_')

## cleaning final dataset

final <- seperated_data[, !(names(seperated_data) %in% c("model_label", "BPS_Code"))]

final_data <- subset(final, select = c(BpS_Name, Category, Value_Past, currentPercent)) 

## renaming columns

colnames(final_data) <- c('BpS_Name','Category','Past', 'Present')

final_data_ <- pivot_longer(final_data, 
                            cols = c(Past, Present), 
                            names_to = "Time_Period",
                            values_to = "Percent")

### making the graph

final_graph <-
  ggplot(final_data_, aes(fill = factor(Time_Period), y = Percent, x = Category)) + 
  geom_col(width = 0.8, position = position_dodge()) +
  coord_flip() +
  facet_grid(. ~BpS) +
  scale_x_discrete(limits = (levels(final_data_$Category))) +
  labs(
    title = "Succession Classes past and present",
    subtitle = "Top BpSs selected for illustration. Not all succession classes present in all BpSs",
    caption = "Data from landfire.gov.",
    x = "",
    y = "Percent") +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(hjust = 0, face = "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot") +
  scale_fill_manual(values = c("#3d4740", "#32a852" ), # present (grey), historical (green)
                    name = " ", 
                    labels = c("Present",
                               "Past")) +
  facet_wrap(~BpS_Name, nrow(3),labeller = labeller(BpS_Name = label_wrap_gen())) +
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1))


### END OF DATA

### left join data

## look up tidyverse complete left join (past is on left and present is on the right by the model_label), bring in only certain columns (CurrentPresent). More work to format chart, use pivot_longer. Have 4 columns - name, label categories, current percent and past percent. 
  


## filtering present data



aoi_ref_cur <- left_join(aoi_ref_con,
                         combine) %>%
  drop_na(refPercent) %>%
  mutate(currentPercent = as.numeric(currentPercent),
         currentPercent = ifelse(is.na(currentPercent), 0, currentPercent))%>%
  mutate(total_count = as.numeric(total_count),
         total_count = ifelse(is.na(total_count), 0, total_count)) %>%
  select(-c(BPS_CODE, ZONE)) %>%
  select(c(Freq,
           Var1,
           Var2,
           BpS_Name,
           Model_Code,
           refLabel,
           model_label,
           refPercent,
           currentPercent,
           total_count)) %>%
  rename(count = Freq,
         bps_value = Var1,
         scl_value = Var2,
         bps_name = BpS_Name) %>%
  clean_names()


## present data

ref_con <- ref_con_modified %>%
  pivot_longer(!Model_Code, names_to = "refLabel", values_to = "refPercent") %>%
  unite(model_label, c("Model_Code", "refLabel"), remove = FALSE) %>%
  left_join(bps_model_number_name)






# filtering attributes to just Ozark-Ouachita Dry-Mesic Oak Forest, Ozark-Ouachita Shortleaf Pine-Bluestem Woodland, and Ozark-Ouachita Shortleaf Pine-Oak Forest and Woodland

Past_data <- merged_df_all %>%
  pivot_longer(!Model_Code, names_to = "refLabel", values_to = "refPercent") %>%
  unite(model_label, c("Model_Code", "refLabel"), remove = FALSE) %>%
  left_join(bps_model_number_name)

filtered_merged <- merged_df_all %>%filter(BPS_NAME == c("Ozark-Ouachita Shortleaf Pine-Bluestem Woodland", "Ozark-Ouachita Shortleaf Pine-Oak Forest and Woodland"))

filtered_merged_PineB <- merged_df_all %>%filter(Model_Code == c("15070_44"))

filtered_merged_PineO <- merged_df_all %>%filter(Model_Code == 
                                                   c("13670_32_44"))

Past_data <- pivot_longer(filtered_merged_PineO, cols = everything())

## making the plot

PineOak <- read_excel("~/Desktop/R Studio/code_challenge/data/PineOak.xlsx")
View(PineOak)

barplot(PineOak$Percentage, names.arg = PineOak$Category, col = "green")

ggplot(PineOak, aes(x = Percentage, y = Category, fill = Category)) +
  labs(title = "Bar Plot with Several Bars",
       x = "Category",
       y = "Percent")


## test plot

# Create the dataset
data <- data.frame(
  bps = rep(c("ecosystem A", "ecosystem B", "ecosystem C"), each = 10),
  label = rep(c("A", "B", "C", "D", "E", "Agriculture", "Developed", "Water", "UN", "UE"), times = 3),
  ref_cur = rep(c("refPercent", "currentPercent"), each = 15),
  amount = sample(10:200, 30, replace = TRUE)
)

# Plot
fake_plot <-
  ggplot(data, aes(fill=(ref_cur), y=amount, x=label)) +
  geom_col(width = 0.8, position = position_dodge()) +
  coord_flip() +
  facet_grid(. ~bps) +
  labs(
    title = "Succession Classes past and present",
    subtitle = "Fake chart with fake data",
    caption = "Data from landfire.gov",
    x = "",
    y = "Amount (units unknown")

fake_plot



#### randy's help

ref_con <- ref_con_modified %>%
  pivot_longer(!Model_Code, names_to = "refLabel", values_to = "refPercent") %>%
  unite(model_label, c("Model_Code", "refLabel"), remove = FALSE) %>%
  left_join(bps_model_number_name)

#bring in s-class labels
combine <- left_join(combine_raw, 
                     scls_aoi_attributes %>%
                       dplyr::select(2, 4),  
                     by = c("Var2" = "VALUE"))

#bring in bps labels
combine <- left_join(combine, 
                     LF16_BPS_200 %>%
                       dplyr::select(1:4),
                     by = c("Var1" = "VALUE"))

# calculate current sclass percents
combine <- combine %>%
  group_by(Var1, BPS_MODEL) %>%
  mutate(total_count = sum(Freq)) %>%
  mutate(currentPercent = as.integer((Freq/total_count)*100)) %>%
  unite(model_label, c("BPS_MODEL", "LABEL"))

aoi_ref_cur <- left_join(aoi_ref_con,
                         combine) %>%
  drop_na(refPercent) %>%
  mutate(currentPercent = as.numeric(currentPercent),
         currentPercent = ifelse(is.na(currentPercent), 0, currentPercent))%>%
  mutate(total_count = as.numeric(total_count),
         total_count = ifelse(is.na(total_count), 0, total_count)) %>%
  select(-c(BPS_CODE, ZONE)) %>%
  select(c(Freq,
           Var1,
           Var2,
           BpS_Name,
           Model_Code,
           refLabel,
           model_label,
           refPercent,
           currentPercent,
           total_count)) %>%
  rename(count = Freq,
         bps_value = Var1,
         scl_value = Var2,
         bps_name = BpS_Name) %>%
  clean_names()
