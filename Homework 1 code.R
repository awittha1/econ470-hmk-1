
#Homework 1 -  analysis 

################################
#Load packages and load datasets
################################

library(tidyverse)

full_ma <- read_rds("data/output/full_ma_data.rds")
contract_service <- read_rds("data/output/contract_service_area.rds")
ma_penetration <- read_rds("data/output/ma_penetration.rds")
plan_premiums <- read_rds("data/output/plan_premiums.rds")


################################
#Question 3
################################

q_3_df <- full_ma %>% 
  group_by(year, plan_type)%>% 
  summarize(n_under_plan_type = n())%>% 
  spread(year, n_under_plan_type)

################################
# Question 4 
################################

###Filter data
filtered_full_ma <- full_ma %>% 
  filter(snp == 'No'& eghp == 'No' & !(planid %in% 800:899))


###Create dataframe with filtered data
q_4_df <- filtered_full_ma %>% 
  group_by(year, plan_type)%>% 
  summarize(n_under_plan_type = n()) %>% 
  spread(year, n_under_plan_type)



################################
# Question 5
################################

### Join dataframes
joined_df <- inner_join(filtered_full_ma, contract_service %>% select(c("contractid", "fips","year")) ,by = c("contractid", "fips", "year"))


### Make plot 

joined_df %>% 
  filter(!is.na(avg_enrollment))%>%
  group_by(fips, year)%>% 
  summarize(avg_prem = sum(avg_enrollment, na.rm = TRUE))
  # ggplot(aes( x = year, y  = avg_pem))+
  # geom_line()+
  # labs( title = 'Average of enrollees per county', x = 'Year', y = 'Number of enrollees')+
  # theme_minimal()


# Question 6 


## join penetration data with premium data

joined_df2 <- left_join(premium_data, penetration_data, by = c("state", "county", "year"))

## Join this to previously filtered df 




save.image("Hwk1_workspace.Rdata")
rmarkdown::render(input = "witthaus-a-hwk1-3", output = "witthaus-a-hwk1-3")



