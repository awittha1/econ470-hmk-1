# Question 3

q_3_df <- full_ma_data %>% 
  group_by(year, plan_type)%>% 
  summarize(n_under_plan_type = n())%>% 
  spread(year, n_under_plan_type)

# Question 4 

###Filter data
filtered_full_ma <- full_ma_data %>% 
  filter(snp == 'No'& eghp == 'No' & !(planid %in% 800:899))


###Create dataframe with filtered data
q_4_df <- filtered_full_ma %>% 
  group_by(year, plan_type)%>% 
  summarize(n_under_plan_type = n()) %>% 
  spread(year, n_under_plan_type)

# Question 5 

contract_service_area<- read_rds("data/output/contract_service_area.rds")

### Join dataframes
joined_df <- left_join(filtered_full_ma, contract_service_area ,by = c("contractid", "fips", "year"))


### Make plot 

joined_df %>% 
  filter(!is.na(avg_enrollment))%>%
  group_by(fips)%>% 
  summarize(avg_prem = mean(avg_enrollment, na.rm = TRUE))%>% 
  group_by(year)%>% 
  summarize(avg = mean(avg_prem))%>%
  ggplot(aes( x = year, y  = avg))+
  geom_line()+
  labs( title = 'Average of enrollees per county', x = 'Year', y = 'Number of enrollees')+
  theme_minimal()


# Question 6 

penetration_data <- read_rds("data/output/ma_penetration.rds")
premium_data <- read_rds ("data/output/plan_premiums.rds")

## join penetration data with premium data

joined_df2 <- left_join(premium_data, penetration_data, by = c("state", "county", "year"))

## Join this to previously filtered df 




#save.image("Hwk1_workspace.Rdata")
#rmarkdown::render(input = "hwk-01-answers", output = "hwk1-01-answers')



