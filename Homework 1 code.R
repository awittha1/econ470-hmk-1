
#Homework 1 -  analysis 

################################
#Load packages and load datasets
################################

library(tidyverse)

full.ma.data <- read_rds("data/output/full_ma_data.rds")
contract.service.area <- read_rds("data/output/contract_service_area.rds")
ma.penetration.data<- read_rds("data/output/ma_penetration.rds")
plan.premiums <- read_rds("data/output/plan_premiums.rds")


################################
#Question 3
################################

q_3_df <- full.ma.data %>% 
  group_by(year,plan_type)%>% 
  summarize(n_under_plan_type = n())%>% 
  spread(year, n_under_plan_type)

################################
# Question 4 
################################

###Filter data
final.plans <- full.ma.data %>%
               filter(snp== 'No' & eghp == "No" &
               (planid < 800 | planid >= 900))



###Create dataframe with filtered data
q_4_df <- final.plans %>% 
  group_by(year, plan_type)%>% 
  summarize(n_under_plan_type = n()) %>% 
  spread(year, n_under_plan_type)



################################
# Question 5
################################

### Join dataframes
final.data <- final.plans %>%
  inner_join(contract.service.area %>% 
            select(contractid, fips, year), 
            by=c("contractid", "fips", "year")) %>%
            filter(!is.na(avg_enrollment))


### Make plot 

graph_q5<- final.data%>% 
  filter(!is.na(avg_enrollment))%>%
  group_by(fips, year)%>% 
  summarize(sum_prem = sum(avg_enrollment, na.rm = TRUE))%>% 
  group_by(year)%>% 
  summarize(avg_prem = mean(sum_prem))%>% 
  ggplot(aes( x = year, y  = avg_prem))+
  geom_line()+
  labs( title = 'Average of enrollees per county', x = 'Year', y = 'Number of enrollees')+
  theme_minimal()


################################
# Question 6
################################

final.data.pen <- final.data %>%
  left_join( ma.penetration.data %>% ungroup() %>% select(-ssa) %>%
               rename(state_long=state, county_long=county), 
             by=c("fips", "year"))

final.state <- final.data.pen %>% 
  group_by(state) %>% 
  filter(!is.na(state_long))%>%
  summarize(state_name = last(state_long))

final.data <- final.data %>%
  left_join(final.state,
            by=c("state"))

final.data <- final.data %>%
  left_join( plan.premiums,
             by=c("contractid","planid","state_name"="state","county","year")) 

## join full_ma with ma_penetration


# Make graph 
graph_q6 <- final.data %>% 
  group_by(year)%>%
  summarize(avg_premium = mean(premium, na.rm = TRUE))%>% 
  ggplot(aes(year, avg_premium))+
  geom_line()+
  labs( title = 'Average premium over time', x = 'Year', y = 'Average premium')+
  theme_minimal()


################################
# Question 7 
################################

graph_q7<- final.data %>% 
  filter(!is.na(premium))%>%
  group_by(year)%>% 
  summarize(perc_0 = ((sum(premium == 0))/n())* 100)%>% 
  ggplot( aes(year, perc_0))+
  geom_line()+
  labs( title = 'Percentage of $0 Premium Plans over time', x = 'Year', y = 'Percentage of $0 Premium Plans')+
  theme_minimal()

save.image("Hwk1_workspace.Rdata")
rmarkdown::render(input = "witthaus-a-hwk1-3", output = "witthaus-a-hwk1-3")



