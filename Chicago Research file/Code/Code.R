## ipums_explore.R
## Explore historical census data pulled using IPUMS
## Requires: 
## Last edited 10/25/22 by Zahoor Ahmad Khan
##########################################################


library(ipumsr)
library(haven)
library(tidyverse)

library(writexl)
library(ggthemes)
library(scales)

setwd("C:/Users/pc/Desktop/Chicago-Research-file")

##########################################
## Opportunity Insights Data
##########################################
opp_ins_data <- read.csv("data/social_capital_zip.csv")

opp_ins_data$zip <- as.character(opp_ins_data$zip)

## keep if zip code starts with 606 and are in Cook county - our area of interest
chicago_data <- opp_ins_data %>%
  filter(substr(zip, 1, 3) == "606") %>%
  filter(county==17031) %>%
  mutate(year=2018)

##########################################
## IPUMS NHGIS Demographic 1990-2020 Data
##########################################
## now bring in IPUMS NHGIS population and demographics data
demographic_data <- read.csv("data/nhgis_pop_demographics_1990_2020.csv", header = T)

demographic_data_clean <- demographic_data %>%
  rename_with(tolower) %>%
  filter(substr(zctaa, 1, 3) == "606") %>%
  rename(zip=zctaa, white=cm1aa, black=cm1ab, native=cm1ac, asian=cm1ad, 
         pacific_islander=cm1ae, other_race=cm1af, two_plus_races=cm1ag) %>%
  select(datayear, zip, white, black, native, asian, pacific_islander, other_race, two_plus_races) %>% 
  slice(-1) %>%
  mutate_if(is.character, as.numeric) %>%
  rowwise() %>%
  mutate(total_pop=sum(c_across(white:two_plus_races), na.rm=T)) %>%
  mutate(across(white:two_plus_races, ~ 100*.x/total_pop, .names="prop_{.col}")) %>%
  mutate(across(prop_white:prop_two_plus_races, round))

demographic_data_clean$zip <- as.character(demographic_data_clean$zip)
demographic_data_clean$datayear <- as.character(demographic_data_clean$datayear)

## Compare black population distributions (count, prop) across zip codes
# raw counts in 2020
p <- demographic_data_clean %>%
  filter(datayear=="2020") %>%
  ggplot() + 
  geom_bar(aes(x=reorder(zip, black), y=black), 
           stat='identity', position='dodge') + 
  theme_fivethirtyeight() + 
  labs(title="2020 Black Population Counts, Chicago Zip Codes") + 
  theme(axis.text.x = element_blank(), 
        panel.grid.major.x = element_blank()) + 
  theme(plot.title = element_text(size=16, hjust=0.8)) + 
  scale_y_continuous(label=comma)
p
ggsave("charts/black_pop_counts_dist.png", plot = p, 
       width = 7.5, height = 5, units = "in", dpi=600)

# proportions in 2020
p <- demographic_data_clean %>%
  filter(datayear=="2020") %>%
  ggplot() + 
  geom_bar(aes(x=reorder(zip, prop_black), y=prop_black), 
           stat='identity', position='dodge') + 
  theme_fivethirtyeight() + 
  labs(title="2020 Black Population Proportions, Chicago Zip Codes") + 
  theme(axis.text.x = element_blank(), 
        panel.grid.major.x = element_blank()) + 
  theme(plot.title = element_text(size=16, hjust=0.8)) + 
  scale_y_continuous(labels=paste0(seq(0,100, 20), "%"), 
                     breaks=seq(0,100, 20), limits=c(0,100))
p
ggsave("charts/black_pop_prop_dist.png", plot = p, 
       width = 7.5, height = 5, units = "in", dpi=600)


# proportions in 1990
p <- demographic_data_clean %>%
  filter(datayear=="1990") %>%
  ggplot() + 
  geom_bar(aes(x=reorder(zip, prop_black), y=prop_black), 
           stat='identity', position='dodge') + 
  theme_fivethirtyeight() + 
  labs(title="1990 Black Population Proportions, Chicago Zip Codes") + 
  theme(axis.text.x = element_blank(), 
        panel.grid.major.x = element_blank()) + 
  theme(plot.title = element_text(size=16, hjust=0.8)) + 
  scale_y_continuous(labels=paste0(seq(0,100, 20), "%"), 
                     breaks=seq(0,100, 20))
p
ggsave("charts/1990_black_pop_prop_dist.png", plot = p, 
       width = 7.5, height = 5, units = "in", dpi=600)

# export to excel to share with RA team
#write_xlsx(demographic_data_clean, 'chicago_demographic_pops_1990_2020.xlsx')

dem_trends <- demographic_data_clean %>% 
  group_by(zip) %>%
  arrange(datayear) %>%
  mutate(across(white:two_plus_races, ~ 100*(.x-lag(.x, n=3))/lag(.x, n=3), .names="growth_rate_{.col}")) %>%
  filter(growth_rate_black<500)

# population count changes 1990-2020
p <- dem_trends %>%
  filter(datayear=="2020") %>%
  ggplot() + 
  geom_bar(aes(x=reorder(zip, growth_rate_black), y=growth_rate_black), 
           stat='identity', position='dodge') + 
  theme_fivethirtyeight() + 
  labs(title="Black Population Changes 1990-2020, Chicago Zip Codes") + 
  theme(axis.text.x = element_blank(), 
        panel.grid.major.x = element_blank()) + 
  theme(plot.title = element_text(size=16, hjust=0.8)) + 
  scale_y_continuous(labels=paste0(seq(-100,500, 100), "%"), 
                     breaks=seq(-100,500, 100), limits=c(-100,500))
p
ggsave("charts/black_pop_trends_1990_2020.png", plot = p, 
       width = 7.5, height = 5, units = "in", dpi=600)

##########################################
## Merge datasets and look at spatial correlations
##########################################
# inner join Opportunity Insights data and IPUMS demographic data
chicago_combined <- chicago_data %>%
  inner_join(demographic_data_clean, by="zip")

# what is the correlation between minority proportion and economic mobility in 2020
mobility_minority_2020 <- chicago_combined %>%
  filter(datayear=="2020") %>%
  # convert number below 50th percentile variable to a proportion
  mutate(prop_below_p50=100*num_below_p50/pop2018) %>%
  # distribution of black population follows exponential shape 
  #take log of this measure
  mutate(prop_black=log(prop_black))

theme_settings <-   theme(axis.title = element_text(size=16, face="bold"),
                          plot.subtitle = element_text(size=16, hjust=0.5, face="italic"),
                          plot.title = element_text(size=20, hjust=0.5),
                          axis.text.x = element_blank(), 
                          axis.text.y = element_text(size=14))

p <- ggplot(mobility_minority_2020, aes(x=prop_black, y=prop_below_p50)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) + 
  theme_fivethirtyeight() + 
  theme_settings + 
  labs(title="Mobility-Black Proportion by Zip Code", 
       subtitle="Blue Line = Simple Linear Fit",
       x="Log(% Black)", y="% Below P50") + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid"))
p
ggsave("charts/income-percentile_black_scatter.png", plot = p, 
       width = 7.5, height = 5, units = "in", dpi=600)

p <- ggplot(mobility_minority_2020, aes(x=prop_black, y=ec_zip)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) + 
  theme_fivethirtyeight() + 
  theme_settings + 
  labs(title="Mobility-Black Proportion by Zip Code", 
       subtitle="Blue Line = Simple Linear Fit",
       x="Log(% Black)", y="Economic Connectedness") + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid"))
p
ggsave("charts/econ-connect_black_scatter.png", plot = p, 
       width = 7.5, height = 5, units = "in", dpi=600)


p <- ggplot(mobility_minority_2020, aes(x=prop_black, y=volunteering_rate_zip)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) + 
  theme_fivethirtyeight() + 
  theme_settings + 
  labs(title="Mobility-Black Proportion by Zip Code", 
       subtitle="Blue Line = Simple Linear Fit",
       x="Log(% Black)", y="Volunteering Rate") + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid"))
p
ggsave("charts/volunteer-rate_black_scatter.png", plot = p, 
       width = 7.5, height = 5, units = "in", dpi=600)
