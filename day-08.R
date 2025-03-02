# Name: Yazeed Aljohani
# Date: 2025-03-01
# Purpose: Create a faceted plot of cumulative COVID-19 cases & deaths by USA region and additional plots


library(tidyverse)

# Load COVID-19 dataset
covid <- read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')

# Create the state mapping data frame
df <- data.frame(region = state.region,
                 abbr = state.abb,
                 state = state.name)

# Verify column names in covid dataset
colnames(covid)  

# Ensure column names match for the join
covid_fixed <- covid %>%
  rename(state = state)  # If necessary, adjust column names

# Join and process data
covid_summary <- inner_join(df, covid, by = "state") %>%
  group_by(region, date) %>%
  summarize(cases = sum(cases, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE)) %>%
  pivot_longer(cols = c(cases, deaths),
               names_to = "type",
               values_to = "count")

# Plot data
ggplot(covid_summary, aes(x = date, y = count)) +
  geom_line() +
  facet_grid(type~region, scales = "free_y") +
  theme_bw()

