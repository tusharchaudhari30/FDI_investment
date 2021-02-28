library(tidyverse)
library(janitor)

"abc.csv" %>% 
  read.csv() %>% 
  filter(Sector == "FOOD PROCESSING INDUSTRIES") %>% 
  pivot_longer(cols = c(-1), names_to = "year", values_to = "values") %>% 
  separate(year, into=c("metric","date"), sep = 1) -> food_processing_raw_data

  gsub("[.]", "-", food_processing_raw_data$date) -> food_processing_raw_data$date
  
  food_processing_raw_data %>% 
    select(-2) %>% 
    clean_names()-> food_processing_cleaned_data


  food_processing_cleaned_data %>% 
  ggplot(aes(x = date, y = values, group = 1, color = sector)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_line(size = 1.5) + 
  scale_y_continuous(limits = c(0, 4000), breaks = seq(0, 4000, by = 500), 
                     name = "US $ Million") +
  scale_x_discrete(name ="Year") +
  theme(legend.position="top",
        legend.title = element_blank(),
        #plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5)) +
    labs(title = "Trend of Food processing Industry in FDI Equality Inflows")

         