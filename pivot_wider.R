# pivot wider: table2

# load packages
library(tidyverse)

# the problem
table2

# the solution
table2 %>% 
  pivot_wider(id_cols=c(country,year),
              names_from = type,
              values_from = count)
