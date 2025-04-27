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

# doing a merge on git, you want to be in the branch you are merging INTO 
# not in the branch you are merging from

# go to terminal `git merge sandbox --message "do the merge"`

# now that we are done with the branch we can delete it `git branch -d sandbox`
