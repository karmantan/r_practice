# pivot longer: billboard

library(tidyverse)

# the pivot
billboard_long <- billboard %>% 
  pivot_longer(cols = starts_with("wk"),
               names_to = "week",
               values_to = "rank",
               names_prefix = "wk",
               values_drop_na = TRUE) %>% 
  mutate(week = as.numeric(week))
glimpse(billboard_long)

# which tracks lasted at least 52 weeks?
all_year <- billboard_long %>% 
  count(track) %>% 
  filter(n >= 52)
all_year

# filter for these tracks and add real date of chart
billboard_top4 <- billboard_long %>% 
  filter(track %in% all_year$track) %>% 
  mutate(Date = date.entered + 7 * week - 7)

# the plot
ggplot(billboard_top4,
       aes(x = Date,
           y = rank,
           color = track)) +
  geom_line() + 
  labs(y = "Rank",
       color = "Song title",
       title = "Popular songs of 2000") +
  scale_color_brewer(palette = "Dark2") +
  # billboard ranks, lower is better, therefore reverse chart
  ylim(c(85,0)) + # flip axes, this is the change
  theme_minimal() 


# make a new branch by going to the git tab and click on new branch on the right side
# call it sandbox and then you can see a dropdown menu next to "new branch"
# commit to sandbox