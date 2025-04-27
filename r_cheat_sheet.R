rm(list=ls())
setwd("/Users/tankarman/Library/CloudStorage/OneDrive-Personal/BiB/DIDI/r_practice")


abs(-17)
y <- c(-12,6,0,1)
sessionInfo()

library(readxl)
library(tidyverse)
Scooby <- read.csv('scoobydoo.csv')
View(Scooby)


# summary information
mean(Scooby$run_time)
# view all variables
str(Scooby)
# look at mean imdb rating
mean(Scooby$imdb) # there are NAs
# to deal with NAs, remove them
mean(Scooby$imdb, na.rm=TRUE)
Scooby$imdb<- as.numeric(Scooby$imdb)
mean(Scooby$imdb, na.rm=TRUE)
View(Scooby$imdb)

# preset R datasets
data()
View(mpg) # mpg dataset
?mpg
# overview of variables
glimpse(mpg) # works like str() command, also shows the columns
# filter data based on some characteristic values/condition
?filter
# filter to obs where the city mileage is greater than 20
filter(mpg, cty >= 20)
mpg_efficient <- filter(mpg, cty >= 20)
View(mpg_efficient)
# now filter by manufacturer. keep only fords
mpg_ford <- filter(mpg, manufacturer=='ford')
head(mpg_ford) # tibble in console
View(mpg_ford) # new window
mpg_ford # bigger tibble in console
# now create a new dataset with kilometers per liter instead of mpg
# use mutate to change variable cty. call it cty_metric
mpg_metric <- mutate(mpg, cty_metric=0.425144*cty)
# add new column to original dataset
mpg$cty_metric <- mpg$cty*0.425144
glimpse(mpg)

# pass mpg as the first argument into the mutate function
mpg_metric <- mpg %>% mutate(cty_metric=0.425144*cty)

# groupby: take mpg, group by class and find the mean of each class
mpg %>% 
  group_by(class) %>% 
  summarize(mean(cty), 
            median(cty))

# plots with ggplot2
library(ggplot2)
# without pipe, variables go in aes/aesthetic, type of plot
ggplot(mpg, aes(x = cty)) +
  geom_histogram() + # type of plot
  geom_freqpoly() + # plot another plot simultaneously
  labs(x='City mileage') # x-label

# with pipe
mpg %>% ggplot(aes(x=cty,y=hwy)) +
  geom_point() + # scatter line
  geom_smooth(method='lm') # linear regression line

# add extra aesthetic, so class has different color points
mpg %>% ggplot(aes(x=cty,y=hwy,color=class)) +
  geom_point() +
  scale_color_brewer(palette='Dark2') # change color palette


#########################################################
#         variable generation for simulation            #
#########################################################

library(tidyverse)
x <- 1:10
y <- rnorm(10)
df <- data.frame(x=x,y=y)
w <- 5

df %>% 
  ggplot(aes(x,y)) +
  geom_point(col='red', # change color to red
             size = 2) # change point size

#########################################################
#                        here                           #
#########################################################
library(here) # kind of like python os.getcwd()

file <- here('data', 'scoobydoo.csv') 

#########################################################
#             generating sequences in R                 #
#########################################################

# method 1
c(1,2,3,4,5)

# method 2 
1:5

# you can also do this for decimal points but it goes in a
# point of 1
1.2:5.2

# if you start with a number which has decimals and end in an integer
# it will ignore the last integer
1.2:5

# sequence operator, specify range, step size is "by"
seq(1,5, by=.5)

# much like the colon operator, leaves out any value that goes above
# upper limit
seq(1,5, by=.3)

# now let R generate 10 numbers between 1 and 5 using length.out
seq(1,5, length.out=10)

x <- c(1,3,5)
# generate a sequence from 1 to 5 that has same length as vector x
seq(1,5, along.with=x) 
?seq

#########################################################
#                       matrix                          #
#########################################################

x <- 1:12

# method 1
# build a matrix, note that by default, it fills in the columns first
m <- matrix(x, nrow=3)
m
m <- matrix(x, ncol=4)

# tell R to fill by rows first (like transpose the above matrix)
m <- matrix(x, ncol=4, byrow=T)
m
# dimensions
dim(m)

# method 2, use the dimensions command
dim(x) <- c(4,3) # build 4 by 3 matrix
x
x_prime <- t(x)
x_prime

# indexing, get specific object/element in matrix
m[2,3]
# get multiple elements/sub matrices, second row, columns 3 and 4
m[2,3:4]
m[1:2, 2:4]
# get all columns, leave columns blank
m[1:2,]
# turn row 1, column 3 element into -3
m[1,3] <- -3
m

# change row names
rownames(m) <- c('R1', 'R2', 'R3')
m
colnames(m) <- c('C1', 'C2', 'C3', 'C4')
m

# summary statistics, get sums of rows
rowSums(m)

# get columns sums
colSums(m)

# get row and column means
rowMeans(m)
colMeans(m)

# get submatrices, 3x3
m2 <- m[1:3, 1:3]
x2 <- x[1:3, 1:3]

# matrix operations
x2 + m2
2*m2

# m2 * x2 is element by element multiplication NOT matrix multiplication
m2 * x2

# matrix multiplication
m2 %*% x2

# transpose
t(m2)

# inverse matrix
solve(m2)

#########################################################
#                    data wrangling                     #
#########################################################

library(tidyverse)
?diamonds

# subset by row with filter
## only ideal cut diamonds
filter(diamonds, cut=='Ideal')
diamonds_sm <- diamonds %>% filter(cut=='Ideal')
## get expensive diamonds
diamonds_sm <- diamonds %>% filter(price>10000)

## multiple criteria (AND) use comma ,
diamonds_sm <- diamonds %>% filter(cut=='Ideal',
                                   price>10000)
## multiple criteria (OR) use vertical line |
diamonds_sm <- diamonds %>% filter(cut=='Ideal'|
                                     price>10000)

## filter for missing values using is.na or not missing using !is.na
diamonds %>% filter(is.na(cut)) # no missing values

# subset by column with select
diamonds_sm <- select(diamonds, cut, color)
diamonds_sm
## can reverse order
diamonds_sm <- diamonds %>% select(color,cut)
diamonds_sm
## just take first 4 columns
diamonds_sm <- diamonds %>% select(1:4)
diamonds_sm
## get all columns which start with c
diamonds_sm <- diamonds %>% select(starts_with('c')) # there is also ends_with
diamonds_sm
## get all columns which contain 'c'
diamonds_sm <- diamonds %>% select(contains('c'))
?select
## move price upfront, i.e. make it the first column, using everything
diamonds_sm <- diamonds %>% select(price, everything())
## get rid of one column
diamonds_sm <- diamonds %>% select(-price)

# reorder rows with arrange
## arrange by color, alphabetically
diamonds_arr <- diamonds %>% arrange(color)
diamonds_arr
## now by carat, ascending
diamonds_arr <- diamonds %>% arrange(carat)
## now first by color then by carat, order matters
diamonds_arr <- diamonds %>% arrange(color,
                                     carat)
## now descending order
diamonds_arr <- diamonds %>% arrange(desc(carat))
## another way to do descending
diamonds_arr <- diamonds %>% arrange(-carat)
glimpse(diamonds_arr)

# add or modify columns with mutate
## add a column with weight in grams 
diamonds_new <- diamonds %>% mutate(mass_g = 0.20*carat)
## add multiple columns
diamonds_new <- diamonds %>% mutate(mass_g=0.2*carat,
                                    price_per_carat = price/carat)
## modify column values while we are at it
diamonds_new <- diamonds %>% mutate(mass_g=0.2*carat,
                                    price_per_carat = price/carat,
                                    cut= tolower(cut)) # turn variables into all lowercase
## make new column, is a diamond expensive?
diamonds_new <- diamonds %>% mutate(mass_g=0.2*carat,
                                    price_per_carat = price/carat,
                                    cut= tolower(cut),
                                    expensive_TF = price>10000)

# other smaller verbs -----------------------------------------------------
# new section is command + shift + R
?slice_max
?bind_rows
?left_join
?rename
?case_when

# grouped summaries with group_by
## group diamonds by cut and then get the mean price
diamonds %>% group_by(cut) %>% summarize(mean(price))

diamonds %>% group_by(cut) %>% summarize(avg_price=mean(price),
                                         sd_price=sd(price))
## group by multiple variables simultaneously
diamonds %>% group_by(cut,color) %>% summarize(avg_price=mean(price),
                                               sd_price=sd(price),
                                               count=n()) # count
## count variable can also be used directly
diamonds %>% count(cut,color)
## group by expensive or not where expensive is greater than 10000
diamonds %>% group_by(expensive=price>10000) %>% summarize(avg_price=mean(price),
                                                           sd_price=sd(price),
                                                           count=n())


# splitting columns with separate -----------------------------------------

# column in data frame can potentially have more than one variable
# example name: john smith, but we want, first and last name
?separate
View(smiths)

smiths %>% separate(col=subject,
                    into=c('first','last'),
                    remove=FALSE) # keep original subject column

# separate usually by non-numeric , i.e. space, ;, ,,
smiths %>% separate(col=subject,
                    into=c('first','last'),
                    remove=FALSE,
                    sep=' ')

# we dont need the last name, because it's redundant here
smiths %>% separate(col=subject,
                    into=c('first',NA),
                    remove=FALSE,
                    sep=' ')
# height can also be separated
smiths %>% separate(col=height,
                    into=c('m','cm'),
                    convert=TRUE) # height becomes chr type, we want numeric


# combine columns with unite ----------------------------------------------
# unite is basically the opposite of separate
library(nycflights13)
?unite
glimpse(flights)

flights %>% unite(date, # new name of column
                  year, month, day, # these are the columns, tidyverse syntax
                  remove=FALSE) 
# default separator for unite command is underscore
flights %>% unite(date,
                  year, month, day,
                  remove=FALSE,
                  sep='-') %>% # use dash as separator
  mutate(date=as.Date(date))

# tidying data with pivot_longer ------------------------------------------
library(readxl)
?pivot_longer
# take data from wide format and make it long
Body_temps <- read_excel('03 - Body Temperatures.xlsx')
length(Body_temps) # columns
nrow(Body_temps)
# in the body temps data set, there are four columns with temps at diff
# times of the day. so make a new variable with time and one with temperature
Body_temps %>% pivot_longer(3:6, # columns 3 to 6, note indexing a bit diff
                            names_to='Day_Time',
                            values_to='Temperature')
# another way to get columns
temps <- Body_temps %>% pivot_longer(starts_with('DAY'), # columns 3 to 6, note indexing a bit diff
                                     names_to='Day_Time',
                                     values_to='Temperature',
                                     values_drop_na = TRUE) # get rid of NA

temps %>% ggplot(aes(x=Day_Time,y=Temperature,col=Day_Time)) +
  geom_boxplot()


# split date and time
temps <- Body_temps %>% pivot_longer(starts_with('DAY'), # columns 3 to 6, note indexing a bit diff
                                     names_to=c('Day', 'Time'),
                                     names_sep = ' - ', # separator for day time
                                     values_to='Temperature',
                                     values_drop_na = TRUE) 

temps %>% ggplot(aes(x='', y = Temperature)) +
  geom_boxplot() +
  facet_grid(Day ~ Time) +
  labs(x='') # blank x label


# tidying data with pivot_wider -------------------------------------------

?pivot_wider
FQA <- read.csv('FQA.csv')
# each variable will be a column name
FQA_wide <- FQA %>% pivot_wider(names_from=variable, # the column where the variables names are
                                values_from=value)


# combining data sets with left_join --------------------------------------

library(babynames)
library(tidyverse)
?left_join

babynames # dataset 1
applicants # dataset 2

# take babynames dataset and include the n_all col from applicants dataset
babynames %>% left_join(applicants) # take not of join_by

# building states datasets, preinstalled
state_geog <- tibble(state=state.abb,
                     area=state.area,
                     center_long=state.center$x,
                     center_lat=state.center$y)

state_reg <- tibble(name=state.name,
                    abbr=state.abb,
                    region=state.region)

# state.abb is represented by different variable names in the two datasets
# names are  state and abbr
states <- state_geog %>% left_join(state_reg,
                                   by=c('state'='abbr')) %>% # these two are the same
  select(name,abbr=state,region,everything()) # change column order


# writing functions -------------------------------------------------------

library(modeldata)
library(tidyverse)

View(penguins)

# basic example
keep_top <- function(values) {
  values[values > mean(values, # keep only values greater than the mean
                       na.rm=TRUE)] # drop NAs
}

keep_top(penguins$bill_length_mm)

# a second argument with default value
keep_top_spec <- function(values, cutoff) {
  values[values > cutoff] # keep values above the threshold
}

keep_top_spec(penguins$bill_length_mm, cutoff=45)

# keep top with a default cutoff

keep_top_spec <- function(values, cutoff=40) {
  values[values > cutoff]
}

keep_top_spec(penguins$bill_length_mm) # it's ok to forgo cutoff, it just defaults to 40

# errors and warnings
keep_top(penguins$species) # throws an error message

# fix the error by throwing a helpful warning messages
keep_top <- function(values) {
  if (!is.numeric(values)) {
    stop('Input vector must be numeric.',
         call. = FALSE) # the error message is too long, no need to tell which
    # function threw the error
  }
  else {
    values[values > mean(values,
                         na.rm=TRUE)]
  }
}

keep_top(penguins$species)
keep_top(penguins$bill_length_mm)

# stop() basically stops the entire function. how about just a warning message?
# and pass the original vector back to the user

keep_top <- function(values) {
  if (!is.numeric(values)) {
    warning('Input vector must be numeric',
            call. = FALSE)
    return(values) # input vector given back to user
    # usually return returns the last value which worked before it stopped
  }
  values[values > mean(values, na.rm=TRUE)]
}

keep_top(penguins$species)

# passing arguments ($) to function as a list
args <- list(penguins$bill_length_mm,
             cutoff=50)
args

# can't pass to keep_top_spec directly
keep_top_specs(args) # takes args as literal input

# tell keep_top_specs that args needs to be UNPACKED as a list and not literally taken as an input
do.call(keep_top_spec, args)


# structure of R functions ------------------------------------------------

keep_top_spec <- function(vals, cutoff=mean(vals)){
  # returns only elements of vals above cutoff
  vals(keep_top_spec)
}

# three components to every R function
## first is formals
formals(keep_top_spec) # shows expected arguments
## second is body
body(keep_top_spec) # code inside function

View(keep_top_spec) # just views function as is in viewer

View(which)
?which

body(is.na) # doesn't work -> NULL
typeof(is.na) # shows it is a builtin function written in C, so that's why no body output
typeof(keep_top_spec) # gets "closure" type, written in R

# third is environment of a function
just_y <- function() {
  y <- 4
  y
}

just_y()
y # object y not found, because y inside function (CLOSURE)

y <- 9
just_y() # still outputs 4, local environment dominating

simpler_y <- function(){
  y
}
simpler_y()
y <- 4
simpler_y() # now output depending on globally defined y


add_one <- function(){
  # first check if variable u is defined
  if (!exists('u')){
    u <- 0
  } else {
    u <- u + 1
  }
  u
}

add_one()


# using Git/GitHub with R -------------------------------------------------

library(usethis)
use_git_config(user.name='Kar Man Tan',
               user.email='carmen-tan@hotmail.com')

# next to the environment, history, connections tab, click on "git"
# select projects to commit/stage
# click commit


# edit and track changes --------------------------------------------------

# lots of new code blablabla just to demonstrate for commit

# keep comments short


# revert to last commit ---------------------------------------------------

# this is the commit to revert to
some_Variable <- c(1,2,3)

# blablabla



