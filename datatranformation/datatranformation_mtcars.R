library(tidyverse)
library(glue)
library(lubridate)
library(dplyr)

## glue
my_name <- "auy"
my_age <- 23
glue("Hi! my name is {my_name}. Today I'm {my_age} years old.")

paste("Hi! my name is ", my_name , " ")


##tidyverse
#data transformation => dplyr
# 1. select
# 2. filter
# 3. arrage
# 4. mutate => create new column
# 5. summarise

mtcars<- rownames_to_column(mtcars,"model")
view(mtcars)


mtcars %>%
  select(model,mpg,hp,wt,am)%>%
  filter(grepl("^D",model)& hp >150)


m3 <- mtcars %>%
  select(model,mpg,hp,wt,am)%>%
  filter(between(hp,100,200))%>%
  arrange(am, desc(hp))

#write csv file
write_csv(m3,"result.csv")

#mutate to create new column
mtcars %>%
  select(model,mpg,hp,wt) %>%
  filter(mpg >= 20) %>%
  mutate(model_upper= toupper(model),
        mpg_double=mpg*2,
        mpg_hahaha=mpg_double+10,
        new_col='new rookie')

mtcars %>%
  select(model,mpg,hp,wt,am) %>%
  filter(mpg >= 20) %>%
  mutate(hp_halve=hp/2,
         am=if_else(am==0,"auto","manual"))



## summarise,summarize

mtcars%>%
  summarise(avg_mpg = mean(mpg),
            sum_mpg = sum(mpg),
            min_mpg = min(mpg),
            max_hp = max(hp),
            n = n())

m4 <- mtcars %>%
  mutate(am = if_else(am==0,
                      "Auto","Manual")) %>%
  group_by(am) %>%
  summarise(avg_mpg = mean(mpg),
            sum_mpg = sum(mpg),
            min_mpg = min(mpg),
            max_hp = max(hp),
            n = n())

write_csv(m4,"datayouask.csv")


## random sampling 
mtcars %>%
  sample_n(2) %>%
  pull(model)

mtcars %>%
  sample_frac(0.20) %>%
  summarise(avg_hp = mean(hp))


## count
mtcars2 <- mtcars %>%
  mutate(am = if_else(am==0, "Auto",
                      "Manual"))

mtcars2 %>%
  count(am,cyl)
