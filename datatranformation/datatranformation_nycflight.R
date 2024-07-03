## homework

## transform nycflights13

library(nycflights13)
library(tidyverse) # dplyr

## ask 5 questions about this dataset
data("flights")
data("airlines")

view(flights)

str(flights)


flights %>%
  group_by(year,month)%>%
  summarise(n=n())
  
flights %>%
  count(year,month)

delay<-flights %>%
  select(time_hour,dep_delay,carrier) %>%
  mutate(status=if_else(dep_delay>=0,"On time","Delayed")) %>%
  left_join(airlines,by=c("carrier"="carrier"))
  
delay%>%
  group_by(name,status)%>%
  count(status)
 
delay%>%
  select(time_hour,dep_delay,status,carrier,name)%>%
  filter(status=="On time")%>%
  group_by(name)%>%
  summarise(n=n())

delay%>%
  select(time_hour,dep_delay,status,carrier,name)%>%
  filter(status=="Delayed")%>%
  group_by(name)%>%
  summarise(n=n())

flights %>%
  count(carrier) %>%
  arrange(desc(n)) %>%
  left_join(airlines)
