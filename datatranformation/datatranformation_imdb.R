## install package in r
install.packages("dplyr")

library(dplyr)


##read csv file
imdb<-read.csv("data tranformation/imdb.csv",stringsAsFactors = FALSE)
View(imdb)
#names(imdb) <- toupper(names(imdb))
names(imdb) <- tolower(names(imdb))
glimpse(imdb)
head(imdb)


## review data structure
glimpse(imdb)


head(imdb,10)
tail(imdb,10)


# select columns 
select(imdb, movie_name,RATING)
select(imdb,1,5)

# change name
select(imdb, movie_name=MOVIE_NAME , released_year=YEAR)

#pipe operator
head(imdb)
imdb %>% 
  select(movie_name=MOVIE_NAME , released_year=YEAR) %>%
  head(10)
  
## filter data
filter(imdb,SCORE>=9.0)
imdb %>% filter(SCORE>=9.0)


names(imdb) <- tolower(names(imdb))

head(imdb)

imdb %>% 
  select(movie_name,year,score)%>%
  filter(score>=9.0 & year>2000) 


imdb %>% 
  select(movie_name,length,score) %>%
  filter(score == 8.8 | score==8.3 | score==9)


imdb %>% 
  select(movie_name,length,score) %>%
  filter(score %in% c(8.3,8.8,9.0))



##filter string column
imdb %>% 
  select(movie_name,genre,rating) %>%
  filter(genre == "Drama")


imdb %>% 
  select(movie_name,genre,rating) %>%
  filter(grepl("Drama",imdb$genre))


imdb %>% 
  select(movie_name) %>%
  filter(grepl("The",imdb$movie_name)) ## CASE sensitive upper lower 

##create new column
imdb %>%
  select(movie_name,score,length)%>%
  mutate(score_group = if_else(score>=9,"High Rating","Low Rating"),
         length_group = if_else(length>= 120 ,"Long Film","Short Film"))

  
imdb %>%
  select(movie_name,score)%>%
  mutate( score= score +0.1)%>%
  head(10)


##arrange data
head(imdb)

imdb %>%
  arrange(length)%>%
  head(5)

imdb %>%
  arrange(desc(length))%>%
  head(10)

imdb %>%
  select(movie_name,rating,length)%>%
  arrange(rating,desc(length))

## summarise and group_by
imdb %>%
  filter(rating != "") %>%
  group_by(rating,score)%>%
  summarise(mean_length=mean(length),
            sum_length=sum(length),
            sd_length=sd(length),
            min_length=min(length),
            max_length=max(length),
            n=n())


## join data
favorite_films <- data.frame(id=c(5,10,25,30,98))

favorite_films %>% 
  inner_join(imdb,by=c("id"="no"))
             

## write csv file (export)
imdb_prep<-imdb %>% 
  select(movie_name,year,rating,length,score)%>%
  filter(rating=='R' & year>2000) 

## export
write.csv(imdb_prep,"imdb_prep.csv",row.names = FALSE)
