library(tidyverse)
library(ggplot2)
library(ggthemes)

## how to choose visualization
## 1.number of variable
## 2. data type

ggplot(mtcars , aes(mpg))+
  geom_histogram(bins=8)


## one variable +continuous(number)

ggplot(diamonds,aes(x=price)) +
  geom_histogram()


ggplot(diamonds,aes(x=price)) +
  geom_density()


## DRY: Donot Repeat Yourself
base <- ggplot(diamonds,aes(x=price))
p1 <- base + geom_histogram()
p2 <- base + geom_density()

ggplot(diamonds,aes(price))+
  geom_boxplot()

## violin plot
ggplot(diamonds,aes(price,cut))+
  geom_violin()


## one variable + discrete (factor)
ggplot(diamonds,aes(cut))+
  geom_bar()

diamonds %>% 
  count(cut) %>%
  ggplot(data= .,mapping=aes(x=cut,y=n)) +
  geom_col()

## two variables: number x number
set.seed(42)
ggplot(sample_n(diamonds,3000),
       #mapping
       aes(x=carat,y=price,color=cut)) +
  geom_point(alpha=0.5,size=3)+
  labs(
    title="My first scatter plot",
    subtitle="Cool ggplot2",
    caption="Data: diamonds in Africa",
    y="Price in USD",
    x="Carat Diamonds"
  ) + 
  theme_economist()


## shortcut qplot()
## quick plot

qplot(x=carat,data=diamonds,geom="density")
qplot(x=carat,data=diamonds,geom="histogram")
qplot(x=cut,data=diamonds,geom="bar")



##layer in ggplot2

base<- ggplot(diamonds%>% 
                sample_n(1000)%>%
                filter(carat<=2.8),
              aes(carat,price))

base + 
  theme_minimal()+
  geom_point(alpha=.2,
             color="blue")+
  geom_smooth(method="loess", 
              se=TRUE,
              fill="red",
              color="green")+
  geom_rug()

##bar plot
ggplot(diamonds,aes(cut,fill=clarity))+
  geom_bar(position="fill")+
  theme_minimal()

## How to change color?
ggplot(diamonds,aes(cut,fill=cut))+
  geom_bar()+
  theme_minimal()+
  scale_fill_manual(values=c("red","green","blue","gold","salmon"))

## color palate   http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
ggplot(diamonds,aes(cut,fill=cut))+
  geom_bar()+
  theme_minimal()+
  scale_fill_brewer(palette="Set2")


## Heat map color scale
sample_diamonds<-diamonds%>% 
  sample_n(3000)
ggplot(sample_diamonds,aes(carat,price,color=price))+
  geom_point(alpha=0.3)+
  theme_minimal()+
  scale_color_gradient(low="gold",high="purple")

## multiple sub-plot

ggplot(diamonds%>% sample_frac(.2),aes(carat,price))+
  geom_point(alpha=.2)+
  geom_smooth()+
  theme_minimal()+
  facet_wrap(~cut,ncol=3)

ggplot(diamonds%>% sample_frac(.2),aes(carat,price))+
  geom_point(alpha=.2)+
  geom_smooth()+
  theme_minimal()+
  facet_wrap(clarity ~cut)


ggplot(diamonds%>% sample_frac(.2),aes(carat,price))+
  geom_point(alpha=.2,size=2,color="red")+
  geom_smooth(method="lm",color="black")+
  theme_minimal()+
  facet_grid(~cut)

##multiple dataframe
m1 <- mtcars %>% filter(mpg>30)
m2 <- mtcars %>% filter(mpg<=20)

ggplot()+
  theme_minimal()+
  geom_point(data=m1,aes(wt, mpg),color="blue")+
  geom_point(data=m2,aes(wt,mpg),color="red")


## bin 2D


ggplot(diamonds,aes(carat,price))+
  geom_bin2d(bins=50)+
  theme_minimal()

## homework
##1. create Rmarkdown
##2. create 5 chart
##3. data= diamonds or mpg


## gridextra , patch work multiple grap in one window






