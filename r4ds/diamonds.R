library(tidyverse)
library(hms)
library(nycflights13)
as_tibble(iris)

tibble(x = 1:5,y=1,z=x^2 + y)

tibble(a = lubridate::now() + runif(1e3)*86400,
       b = lubridate::today() + runif(1e3) * 30,
       c = 1:1e3,
       d = runif(1e3),
       e = sample(letters,1e3,replace = TRUE))

nycflights13::flights %>% print(n = 10,width = Inf)
nycflights13::flights %>% View()

df = tibble( x = runif(5), y = rnorm(5))
df$x
df[["x"]]
df[[1]]


heights = read_csv("data/heights.csv")
read_csv("a,b,c
         1,2,3
         4,5,6")


library(readr)

who1 = who %>% gather(new_sp_m014:newrel_f65,key = "key",value = "cases",na.rm = TRUE)
who1
who1 %>% count(key)

who2 = who1 %>% mutate(key = stringr::str_replace(key,"newrel","new_rel"))
who2

who3 = who2 %>% separate(key,c("new","type","sexage"),sep = "_")
tail(who3)

who3 %>% count(new)

who4 = who3 %>% select(-new,-iso2,-iso3)
who4

who5 = who4 %>% separate(sexage,c("sex","age"),sep = 1)
who5

who6 = who %>% gather(code,value,new_sp_m014:newrel_f65,na.rm = TRUE)
who6


planes %>% count(tailnum) %>% filter(n>1)

flights2 = flights %>% select(year:day,hour,origin,dest,tailnum,carrier)
view(flights2)

flights2 %>% select(-origin,-dest) %>% left_join(airlines,by = "carrier")


library(stringr)

double_quote = "\""
double_quote

str_c(c("x","y","z"),collapse = ", ")

x = c("apple","banana","pear")
str_view(x,"an")
str_view(x,".a.")

library(forcats)

gss_cat



