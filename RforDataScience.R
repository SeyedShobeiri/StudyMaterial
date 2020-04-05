library(tidyverse)
tidyverse_update()

library(nycflights13)
library(gapminder)
library(Lahman)


devtools::session_info(c("tidyverse"))

# ---------------------------------- CHAPTER 1 : Data Visualization with ggplot2 -------------
summary(mpg)
ggplot(data=mpg) + geom_point(mapping = aes(x=displ,y=hwy,color=class,size=class),size=2) # color could also be shape, size and alpha aesthetics
?mpg

ggplot(data=mpg) + geom_point(mapping = aes(x=displ,y=hwy)) + facet_wrap(~ class,nrow = 2)

ggplot(data=mpg) + geom_point(mapping = aes(x=displ,y=hwy)) + facet_grid(drv~cyl)

ggplot(data=mpg) + geom_point(mapping = aes(x=displ,y=hwy))
ggplot(data=mpg) + geom_smooth(mapping = aes(x=displ,y=hwy,linetype = drv))

ggplot(data=mpg,mapping = aes(x=displ,y=hwy)) + geom_point(mapping = aes(color = class)) + geom_smooth()

ggplot(data = mpg, mapping = aes(x=displ,y=hwy)) + geom_point(mapping = aes(color = class)) +
  geom_smooth(data = filter(mpg,class == "subcompact"),se = FALSE)

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))

ggplot(data=diamonds) + stat_summary(
  mapping = aes(x=cut,y=depth),
  fun.ymin = min,
  fun.ymax = max,
  fun.y = median
)

ggplot(data=diamonds) + geom_bar(mapping = aes(x=cut,fill=cut))

ggplot(data=mpg) + geom_point(mapping = aes(x=displ,y=hwy),position = "jitter")

nz = map_data("nz")
ggplot(nz,aes(long,lat,group = group)) + geom_polygon(fill = "white",color = "black")

y=seq(1,10,length.out = 5)

# ---------------------------------------- dplyr ------------------------------------

library(nycflights13)
library(tidyverse)
attach(nycflights13)

?nycflights13
nycflights13::flights

view(nycflights13::flights)

filter(flights,month == 1, day == 1)

(filter(flights,month == 1, day == 1))

near(sqrt(2)^2,2)

(filter(flights,month %in% c(11,12)))

is.na(flights)

x = tibble(m = c(1,NA,3))
filter(x,x>1)
filter(x,is.na(x)|x>2)


arrange(flights,year,month,day)
arrange(flights,desc(arr_delay))

select(flights,year,month,day)
select(flights,year:dep_time)
select(flights,-c(year,month,day))


rename(flights,tail_num = tailnum)

select(flights,contains("TIME"))


flights_sml = select(flights,year:day,ends_with("delay"),distance,air_time)
mutate(flights_sml,gain = arr_delay - dep_delay,speed = distance/air_time * 60)

transmute(flights,gain = arr_delay - dep_delay,hours = air_time/60,gain_per_hour = gain / hours)
flights

summarize(flights,delay = mean(dep_delay,na.rm = TRUE))

by_day = group_by(flights,year,month,day)
summarize(by_day,delay = mean(dep_delay,na.rm = TRUE))


delays = flights %>% group_by(dest) %>% summarize(count = n(),dist = mean(distance,na.rm = TRUE),
                                                  delay = mean(arr_delay,na.rm = TRUE)) %>% filter(count > 20,dest != "HNL")

flights %>% group_by(year,month,day) %>% summarize(mean = mean(dep_delay,na.rm = TRUE))

not_canceled = flights %>% filter(!is.na(dep_delay),!is.na(arr_delay))
not_canceled %>% group_by(year,month,day) %>% summarize(mean = mean(dep_delay))

delays = not_canceled %>% group_by(tailnum) %>% summarize(delay = mean(arr_delay), n = n())
ggplot(data = delays,mapping = aes(x=n,y=delay)) + geom_point(alpha = 1.10) 

delays %>% filter(n>25) %>% ggplot(mapping = aes(x=n,y=delay)) + geom_point(alpha=1/10)

not_canceled %>% group_by(dest) %>% summarize(carriers = n_distinct(carrier)) %>% arrange(desc(carriers))






















































