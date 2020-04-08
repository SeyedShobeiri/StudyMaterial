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


ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))
diamonds %>% count(cut)

diamonds %>% count(cut_width(carat,0.5))

smaller = diamonds  %>% filter(carat < 3)
ggplot(data = smaller,mapping = aes(x=carat,color = cut)) + geom_freqpoly(binwidth = 0.05)

ggplot(diamonds) + geom_histogram(mapping = aes(x=y),binwidth = 0.5) + coord_cartesian(ylim = c(0,50))

unusual = diamonds %>% filter(y < 3 | y > 20) %>% arrange(y)
unusual

diamonds2 = diamonds %>% mutate(y = ifelse(y<3|y>20,NA,y))
diamonds2


ggplot(data = diamonds2, mapping = aes(x=x,y=y)) + geom_point(na.rm = TRUE)


nycflights13::flights %>% mutate(canceled = is.na(dep_time),
                                 sched_hour = sched_dep_time %/% 100,
                                 sched_min = sched_dep_time %% 100,
                                 sched_dep_time = sched_hour + sched_min /60)%>%
ggplot(mapping = aes(sched_dep_time))+geom_freqpoly(mapping = aes(color = canceled),binwidth = 1/4)

ggplot(data =diamonds,mapping = aes(x=price)) + geom_freqpoly(mapping = aes(color=cut),binwidth = 500)
ggplot(diamonds) + geom_bar(mapping = aes(x=cut))

ggplot(data = diamonds, mapping = aes(x = price,y = ..density..)) + 
  geom_freqpoly(mapping = aes(color=cut),binwidth = 500)

ggplot(data = diamonds, mapping = aes(x=cut,y=price)) + geom_boxplot()

ggplot(data = mpg) + geom_boxplot(mapping = aes(
  x = class,
  y = hwy
  )
) +
coord_flip()

ggplot(data = diamonds) + geom_count(mapping = aes(x = cut,y = color))

diamonds %>% count(color,cut) %>% ggplot(mapping = aes(x = color,y = cut)) + geom_tile(mapping = aes(fill = n))

ggplot(data = diamonds) + geom_point(mapping = aes(x = carat,y = price),alpha = 0.001)

ggplot(data = smaller) + geom_bind2d(mapping = aes(x=carat,y=price))

ggplot(data = smaller) + geom_hex(mapping = aes(x=carat,y=price))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + geom_boxplot(mapping = aes(group = cut_width(carat,0.1)))

ggplot(data = smaller, mapping = aes(x = carat,y = price)) +
  geom_boxplot(mapping = aes(group = cut_number(carat,20)))


ggplot(data = diamonds) + geom_point(mapping = aes(x= x,y = y)) +
  coord_cartesian(xlim = c(4,11),ylim = c(4,11))


ggplot(data = faithful) + geom_point(mapping = aes(x = eruptions, y = waiting))

library(modelr)
mod = lm(log(price)~log(carat), data = diamonds)
diamonds2 = diamonds %>% add_residuals(mod) %>% mutate(resid = exp(resid))

ggplot(data = diamonds2) %>% geom_point(mapping = aes(x=carat,y=resid))

diamonds %>% count(cut,clarity) %>% ggplot(aes(clarity,cut,fill = n)) + geom_tile()




















