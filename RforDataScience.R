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
