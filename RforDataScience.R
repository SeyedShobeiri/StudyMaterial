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
