library(ggplot2)
table(mpg$class)

ggplot(data = mpg,aes(x=class)) + geom_bar() + ggtitle("ggplot2")

library(shiny)
runExample("01_hello")
