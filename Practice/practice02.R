library("tidyverse")
data(mpg)
mpg

filter(mpg, manufacturer=="hyundai")
filter(mpg, manufacturer=="hyundai", cty >= 20)

hyundai_2008 <- filter(mpg, manufacturer=="hyundai", year==2008)
hyundai_2008

slice(hyundai_2008, 1)
arrange(hyundai_2008, model, trans)
