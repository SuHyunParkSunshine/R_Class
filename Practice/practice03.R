#tidyverse
#palmerpenguins

install.packages("palmerpenguins")
library("tidyverse")
library("palmerpenguins")

## 1. 데이터 확인
glimpse(penguins) #데이터 전체를 가져오기
t(map_df(penguins, ~sum(is.na(.))))

plot_data <- penguins %>%
  drop_na()

t(map_df(plot_data, ~sum(is.na(.))))

## 2. 데이터 구성(이미지 표현, 종)

count_data <- plot_data %>% 
  group_by(species) %>% 
  tally()

ggplot(count_data) +
  aes(x = species, fill = species, weight = n) +
  geom_bar() #여기까지만 해도 그래프는 나오지만 하기에 코드를 더 붙여서 보기 편하게 만든다. (+로 layered시켜가지구 사용)
  
