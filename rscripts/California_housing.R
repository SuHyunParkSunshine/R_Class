library(tidyverse)
library(reshape2)

# 1. 데이터 불러오기
housing = read_csv("data/housing.csv")

## 앞/뒤를 확인해서 해당 데이터 확인
head(housing)
tail(housing)
summary(housing)

## 데이터 전체 구조를 확인
str(housing)

## EDA => 시각화

plot_histo <- ggplot(data = melt(housing), mapping = aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~variable, scales = 'free_x') #변수들을 감싸고 너의 스케일에 맞게 조정하라
plot_histo

ggplot(data = housing, mapping = aes(x = longitude, y = latitude, color = median_house_value)) +
  geom_point(aes(size = population), alpha = 0.4)

# 2. 전처리

## 이상치 처리
#-- 개수세서 median 값 잡으면 된다.
bedroom_mean <- mean(housing$total_bedrooms, na.rm = T)
#bedroom_median <- median(housing$total_bedrooms, na.rm = T)
#bedroom_median
#bedroom_mean
# 값을 밀어넣을 것 -> 결측치 처리해서 NA값 살릴 생각, 저 끝에 있는 데는 어떻게 할 수 없어서 totalBedrooms 살리고 싶음
#혹쉬 'housing$total_bedrooms[is.na(housing$total_bedrooms)]'가 비엇으면 'median(housing$total_bedrooms, na.rm = T)'을 넣어주세여
#결측치 처리
housing$total_bedrooms[is.na(housing$total_bedrooms)] <- median(housing$total_bedrooms, na.rm = T)
housing$mean_bedrooms <- housing$total_bedrooms / housing$households # 1인당 방 개수
housing$mean_rooms <- housing$total_rooms / housing$households
head(housing)

drops <- c('total_bedrooms', 'total_rooms')
housing <- housing[,!(names(housing) %in% drops)] #행은 비우고 열만 지울 것임/ 얘뺴고 다 살리는 방향으로 가야한다(배제조건으로 가야한다). 
housing

## 범주형
categories <- unique(housing$ocean_proximity)
cat_housing <- data.frame(ocean_proximity = housing$ocean_proximity)
head(cat_housing)

ggplot(data = housing, mapping = aes(x = total_bedrooms)) +
  geom_histogram(bins = 30, color = "black", fill = "blue") +
  geom_vline(aes(xintercept = bedroom_mean, color = "red"), lwd = 1.5) +
  geom_vline(aes(xintercept = bedroom_median, color = "yellow"), lwd = 1.5)

### 중위값으로 한다고 가정하자!

## 결측치 처리

# 3. 머신러닝

# 4. 결과 확인