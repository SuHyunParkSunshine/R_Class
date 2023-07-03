#미국 나스닥 데이터 분석

## 주제 : 확정해야 댐

## 0. 패키지 불러오기
library(tidyverse)

## 1. 데이터 프레임 작성
# 파일 목록을 다 들고 와야 됨
files <- list.files(path = "data/nasdaq_stock/")
# 들고온 파일 목록을 다 읽어서, 데이터프레임
stocks <- read_csv(paste0("data/nasdaq_stock/", files), id = "name") %>% # %>% 이 기호 나오면 tidyverse 쓸거임
  mutate(name = gsub("data/nasdaq_stock/", "", name),
         name = gsub("\\.csv", "", name)) %>% 
  rename_with(tolower)
stocks

# 데이터 프레임을 결합
df <- read_csv("data/nasdaq_stock_names.csv")
df
stocks <-
  stocks %>% 
  inner_join(df, by = c("name" = "stock_symbol"))

## 2. 시계열 데이터 시각화
end_labels <- (stocks %>% 
  group_by(company) %>% 
  filter(date == max(date)) %>% 
  arrange(-open) %>%  # 시작가 시준으로 sorting을 다시함
  select(open, company))[c(1:3, 12:14),] #1~3번째, 12~14번째 가지고 올거얌

# 좀 더 해봐요!
stocks %>% 
  ggplot(aes(date, open)) +
  geom_line(aes(color = company)) +
  scale_y_continuous(sec.axis = sec_axis(~., breaks = end_labels$open,
                                         labels = end_labels$company)) + #우리가 선택한 6개의 종가를 표시해줘
  scale_x_date(expand = c(0,0)) +
  labs(x = "", y = "Open", color = "", title = "주요 회사의 시작가격") +
  theme(legend.position = "none")

## 3. 시계열 데이터 분리

## 4. 종가를 예측



























































































































































































































