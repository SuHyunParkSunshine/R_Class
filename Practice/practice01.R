# 연습용 (2023. 06. 15.)

#문자 합치기
#x를 지우고, 1번쨰와2번째,"전체"를 붙이고 붙인 내용을 "."으로 나누어 나타냄
#gsub("x","",paste(n[1], n[2], "전체", sep="."))
#melt_data[melt_data$시군구별 == "시군구별"] #$: colum선택하는 것

## 1. 파일 읽기
df <- read.csv("data/시군구_성_월별_출생_2023.csv", fileEncoding = "euc-kr")
df

## 2. 전처리
colnames(df)

# 데이터 정리하기
#apply
f <- function(x) {
  n <- unlist(strsplit(x, "\\."))
  if (length(n) == 1) {
    return (x)
  } else if (length(n) == 2) {
    return (gsub("X", "", paste(n[1], n[2], "전체", sep=".")))
  } else {
    if (identical(n[3], "1")) {
      return (gsub("X", "", paste(n[1], n[2], "남자", sep=".")))
    } else {
      return (gsub("X", "", paste(n[1], n[2], "여자", sep=".")))
    }
  }
  return (n)
}

names(df) <- lapply(colnames(df), f)
names(df)

head(df)

## 3. 데이터 분석이 용이하도록 구조 변경

library(reshape2)

melt_data <- melt(df, id = "시군구별")
head(melt_data)

melt_data[melt_data["시군구별"] == "시군구별"]
unique(melt_data$시군구별)

df2 <- melt_data[!(melt_data["시군구별"] == "시군구별"),]
head(df2)

## 4. 데이터 정리
f1 <- function(x) {
  n <- unlist(strsplit(x, "\\."))
  return(n[1])
}

f2 <- function(x) {
  n <- unlist(strsplit(x, "\\."))
  return(n[2])
}

f3 <- function(x) {
  n <- unlist(strsplit(x, "\\."))
  return(n[3])
}

df2["연도"] <- apply(df2["variable"], 1, f1)
df2["월"] <- apply(df2["variable"], 1, f2) 
df2["성별"] <- apply(df2["variable"], 1, f3)
head(df2)

colnames(df2)[3] <- "출생아수"

head(df2)

## 5. 데이터 선별
df_all = df2[(df2["시군구별"] == "전국") & (df2["성별"] == "전체"),]
df_all = df_all[, c("출생아수", "연도", "월")]
df_all

## 6. 시각화로 확인해보기
# sum_agg = aggregate(as.integer(df_all["출생아수"]~df_all["연도"]),FUN=sum)
# mode(df_all["출생아수"]) #어떤 타입인지 알기위한 것
# mode(df_all$출생아수)
# class(df_all["출생아수"])
# class(df_all$출생아수)

sum_agg = aggregate(as.integer(df_all$출생아수)~as.integer(df_all$연도), FUN=sum)

colnames(sum_agg)[1] <- "연도"
colnames(sum_agg)[2] <- "출생아수"
colnames(sum_agg)
plot(sum_agg$연도, sum_agg$출생아수, type='b') #그래프 그리기

