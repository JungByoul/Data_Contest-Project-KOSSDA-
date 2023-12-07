install.packages("openxlsx")
library(openxlsx)
library(readxl)

잠재수요 <- read_excel(file.choose())
View(잠재수요)

실재수요 <- read_excel(file.choose())
View(실재수요)

## 결측치, 이상치(소계) 처리
install.packages("dplyr")
library(dplyr)

md <- 실재수요 %>% filter(!is.na(전체) & !is.na(인원수)) %>% 
  filter(시군구 != "소계")
View(md)

boxplot(md$인원수)

## 회귀분석
model <- lm(전체 ~ 인원수, data = md)
summary(model)
summary(lm.beta(model))

plot(md$인원수, md$전체, xlab="건강보험대상 인구수", ylab="노인복지시설 수", pch=16, col="blue")
abline(model)


## 관련 시각화
# "전체비율" 기준 내림차순으로 정렬
md_ratio <- md[order(md$전체_비율, decreasing = TRUE), ]

# 상위 10개와 하위 10개 선택
ratio_10 <- rbind(head(md_sorted, 10), tail(md_sorted, 10))

# 막대그래프 그리기
install.packages("ggplot2")
library(ggplot2)

ggplot(ratio_10, aes(x = reorder(시군구, -전체_비율), y = 전체_비율, fill = 시군구)) +
  geom_bar(stat = "identity") +
  labs(title = "복지시설 비율 상하위 10개 지역",
       x = "시군구",
       y = "전체 비율") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# -> 고성군 행이 두개임... 아마 합쳐져서 보인듯? 이거 어떻게 나누지? 왜 2개가 됐지?


# "인원수" 기준 내림차순으로 정렬
md_people <- md[order(md$인원수, decreasing = TRUE), ]

# 상위 10개와 하위 10개 선택
people_10 <- rbind(head(md_people, 10), tail(md_people, 10))

# 막대그래프 그리기
ggplot(people_10, aes(x = reorder(시군구, -인원수), y = 인원수, fill = 시군구)) +
  geom_bar(stat = "identity") +
  labs(title = "인원수 상하위 10개 지역",
       x = "시군구",
       y = "인원수") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# "전체시설" 기준 내림차순으로 정렬
md_building <- md[order(md$전체, decreasing = TRUE), ]

# 상위 10개와 하위 10개 선택
building_10 <- rbind(head(md_building, 10), tail(md_building, 10))

# 막대그래프 그리기
ggplot(building_10, aes(x = reorder(시군구, -전체), y = 전체, fill = 시군구)) +
  geom_bar(stat = "identity") +
  labs(title = "복지시설 개수 상하위 10개 지역",
       x = "시군구",
       y = "전체시설") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
