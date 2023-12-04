#지표 분석

library('readxl')
library(dplyr)
library(ggplot2)
theme_set(theme_grey(base_family='NanumGothic'))

money<- read_excel('budget.xlsx')
num_hos<- read_excel('의료시설수.xlsx')
num_fixed<- read_excel('재가,시설급여_정원.xlsx')

View(money)
View(num_hos)
View(num_fixed)

#컬럼 변경에 스케일링(전처리)
#예산

colnames(money)<- c('city', 'budget')
money<- money %>% filter(!is.na(city))
n_money<- money %>% filter(city != '전국계') %>% 
  filter(!grepl("서울|대구|대전|인천|광주|부산|울산", city)) %>% 
  mutate(sclaed_num = budget/ 1000)
View(n_money)

#정원
colnames(num_fixed) <- c('city', '시설_정원_수')
n_num_fixed<- num_fixed %>% filter(!is.na(시설_정원_수)) %>% 
  filter(city!= '소계' & 시설_정원_수 <7000)

#병의원
colnames(num_hos)<- c('city', '병의원_수')


#시각화

#예산
#(박스플롯 막대그래프 활용한 시각화)
boxplot(n_money$sclaed_num, ylim = c(0, 2500), main= 'budget of each cities') #단위는 억원이다!
abline(h = 1527, col = "red", lty = 1)
abline(h = 4528, col = "red", lty = 1)
abline(h = 842, col = "red", lty = 1)
abline(h = 1951, col = "red", lty = 1)
abline(h = 1625, col = "red", lty = 1)
abline(h = 426, col = "blue", lty = 1)
abline(h = 539, col = "blue", lty = 1)
abline(h = 693, col = "blue", lty = 1)
abline(h = 388, col = "blue", lty = 1)
abline(h = 328, col = "blue", lty = 1)

text(x = 1.2, y = 2200, labels = "head5 cities", col = "red", pos = 4)
text(x = 1.2, y = 2100, labels = "tail5 cities", col = "blue", pos = 4)


#정원
#(밀도함수 그래프 시각화)
head5_data<- c(6143, 4940, 1856, 3892, 3635)
tail5_data<- c(137, 326, 428, 353, 174)
  
ggplot(n_num_fixed) +
  geom_density(aes(x = 시설_정원_수), fill = "gray", alpha = 0.5) +  # 채우기 색상 및 투명도 조절
  labs(title = "The density of fixed number", x = "number of people", y = "density") +
  theme_classic() +
  theme(axis.text = element_text(size = 12),  # 축 레이블 텍스트 크기 조절
        axis.title = element_text(size = 14, face = "bold"))+  # 축 제목 텍스트 크기 및 굵기 조절
geom_vline(xintercept = head5_data, linetype = "dashed", color = "red", size = 0.5)+
  geom_vline(xintercept = tail5_data, linetype = "dashed", color = "blue", size = 0.5)

text(x = 2000, y = 3e-04, labels = "head5 cities", col = "red", pos = 4)

  

#병의원


boxplot(n_num_fixed$시설_정원_수)

boxplot(num_hos$병의원_수)
