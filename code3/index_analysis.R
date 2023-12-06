#지표 분석

money_hosp_result$병의원_나누기_총인구<- ifelse(money_hosp_result$병의원_나누기_총인구>0, money_hosp_result$병의원_나누기_총인구*1000, NA)

fixed_num_result

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
  mutate(sclaed_num = budget/ 1000)#백만원 단위
View(n_money)

#정원
colnames(num_fixed) <- c('city', '시설_정원_수')
n_num_fixed<- num_fixed %>% filter(!is.na(시설_정원_수)) %>% 
  filter(city!= '소계' & 시설_정원_수 <7000)

#병의원
colnames(num_hos)<- c('city', '병의원_수')
View(num_hos)

#시각화

#예산
#(박스플롯 막대그래프 활용한 시각화)
boxplot(n_money$sclaed_num, ylim = c(0, 4600), main= 'budget of each cities') #단위는 억원이다!
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
boxplot(num_hos$병의원_수,  ylim = c(0, 1000), main= 'hospitals of each city')

abline(h = 288, col = "red", lty = 1)
abline(h = 506, col = "red", lty = 1)
abline(h = 132, col = "red", lty = 1)
abline(h = 674, col = "red", lty = 1)
abline(h = 442, col = "red", lty = 1)
abline(h = 97, col = "blue", lty = 1)
abline(h = 63, col = "blue", lty = 1)
abline(h = 87, col = "blue", lty = 1)
abline(h = 38, col = "blue", lty = 1)
abline(h = 45, col = "blue", lty = 1)


#인구수로 나누기
tot_pop<- read_excel('total_pop.xlsx')
ins_pop<- read_excel('in_pop.xlsx')
View(tot_pop)
View(ins_pop)


예산-병의원-총인구 컬럼 같아야함
tot_money_hosp <- left_join(nn_money, nn_tot_pop, by = 'city')
tot_money_hosp<- left_join(tot_money_hosp, nn_num_hos, by='city')
View(tot_money_hosp)

money_hosp_result<- tot_money_hosp %>%
  mutate(예산_나누기_총인구 = 예산_억원 / 건강보험_대상자,병의원_나누기_총인구 = 병의원_수 / 건강보험_대상자)
View(money_hosp_result)

정원-보험인구 컬럼 같아야함

#예산 나누기 총인구
n_money
View(n_money)
View(num_hos)

#예산 전처리
n_money$city <- gsub("(경기|강원|전북|전남|충북|충남|경북|경남)", "", n_money$city)
df_n_money<- n_money %>%
  filter(!grepl("계", city)) %>%  # '계'가 들어가지 않는 행 선택
  mutate(city = gsub("본청", "시", city),
         예산_억원= sclaed_num) %>% 
  select(city, 예산_억원)
View(nn_money)

nn_money<- df_n_money %>% 
  mutate(city = if_else(city == '고성군' & 예산_억원> 600, '고성군경남', ifelse(city =='고성군', '고성군강원', city) ))

#총인구 전처리
n_tot_pop<- tot_pop %>% mutate(city = gsub(" ", "", 시군구)) %>% 
  select(city, 건강보험_대상자) %>% 
  filter(!is.na(건강보험_대상자))

n1_tot_pop<- n_tot_pop %>% mutate(city = if_else(city == '고성군' & 건강보험_대상자> 10000, '고성군경남', ifelse(city =='고성군', '고성군강원', city) ))

View(n1_tot_pop)

nn_tot_pop <- n1_tot_pop %>%
  mutate(city = gsub("^(.+시).*$", "\\1", city)) %>%
  group_by(city) %>%
  summarise(city = city,
            건강보험_대상자=sum(건강보험_대상자)) %>% 
  distinct()

View(nn_tot_pop)

#병의원 전처리
n_num_hos <- num_hos %>% mutate(city = if_else(city == '고성군' & 병의원_수> 80, '고성군경남', ifelse(city =='고성군', '고성군강원', city) ))
View(n_num_hos)

nn_num_hos<- n_num_hos%>%
  mutate(city = gsub("^(.+시).*$", "\\1", city)) %>%
  group_by(city) %>%
  summarise(city = city,
            병의원_수=sum(병의원_수)) %>% 
  distinct()
View(nn_num_hos)


#정원수 나누기 보험인구
#정원 수
View(n_n_num_fixed)
newfixed<- as.data.frame(ifelse(n_num_fixed$city == '세종', '세종시', n_num_fixed$city))

newfixed <- n_num_fixed %>% mutate(city = if_else(city == '세종', '세종시', city))
View(newfixed)

#보험인구 수
n_ins_pop<- ins_pop %>% mutate(city = gsub(" ", "", 시군구)) %>% 
  select(city, 인원수)

View(n_ins_pop)

merged_data <- left_join(newfixed, n_ins_pop, by = "city")
merged_data
# 정원 수 나누기 보험인구 수
fixed_num_result<- merged_data %>% mutate(정원_나누기_보험인구 = 시설_정원_수 / 인원수)

View(fixed_num_result)



#병의원 나누기 총인구
num_hos


#시각화 마지막
#예산 대비 전체 노인인구
#_>노인1명당 투입되는 복지 예산. 곱하기 10만원 생각하면됨

boxplot(money_hosp_result$예산_나누기_총인구, ylim= c(2.3, 6.8))

abline(h = 3.8, col = "red", lty = 1)
abline(h = 4.9, col = "red", lty = 1)
abline(h = 4.1, col = "red", lty = 1)
abline(h = 4.5, col = "red", lty = 1)
abline(h = 3.65, col = "red", lty = 1)

abline(h = 6.4, col = "blue", lty = 1)
abline(h = 6.7, col = "blue", lty = 1)
abline(h = 5.4, col = "blue", lty = 1)
abline(h = 5.1, col = "blue", lty = 1)
abline(h = 4.6, col = "blue", lty = 1)

#병의원수 대비 전체 노인인구
#노인 1,000명당 이용가능한 기관 수
boxplot(money_hosp_result$병의원_나누기_총인구)

abline(h = 13, col = "red", lty = 1)
abline(h = 9, col = "red", lty = 1)
abline(h = 11, col = "red", lty = 1)
abline(h = 7, col = "red", lty = 1)
abline(h = 9.9, col = "red", lty = 1)

abline(h = 6.3, col = "blue", lty = 1)
abline(h = 5.3, col = "blue", lty = 1)
abline(h = 6.4, col = "blue", lty = 1)
abline(h = 6.9, col = "blue", lty = 1)
abline(h = 5.5, col = "blue", lty = 1)

#정원 대비 보험대상 인구
#보험대상인구 1명당 이용할 수 있는 노인복지시설 자리

View(fixed_num_result)
boxplot(fixed_num_result$정원_나누기_보험인구, ylim=c(0, 2.1))

abline(h = 1.69, col = "red", lty = 1)
abline(h = 1.01, col = "red", lty = 1)
abline(h = 1.2, col = "red", lty = 1)
abline(h = 1.4, col = "red", lty = 1)
abline(h = 1.6, col = "red", lty = 1)

abline(h = 0.4, col = "blue", lty = 1)
abline(h = 0.3, col = "blue", lty = 1)
abline(h = 0.5, col = "blue", lty = 1)
abline(h = 0.52, col = "blue", lty = 1)
abline(h = 0.25, col = "blue", lty = 1)



head5_data<- c(1.69,1.01,  1.2, 1.4,  1.6)
tail5_data<- c(0.4, 0.3, 0.5, 0.52, 0.25)

ggplot(fixed_num_result) +
  geom_density(aes(x = 정원_나누기_보험인구), fill = "gray", alpha = 0.5) +  # 채우기 색상 및 투명도 조절
  theme_classic() +
  theme(axis.text = element_text(size = 12),  # 축 레이블 텍스트 크기 조절
        axis.title = element_text(size = 14, face = "bold"))+  # 축 제목 텍스트 크기 및 굵기 조절
  geom_vline(xintercept = head5_data, linetype = "solid", color = "red", size = 0.5)+
  geom_vline(xintercept = tail5_data, linetype = "solid", color = "blue", size = 0.5)
  
  