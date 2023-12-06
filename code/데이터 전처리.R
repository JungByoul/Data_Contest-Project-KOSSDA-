#빅데정 팀플

#1.급여(재가/시설)별, 지역별, 수요 대비 공급 분포

#대상: 서울, 광역시 등의 도심지들을 제외한 모든 지역(시/군/구 단위)
library('openxlsx')
library(dplyr)
#액셀에서 약간 손보고 불러오기
library(readxl)

#시군구별 65세이상 전체 건강보험 대상자
total_pop<- read_excel('total_pop.xlsx') #preproceseed data 폴더에 있음
total_pop<-  total_pop%>% filter(!is.na(건강보험_대상자))
View(total_pop)
#시군구별 노인복지 기관수
place <- read_excel('newplace.xlsx') #preproceseed data 폴더에 있음
place<- place %>% filter(!is.na(전체))
View(place)

#시군구별 장기요양보험 대상자(1~3등급) 수
in_pop <- read_excel('new_insurance_pop.xlsx') 
View(in_pop)

#등급별 총합 전처리
library(tidyr)
# NA 값을 이전 행의 도시 이름으로 채우기
in_pop_filled <- in_pop %>%
  fill(시군구)
View(in_pop_filled)
# 도시별로 그룹화하여 합을 계산
in_pop_sum <- in_pop_filled %>%
  group_by(시군구) %>%
  summarise(인원수 = sum(인원수))
View(in_pop_sum)
write.xlsx(in_pop_sum, 'in_pop_sum(시군구별_장기요양보험_대상자).xlsx') #preproceseed data 폴더에 있음

#테이블 조인
total_in_pop_join<- left_join(in_pop_sum, place, by='시군구')

#지역별 인구 1000명당 보험 대상자 대비 시설 비율(실재수요)
#해당지역에서 모든 노인인구 1000명씩 나눴을 때, 각 그룹이 이용할 수 있는 시설의 수 => 해당 시설 n개가 1000명을 커버한다.
total_in_pop_join<- total_in_pop_join %>% mutate(전체_비율 = 전체 / 인원수 * 1000,재가_비율 = 재가 / 인원수 * 1000,시설_비율 = 시설급여 / 인원수 * 1000)

#파일 내보내기
View(total_in_pop_join)
write.csv(total_in_pop_join, '보험 대상자 대비 시설비율(실재수요).csv')

#지역별 인구 100명당 전체 인구 대비 시설 비율(잠재수요)
total_pop_join <- left_join(total_pop, place, by = '시군구') %>%
  mutate(전체_비율 = 전체 / 건강보험_대상자*1000 ,재가_비율 = 재가 / 건강보험_대상자*1000 ,시설_비율 = 시설급여 / 건강보험_대상자*1000 )
View(total_pop_join)

#파일 내보내기
write.csv(total_in_pop_join, '전체 인구 대비 시설비율(잠재수요).csv')



#추가 분석
insure <- read_excel('insurance_ratio.xlsx')
View(insure)
insure<- insure %>% select(시군구, 전체_비율)
colnames(insure)<- c('시군구', 'ratio')
 
boxplot(insure$ratio, main= 'The number of welfare facilities')

ggplot(data = insure, aes(x = insure$ratio)) +
  geom_density(fill = "blue", alpha = 0.8) +
  theme_minimal()+
  xlab('The number of welfare facilities')



