#빅데정 팀플

#1.급여(재가/시설)별, 지역별, 수요 대비 공급 분포

#대상: 서울, 광역시 등의 도심지들을 제외한 모든 지역(시/군/구 단위)
library(dplyr)
#액셀에서 약간 손보고 불러오기
library(readxl)

#시군구별 65세이상 전체 건강보험 대상자
total_pop<- read_excel('total_pop.xlsx')
total_pop<-  total_pop%>% filter(!is.na(건강보험_대상자))
View(total_pop)
#시군구별 노인복지 기관수
place <- read_excel('newplace.xlsx')
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

#테이블 조인
joindf<- left_join(in_pop_sum, place, by='시군구')
View(joindf)

#지역별 인구 1000명당 보험 대상자 대비 시설 비율(실재수요)
join_new<- joindf %>% mutate(전체_비율 = 전체 / 인원수 * 1000,재가_비율 = 재가 / 인원수 * 1000,시설_비율 = 시설급여 / 인원수 * 1000)


#지역별 인구 1000명당 전체 인구 대비 시설 비율(잠재수요)
total_pop_join <- left_join(total_pop, place, by = '시군구') %>%
  mutate(전체_비율 = 전체 / 건강보험_대상자 * 1000,재가_비율 = 재가 / 건강보험_대상자 * 1000,시설_비율 = 시설급여 / 건강보험_대상자 * 1000)
View(total_pop_join)
