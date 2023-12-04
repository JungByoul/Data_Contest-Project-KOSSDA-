#)은 목차
#0)사용데이터 모두 불러오기
#1)사용데이터들 행 합치기
#2) 전처리 및 토큰화

#>은 기준
#1> 보험인구 1000명당 시설수 head3 토픽모델링
#2> 보험인구 1000명당 시설수 tail3 토픽모델링
#3> 총인구 1000명당 시설수 head3 토픽모델링
#4> 총인구 1000명당 시설수 tail3 토픽모델링

library(readxl)
library(dplyr)
library(stringr)
library(textclean)
library(tidytext)
library(KoNLP)
useNIADic()

#1> 보험인구 100명당 시설수 head3 
#문정복_시흥시_갑구(1-1)
kk_sh_moon <- read_excel("문정복.xlsx", sheet = '문정복_01') %>% 
  select(...15)
#조정식_시흥시_을구(1-2)
kk_yj_jo <- read_excel("조정식.xlsx", sheet = '조정식_01') %>% 
  select(...15)
#윤준병_정읍시*고창군(2)
jb_je_yoon <- read_excel("윤준병.xlsx", sheet = '윤준병_01') %>% 
  select(...15)
#김원이_목포시(3)
jn_mp_kim <- read_excel("김원이.xlsx", sheet = '김원이_01') %>% 
  select(...15)

#2> 보험인구 1000명당 시설수 tail3
#이양수_속초시·인제군·고성군·양양군(1)(고성군)
kw_yj_ks_lee <- read_excel("이양수.xlsx", sheet = '이양수_01') %>% 
  select(...15)
#이양수_속초시·인제군·고성군·양양군(2)(인제군)
kw_yj_ks_lee

#이철규_동해시*태백시*삼척시*정선군(3)(태백시)
kw_tb_lee <- read_excel("이철규.xlsx", sheet = '이철규_01') %>% 
  select(...15)

#3> 총인구 1000명당 시설수 head3
#정성호_양주시(1)
kk_yj_jung <- read_excel("정성호.xlsx", sheet = '정성호_01') %>% 
  select(...15)
#홍정민_고양시 병(2)
kk_ky_is_hong <- read_excel("홍정민.xlsx", sheet = '홍정민_01') %>% 
  select(...15)
#김성원_동두천시*연천군(3)
kk_ddch_kim <- read_excel("김성원.xlsx", sheet = '김성원_01') %>% 
  select(...15)

#4> 총인구 1000명당 시설수 tail3 
#이양수_속초시·인제군·고성군·양양군(1)(인제군)
kw_yj_ks_lee
#정점식_통영시*고성군(2)
kn_ks_jung<- read_excel("정점식.xlsx", sheet="정점식_01") %>% 
  select(...15)

#이양수_속초시·인제군·고성군·양양군(3)(고성군)
kw_yj_ks_lee

#1)사용데이터들 행 합치기
#1> 보험인구 1000명당 시설수 head3
in_pop_head3 <- bind_rows(kk_sh_moon, kk_yj_jo, jb_je_yoon, jn_mp_kim )

#2> 보험인구 1000명당 시설수 tail3 
in_pop_tail3 <- bind_rows(kw_yj_ks_lee, kw_tb_lee)

#3> 총인구 1000명당 시설수 head3 
all_pop_head3 <- bind_rows(kk_yj_jung, kk_ky_is_hong, kk_ddch_kim)

#4> 총인구 1000명당 시설수 tail3 
all_pop_head3 <- bind_rows(kw_yj_ks_lee, kn_ks_jung)

View(in_pop_head3)
#2) 전처리 및 토큰화
#(보험인구 1000명당 시설수 head3 먼저)
in_pop_head3<- in_pop_head3 %>% as_tibble()
in_pop_tail3 <- in_pop_tail3 %>% as_tibble()

in_pop_head3_pr<- in_pop_head3 %>%
  mutate(...1 = str_replace_all(...1, "[^가-힣]", " "),
         ...1 = str_squish(...1))
in_pop_tail3_pr <- in_pop_tail3 %>% 
  mutate(...1 = str_replace_all(...1, "[^가-힣]", " "),
         ...1 = str_squish(...1))


in_pop_head3_token<- in_pop_head3_pr%>% 
  unnest_tokens(
    input = ...1,
    output = word,
    token = extractNoun,
    drop = F
  )

in_pop_tail3_token <- in_pop_tail3_pr %>% 
  unnest_tokens(
    input = ...1,
    output = word,
    token = extractNoun,
    drop = F
  )


View(in_pop_head3_token)

# 불용어 제거 및 1음절로 된 단어 제거
stopword <- c("질의", "요청", "의사", "있습니", "하나", "말씀", "때문", "문제", "사안", "정말", "너무", "예", "들이", "하다", "하게", "하면", "해서", "이번", "하네",
              "해요", "이것", "니들", "하기", "하지", "한거", "해주",
              "그것", "어디", "여기", "까지", "이거", "하신", "만큼",
              "첫째", "덕분", "단지", "사이", "셋째", "정도", "여기", 
              "하고", "이름", "당시", "다음", "하신", "차례", "위대",
              "이야기",
              "그때", "번째", "이후", "그다음", "답변", "가지", "그렇습니", "거지", "확인","경우", "의원님", "주십시", "않습니", "하게", "부장관", "이상", "위원회", "하기", "위원회", "그것", "페이지", "발의", "다음", "하면", "들이", "얘기", " 정도", "규정", "하시", "이것", "진행", "오늘", "설치", "경우", "과정", "선정", "저장용량", "동안", "명확", "저희", "관리", "거기", "사항", "규정하", "정리", "일반", "문제", "그렇습니", "이상", "일부", "요청", "관련", "내용", "이유", "발언", "해서", "우리", "위원", "하신", "하겠습니", "결과", "부분", "말씀", "생각", "확인", "얘기", "운영", "위원", "그것", "를이", "사무", "진행", "주제관", "심사", "해서", "관련", "하신", "여담", "감사", "간사", "있습니", "만약", "시간", "년도", "조가", "의원", "사실", "중요")



in_pop_head3_tokens<- in_pop_head3_token %>% 
  filter(!word %in% stopword) %>% 
  filter(str_count(word)>1) %>% 
  select(word)

in_pop_tail3_tokens<-  in_pop_tail3_token %>% 
  filter(!word %in% stopword) %>% 
  filter(str_count(word)>1) %>% 
  select(word)

View(in_pop_head3_tokens)

#3)LDA모델 생성

#각 단어 빈도표 생성 및 빈도100이상 제거
in_pop_head3_count <- in_pop_head3_tokens %>% 
  count(word, sort=T) %>% 
  filter(n<=100)
in_pop_head3_count$id = rownames(in_pop_head3_count)
View(in_pop_head3_count) 

in_pop_tail3_count <- in_pop_tail3_tokens %>% 
  count(word, sort=T) %>% 
  filter(n<=100)
in_pop_tail3_count$id = rownames(in_pop_tail3_count)


#DTM 생성
dtm_in_pop_head3_count <- in_pop_head3_count%>% 
  cast_dtm(document = id, term = word, value = n)
library(tm)
as.matrix(dtm_in_pop_head3_count[1:15, 1:15])

dtm_in_pop_tail3_count <- in_pop_tail3_count%>% 
  cast_dtm(document = id, term = word, value = n)
library(tm)
as.matrix(dtm_in_pop_tail3_count[1:15, 1:15])

#LDA 모델 생성
library(topicmodels)
lda_model_in_pop_head3<- LDA(dtm_in_pop_head3_count,
                     k = 10,
                     method = 'Gibbs',
                     control = list(seed = 123)
)
lda_model_in_pop_head3

glimpse(lda_model_in_pop_head3)


lda_model_in_pop_tail3<- LDA(dtm_in_pop_tail3_count,
                             k = 10,
                             method = 'Gibbs',
                             control = list(seed = 123)
)
#beta값 활용하여 토픽별 주요 단어 분석
library(tidytext)
term_topic<- tidy(lda_model_in_pop_head3, matrix = 'beta')
term_topic
term_topic %>% count(topic) #토픽별로 22,911 행으로 되어 있음

term_topic_tail<- tidy(lda_model_in_pop_tail3, matrix = 'beta')
term_topic
term_topic_tail %>% count(topic) #토픽별로 22,911 행으로 되어 있음


#모든 토픽에서 등장 확률 높은 상위 15개 단어 추출
terms(lda_model_in_pop_head3, 15) %>% data.frame()

#추출된 주요 단어 시각화하기
#토픽별 beta값 가장 높은 단어 15개 추출
lda_model_in_pop_head3_topic <- term_topic %>% 
  group_by(topic) %>% 
  slice_max(beta, n=15, with_ties = F)

lda_model_in_pop_tail3_topic <- term_topic_tail %>% 
  group_by(topic) %>% 
  slice_max(beta, n=15, with_ties = F)

#막대그래프 생성
library(scales)
library(ggplot2)
ggplot(lda_model_in_pop_head3_topic,
       aes(x = reorder_within(term, beta, topic),
           y = beta,
           fill = factor(topic))) +
  geom_col(show.legend = F)+
  facet_wrap(~topic, scales='free', ncol = 4)+
  coord_flip()+
  scale_x_reordered()+
  scale_y_continuous(n.breaks = 4,
                     labels = number_format(accuracy = 0.1))+
  labs(x = NULL)+
  theme(text = element_text(family = 'nanumgothic'))


ggplot(lda_model_in_pop_tail3_topic,
       aes(x = reorder_within(term, beta, topic),
           y = beta,
           fill = factor(topic))) +
  geom_col(show.legend = F)+
  facet_wrap(~topic, scales='free', ncol = 4)+
  coord_flip()+
  scale_x_reordered()+
  scale_y_continuous(n.breaks = 4,
                     labels = number_format(accuracy = 0.1))+
  labs(x = NULL)+
  theme(text = element_text(family = 'nanumgothic'))


#아래는 함수작성해서 구하기.
library(readxl)
library(dplyr)
library(stringr)
library(textclean)
library(tidytext)
library(KoNLP)
useNIADic()

# 함수 정의: 데이터 불러오기
read_data <- function(file_path, sheet_name) {
  data <- read_excel(file_path, sheet = sheet_name) %>% 
    select(...15)  # 적절한 열을 선택하도록 수정
  return(data)
}

# 함수 정의: 데이터 전처리 및 토큰화
preprocess_data <- function(data) {
  data <- data %>% as_tibble()
  
  data_processed <- data %>%
    mutate(...15 = str_replace_all(...15, "[^가-힣]", " "),
           ...15 = str_squish(...15))
  
  data_tokenized <- data_processed %>% 
    unnest_tokens(
      input = ...15,
      output = word,
      token = extractNoun,
      drop = FALSE
    ) %>% filter(str_count(word)>1)%>% 
    filter(!word %in% stopword)
  
  return(data_tokenized)
}

# 함수 정의: LDA 모델 생성
create_lda_model <- function(data_tokenized) {
  data_count <- data_tokenized %>% 
    count(word, sort = T) %>% 
    filter(n <= 100)
  
  data_count$id <- rownames(data_count)
  
  dtm_data_count <- data_count %>% 
    cast_dtm(document = id, term = word, value = n)
  
  lda_model_data <- LDA(dtm_data_count,
                        k = 10,
                        method = 'Gibbs',
                        control = list(seed = 123))
  
  return(lda_model_data)
}

# 함수 정의: 토픽별 주요 단어 분석 및 시각화
visualize_topic_words <- function(lda_model_data) {
  term_topic <- tidy(lda_model_data, matrix = 'beta')
  
  lda_model_topic_words <- term_topic %>% 
    group_by(topic) %>% 
    slice_max(beta, n = 15, with_ties = FALSE)
  
  p<- ggplot(lda_model_topic_words,
         aes(x = reorder_within(term, beta, topic),
             y = beta,
             fill = factor(topic))) +
    geom_col(show.legend = FALSE)+
    facet_wrap(~topic, scales = 'free', ncol = 4)+
    coord_flip()+
    scale_x_reordered()+
    scale_y_continuous(n.breaks = 4,
                       labels = scales::number_format(accuracy = 0.1))+
    labs(x = NULL)+
    theme(text = element_text(family = 'nanumgothic'))
  print(p)
}

# 데이터 파일 리스트
file_list <- c("문정복.xlsx", "조정식.xlsx", "윤준병.xlsx", "김원이.xlsx", 
               "이양수.xlsx", "이철규.xlsx", "정성호.xlsx", "홍정민.xlsx", 
               "김성원.xlsx", "정점식.xlsx")

# 시트명 리스트
sheet_list <- c('문정복_01', '조정식_01', '윤준병_01', '김원이_01', 
                '이양수_01', '이철규_01', '정성호_01', '홍정민_01', 
                '김성원_01', '정점식_01')

# 반복문을 통한 데이터 처리
for (i in seq_along(file_list)) {
  # 데이터 불러오기
  data <- read_data(file_list[i], sheet_list[i])
  
  # 데이터 전처리 및 토큰화
  data_tokenized <- preprocess_data(data)
  
  # LDA 모델 생성
  lda_model_data <- create_lda_model(data_tokenized)
  
  # 토픽별 주요 단어 시각화
  visualize_topic_words(lda_model_data)
}
