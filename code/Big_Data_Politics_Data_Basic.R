### R 은  계산기이다.(calculator)
30*50
300+20

### assign the value x (x라고 하는 변수에 값을 설정하기)
x<-34  # 34라는 값을 x에 저장. =도 가능
## 변수/객체: 어떠한 값을 저장할 수 있는 저장소
## 알파벳 문자값을 이용해서 변수를 만들 수 있다.
## 변수의 경우 첫 이름은 무조건 알파벳 문자로 시작해야 한다.
## a1은 가능한 변수이름
## 1a는 불가능
## 특수문자는 선택적으로 변수이름에 사용될 수 있다. 보통 사용되는 특수문자는 언더바_
## R은 case sensitive: 소문자 x 와 대문자 X를 구분한다. 
x=34  # 34를 x라는 변수에 저장 (<-와 =을 동시에 사용할 수 있다.)
x23->x  #결과가 어떤지 확인해보자.
print(x) # x변수의 아웃풋 출력
#print 함수 없이도 출력 가능
x #위의 결과와 비교해보자.
y<-3 # 3을 x에 저장 (숫자로 된 변수)

## 변수를 조합해서 새로운 함수 만들기
x <- c(3, 4, 6)
y <- c(2, 7, 8)
addition<-x+y 
substraction<-x-y
division<-x/y
multiplication<-x*y
exponentiation<-x^y #지수


### 결과 확인하기 (Check the outcome)
addition
substraction
division
multiplication
exponentiation

## 변수 만들어보기
## 26명의 학생
## (2학년, 3학년, 1학년, ....., 4학년) 학년 변수
class <- c(2, 3, 1, 4, 5)
## (남, 남, 여, 여, 여, 남, 여, .... 남) 성별 변수
gender <- c(1, 0, 1, 0, 0)
gender_str <- c('M', 'F')
gender_str

## (정치외교, 정치외교, 행정, EICC, ...., 노어과) 전공 변수

## 이후 2차원적 데이터에 대해서 학습할 예정: 매트릭스와 데이터
## 학년+성별+전공+....

## numeric (숫자), character (문자), logical (논리) 
## factor: 분석가능한 범주형 변수
## R에서는 c (combine) 함수 (function)을 이용하서 각각의 값을 하나의 변수로 만들 수 있다.
z<-x+y
z
sum(z) # 변수의 합


## x vector의 세 번째 값을 선택 [ ] 
x[3]
x_3<-x[3]
x_3
## 두 번째와 세 번째 요소 선택
x[2:3] 
x[-1] #첫번째를 제외하고 전부임
x_23<-x[2:3]
x5<-c(1,2,3,4,5,10,13,15)
x5[2:8]


## 문자로 된 변수 만들기 "" (큰 따옴표  사용)

# R에서 문자로 된 변수는 문자형(character) 변수라고 부른다. 
x_cha<-"Smith"
x_cha
x_cha<-c("Smith", "John", "Adams")
x_cha

## class 기능을 사용해서 각 변수의 class를 확인
x_logical # 논리형 변수
x_logical<-c(TRUE, FALSE, FALSE)
x_logical

## factor형 변수
## 문자형 변수 중에서 category를 나누는 변수
## 먼저 문자형 변수를 만들어보자
days_character<-c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
days2<-as.factor(days_character)
#중복된 것 제외함.
days_character
days2 # 자동 오름차순

## 어떤 자료형을 논리형으로 변환시킬 때에는 as.logical() 함수를 이용

## 어떤 자료형을 숫자형으로 변환시킬 때에는 as.numeric() 함수를 이용

## 어떤 자료형을 문자형으로 변환시킬 때에는 as.character() 함수를 이용


### R 의 논리비교 연산자(Logical comparison operators) 
## < for less than (~보다 작은)
## > for greater than (~보다 큰)
## <= for less than or equal to (~보다 작거나 같은 )
## >= for greater than or equal to (~보다 크거나 같은)
## == for equal to each other (서로가 정확히 일치하는 )
## = R 에서 = 은 특정한 값을 변수에 저장할 때 사용한다. 
## != not equal to each other (서로가 다른)

x <- c(3, 4, 6)
y <- c(2, 7, 8)

## x의 합이 y의 합보다 큰 지 확인해보자. 
sum(x)>sum(y)

# some more calculation
# maximum: find the maximum
# minimum: find the minimum
# average: mean()
# median: median()
# standard deviation: sd()


max(1, 4, 5)
max(x)
min(1,4,5)
min(x)
mean(x)
median(x)
sd(x)

## okay but what if we have a missing value (NA)
#결측치가 포함된 것은, 통계적 수치들잉 다 NA로 나옴.
x1<-c(3,4,5,6,7,8, NA)
mean(x1)
sd(x1)

#이럴 때 사용할 수 있는 조건
## R cannot calculate the mathematical function with a missing value
## You ca use the na.rm option, make it "na.rm=TRUE"
mean(x1, na.rm=TRUE)
sd(x1, na.rm=TRUE)

## R에서는 개인 개발자들이 종종 유용한 함수와 program을 만든다.
## 이러한 함수, 프로그램, 데이터의 집합을 패키지
# 각각의 package를 설치해야 R에서 특정 함수나 프로그램을 사용할 수 있고, 
# 이 때 install.packages 명령어를 사용
## library() package를 설치하는 것으로 끝나지 않는다. 항상 library명령어를 이용해서 
# 각각의 package를 load 해 주어야 한다. 

## ggplot2 package의 설치
install.packages("ggplot2")
## help file
help(package="ggplot2")
## ggplot2의 qplot함수를 사용해보기
library(ggplot2)
help(qplot)



## R은 또한 각각의 function에 대한 예제를 보여준다.
example(qplot)

# 현재 R에 설치된 모든 packages들을 보고 싶다면?
search()

# qplot 활용, mpg 데이터 활용 (ggplot2 package에 있음)
## mpg = mile per gallon
##mile = 약1.6km
##gallon = 약 3.7L
qplot(data=mpg, x=hwy) #highway mile
qplot(data=mpg, x=cty) #city mile
qplot(data=mpg, x=drv, y=hwy) # 
qplot(data=mpg, x=drv, y=hwy, geom="line")
qplot(data=mpg, x=drv, y=hwy, geom="boxplot")
qplot(data=mpg, x=drv,y=hwy, geom="boxplot", colour=drv)
#boxplot: 평균(또는 중앙값)과 분포를 같이 보여줌

help(view)

##view()
##csv 파일 넣었을 때 보여줌. 자세한건 다음 시간




