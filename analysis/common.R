# ------------------------------------------------------------------------------
# 0. 패키지 로드
# ------------------------------------------------------------------------------
library(readr)
library(dplyr)
library(stringr)
library(tidytext)


# ------------------------------------------------------------------------------
# 1. KoNLP 한글 형태소 분석 패키지 로드
# ------------------------------------------------------------------------------

# 1.1. 자바와 rJava 패키지 설치 ------------------------------------------------
library(multilinguer)
library(rJava)

# install_jdk()


# 1.2. KoNLP 패키지 설치 -------------------------------------------------------
# install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = 'binary')

remotes::install_github('haven-jeon/KoNLP',
                        force = T, 
                        upgrade = 'never',
                        INSTALL_opts = c('--no-multiarch'))

library(KoNLP)



# ------------------------------------------------------------------------------
# 2. 형태소 사전 설정
# ------------------------------------------------------------------------------
useNIADic()

user_dic <- data.frame(term = readLines('data/jobplanet_dic.txt'), tag = c('ncn'))
buildDictionary(ext_dic = 'NIAdic', user_dic = user_dic)



# ------------------------------------------------------------------------------
# 3. 데이터 불러오기
# ------------------------------------------------------------------------------

# 3.1. 데이터 불러오기 ---------------------------------------------------------
read_data <- function(company_name) {
  review <- read_csv(sprintf('data/jobplanet_review_%s.csv', company_name), 
                     locale = locale('ko', encoding = 'utf-8'), 
                     col_names = T)
  return(review)
}


# 3.2. 데이터를 불러오고, row를 합치기 -----------------------------------------
read_combine_data <- function(company_name) {
  review <- read_data(company_name)
  
  review <- review %>%
    summarise(
      pros = paste(pros, collapse = ' '),
      cons = paste(cons, collapse = ' ')
    )
  
  review <- raw_review %>% 
    mutate(company = company_name)
}



# ------------------------------------------------------------------------------
# 4. 데이터 전처리
# ------------------------------------------------------------------------------
clean_data <- function(review, col_name) {
  
# 4.1. 한글을 제외한 문자 제거 -------------------------------------------------
  reg_exp <- '[^가-힣]'
  review <- review %>% mutate(!!sym(col_name) := str_replace_all(!!sym(col_name), reg_exp, ' '),
                                  !!sym(col_name) := str_squish(!!sym(col_name)))
  
  
# 4.2. 단어 통일 ---------------------------------------------------------------
  review <- review %>% mutate(
    !!sym(col_name) := !!sym(col_name) %>% 
      str_replace_all('개인 주의', '개인주의') %>%
      str_replace_all('고인', '고인물') %>%  
      str_replace_all('네임밸류|네임 밸류|네임 벨류', '네임벨류') %>%  
      str_replace_all('도서비|도서지원비|도서구입지원', '도서구입비') %>%  
      str_replace_all('리프레쉬 휴가|리프레시 휴가', '리프레쉬휴가') %>%  
      str_replace_all('반차|월차|연차', '휴가') %>%  
      str_replace_all('배민', '배달의민족') %>%  
      str_replace_all('배울 점', '배울점') %>%  
      str_replace_all('복지카드|복지 포인트|베네핏카드|쿠팡 포인트', '복지포인트') %>%  
      str_replace_all('부바부|부 바이 부|부서 바이 부서|팀 바이 팀|팀바이팀', '팀바팀') %>% 
      str_replace_all('비포괄|비포괄임금|비포괄 임금제', '비포괄임금제') %>%  
      str_replace_all('상여|떡값', '상여급') %>%  
      str_replace_all('셔틀버스', '통근버스') %>%  
      str_replace_all('스톡', '스톡옵션') %>%  
      str_replace_all('실손보험|실비보험', '보험') %>%  
      str_replace_all('안식 휴가', '안식휴가') %>%  
      str_replace_all('연봉인상률|연봉 인상율|연봉 인상률|연봉상승률|연봉 상승률|급여 인상률|급여 인상', '연봉인상') %>%  
      str_replace_all('영어이름|영어 이름', '닉네임') %>% 
      str_replace_all('워라벨|워크라이프밸런스|워크앤라이프밸런스', '워라밸') %>% 
      str_replace_all('월급|연봉|페이', '급여') %>% 
      str_replace_all('월급 루팡|월루|월급좀도둑', '월급루팡') %>%  
      str_replace_all('의사소통', '커뮤니케이션') %>%  
      str_replace_all('인센티브|인센', '성과급') %>% 
      str_replace_all('자기 개발', '자기개발') %>%  
      str_replace_all('자기 계발', '자기계발') %>%  
      str_replace_all('자율출퇴근|자율 출퇴근|자유 출퇴근|자율출근제|자출제|자율근무제|자율출퇴근|자율출퇴|자율근무|자율 근무|자유로운 출퇴근|원격근무|유연근무|유연근무제|유연 근무제|탄련근무|탄력근무제|선택근무제', '자율출퇴근제') %>% 
      str_replace_all('재택|재택 근무|자택근무|풀재택근무|풀재택', '재택근무') %>% 
      str_replace_all('중국인|인도인|미국인', '외국인') %>% 
      str_replace_all('코드 리뷰', '코드리뷰') %>%  
      str_replace_all('특근비|야근수당', '야근비')  
  )
  
  
# 4.3. 불용어 제거 -------------------------------------------------------------
  review <- review %>% mutate(
    !!sym(col_name) := !!sym(col_name) %>% 
      str_replace_all('극강|대한민국|소소한|준수|보통|대상|특면|좋은거|채널|감성|분단|자잘|년|대한민국|소소한', '') %>% 
      str_replace_all('로운|들이|하기|가지|등등|이긴|이지만|이겠지만|겠지만|지만|라고|이긴한데|이나|이다보니|이니|인거|하지만|들과|극강|지려|을|를|하다|하는건', ''))
  
  
# 4.4. 명사를 기준으로 토큰화 --------------------------------------------------
  review <- review %>% unnest_tokens(input = !!sym(col_name), output = word, token = extractNoun)

  return(review)
}



# ------------------------------------------------------------------------------
# 5. 데이터 전처리가 완료된 데이터 불러오기
# ------------------------------------------------------------------------------
get_data <- function(company_name, col_name) {
  review <- read_data(company_name)
  review <- clean_data(review, col_name)
  return(review)
}
