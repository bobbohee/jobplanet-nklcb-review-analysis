# ------------------------------------------------------------------------------
# 0. 패키지 로드
# ------------------------------------------------------------------------------
library(readr)
library(dplyr)
library(stringr)
library(showtext)
library(tidytext)
library(ggwordcloud)


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
# 2. 폰트 설정
# ------------------------------------------------------------------------------
font_add_google(name = 'Black Han Sans', family = 'blackhansans')
showtext_auto()



# ------------------------------------------------------------------------------
# 3. 형태소 사전 설정
# ------------------------------------------------------------------------------
useNIADic()

user_dic <- data.frame(term = readLines('data/jobplanet_dic.txt'), tag = c('ncn'))
buildDictionary(ext_dic = 'NIAdic', user_dic = user_dic)



# ------------------------------------------------------------------------------
# 4. 데이터 불러오기
# ------------------------------------------------------------------------------
read_data <- function(company) {
  raw_review <- read_csv(sprintf('data/jobplanet_review_%s.csv', company), 
                         locale = locale('ko', encoding = 'utf-8'), 
                         col_names = T)
  return(raw_review)
}



# ------------------------------------------------------------------------------
# 5. 데이터 전처리
# ------------------------------------------------------------------------------
clean_data <- function(raw_review) {

# 5.1. 한글을 제외한 문자 제거 -------------------------------------------------
  reg_exp <- '[^가-힣]'
  review <- raw_review %>% mutate(pros = str_replace_all(pros, reg_exp, ' '),
                                  cons = str_replace_all(cons, reg_exp, ' '),
                                  pros = str_squish(pros),
                                  cons = str_squish(cons))
  
  
# 5.2. 단어 통일 ---------------------------------------------------------------
  review <- review %>% mutate(
    pros = pros %>% 
      str_replace_all('워라벨|워크라이프밸런스|워크앤라이프밸런스', '워라밸') %>% 
      str_replace_all('재택|재택 근무|자택근무|풀재택근무|풀재택', '재택근무') %>% 
      str_replace_all('월급|연봉|페이', '급여') %>% 
      str_replace_all('자율출퇴근|자율 출퇴근|자유 출퇴근|자율출근제|자출제|자율근무제|자율출퇴근|자율출퇴|자율근무|자율 근무|자유로운 출퇴근|원격근무|유연근무|유연근무제|유연 근무제|탄련근무|탄력근무제|선택근무제', '자율출퇴근제') %>% 
      str_replace_all('부바부|부 바이 부|부서 바이 부서|팀 바이 팀|팀바이팀', '팀바팀') %>% 
      str_replace_all('의사소통', '커뮤니케이션') %>%  
      str_replace_all('리프레쉬 휴가|리프레시 휴가', '리프레쉬휴가') %>%  
      str_replace_all('안식 휴가', '안식휴가') %>%  
      str_replace_all('비포괄|비포괄임금|비포괄 임금제', '비포괄임금제') %>%  
      str_replace_all('복지카드|복지 포인트|베네핏카드|쿠팡 포인트', '복지포인트') %>%  
      str_replace_all('실손보험|실비보험', '보험') %>%  
      str_replace_all('배울 점', '배울점') %>%  
      str_replace_all('특근비|야근수당', '야근비') %>%  
      str_replace_all('자기 계발|자기개발|자기 개발', '자기계발') %>%  
      str_replace_all('스톡', '스톡옵션') %>%  
      str_replace_all('코드 리뷰', '코드리뷰') %>%  
      str_replace_all('셔틀버스', '통근버스') %>%  
      str_replace_all('고인', '고인물') %>%  
      str_replace_all('연봉인상률|연봉 인상율|연봉 인상률|연봉상승률|연봉 상승률|급여 인상률|급여 인상', '연봉인상') %>%  
      str_replace_all('네임밸류', '네임벨류') %>%  
      str_replace_all('월급 루팡|월루|월급좀도둑', '월급루팡') %>%  
      str_replace_all('반차|월차|연차', '휴가') %>%  
      str_replace_all('영어이름|영어 이름', '닉네임') %>% 
      str_replace_all('도서비|도서지원비|도서구입지원', '도서구입비') %>%  
      str_replace_all('개인 주의', '개인주의') %>%  
      str_replace_all('상여|떡값', '상여급') %>%  
      str_replace_all('인센티브|인센', '성과급'))
  
  
# 5.3. 명사를 기준으로 토큰화 --------------------------------------------------
  review_pros <- review %>% unnest_tokens(input = pros, output = word, token = extractNoun)
  review_cons <- review %>% unnest_tokens(input = cons, output = word, token = extractNoun)
  
  
# 5.4. 단어 빈도 구하기 + 2글자 이상만 남기기 ----------------------------------
  review_pros <- review_pros %>% count(word, sort = T) %>% filter(str_count(word) > 1)
  review_cons <- review_cons %>% count(word, sort = T) %>% filter(str_count(word) > 1)
  
  return(list(pros = review_pros, cons = review_cons))
}



# ------------------------------------------------------------------------------
# 6. 워드 클라우드 생성
# ------------------------------------------------------------------------------

# 6.1. 장점 워드 클라우드 ------------------------------------------------------
create_wordcloud_pros <- function(review) {
  ggplot(review, aes(label = word, size = n, col = n)) +
    geom_text_wordcloud(seed = 1234, family = 'blackhansans') + 
    scale_radius(limits = c(10, NA), range = c(5, 50)) +
    scale_color_gradient(low = '#80c8ff', high = '#004880') +
    theme_minimal()
}


# 6.2. 단점 워드 클라우드 ------------------------------------------------------
create_wordcloud_cons <- function(review) {
  ggplot(review, aes(label = word, size = n, col = n)) +
    geom_text_wordcloud(seed = 1234, family = 'blackhansans') + 
    scale_radius(limits = c(10, NA), range = c(5, 50)) +
    scale_color_gradient(low = '#ffb3b3', high = '#b30000') +
    theme_minimal()
}



# ------------------------------------------------------------------------------
# 7. 워드 클라우드 함수
# ------------------------------------------------------------------------------
run_wordcloud <- function(company) {
  raw_review <- read_data(company)
  review <- clean_data(raw_review)
  create_wordcloud_pros(review$pros)
  create_wordcloud_cons(review$cons)
}



# ------------------------------------------------------------------------------
# 7. 워드 클라우드 실행
# ------------------------------------------------------------------------------
run_wordcloud('naver')
run_wordcloud('kakao')
run_wordcloud('line')
run_wordcloud('coupang')
run_wordcloud('baemin')
