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
# 2. 폰트 설정
# ------------------------------------------------------------------------------
library(showtext)

font_add_google(name = 'Black Han Sans', family = 'blackhansans')
showtext_auto()



# ------------------------------------------------------------------------------
# 3. 형태소 사전 설정
# ------------------------------------------------------------------------------
useNIADic()

user_dic <- data.frame(term = readLines("data/jobplanet_dic_naver.txt"), tag = c('ncn'))
buildDictionary(ext_dic = 'NIAdic', user_dic = user_dic)



# ------------------------------------------------------------------------------
# 4. 데이터 불러오기
# ------------------------------------------------------------------------------
raw_review <- read_csv('data/jobplanet_review_naver.csv', 
                       locale = locale('ko', encoding = 'utf-8'), 
                       col_names = T)



# ------------------------------------------------------------------------------
# 5. 데이터 전처리
# ------------------------------------------------------------------------------

# 5.1. 한글을 제외한 문자 제거 -------------------------------------------------
reg_exp <- '[^가-힣]'
review <- raw_review %>% mutate(pros = str_replace_all(pros, reg_exp, ' '),
                                cons = str_replace_all(cons, reg_exp, ' '),
                                pros = str_squish(pros),
                                cons = str_squish(cons))


# 5.2. 단어 통일 ---------------------------------------------------------------
review <- review %>% mutate(
  pros = pros %>% 
    str_replace_all('워라벨|워크라이프밸런스', '워라밸') %>% 
    str_replace_all('재택|자택근무', '재택근무') %>% 
    str_replace_all('월급|연봉|페이', '급여') %>% 
    str_replace_all('자율출퇴근|자율 출퇴근|자율출근제|자출제|자율근주제|자율출퇴근|자율출퇴', '자율출퇴근제') %>% 
    str_replace_all('부바부|부서 바이 부서|팀 바이 팀|팀바이팀', '팀바팀') %>% 
    str_replace_all('인센티브|인센', '성과급'))


# 5.3. 명사를 기준으로 토큰화 --------------------------------------------------
review_pros <- review %>% unnest_tokens(input = pros, output = word, token = extractNoun)
review_cons <- review %>% unnest_tokens(input = cons, output = word, token = extractNoun)


# 5.4. 단어 빈도 구하기 + 2글자 이상만 남기기 ----------------------------------
review_pros <- review_pros %>% count(word, sort = T) %>% filter(str_count(word) > 1)
review_cons <- review_cons %>% count(word, sort = T) %>% filter(str_count(word) > 1)



# ------------------------------------------------------------------------------
# 6. 워드 클라우드 생성
# ------------------------------------------------------------------------------
library(ggwordcloud)


# 6.1. 장점 워드 클라우드 ------------------------------------------------------
ggplot(review_pros, aes(label = word, size = n, col = n)) +
  geom_text_wordcloud(seed = 1234, family = 'blackhansans') + 
  scale_radius(limits = c(10, NA), range = c(5, 50)) +
  scale_color_gradient(low = '#80c8ff', high = '#004880') +
  theme_minimal()


# 6.2. 단점 워드 클라우드 ------------------------------------------------------
ggplot(review_cons, aes(label = word, size = n, col = n)) +
  geom_text_wordcloud(seed = 1234, family = 'blackhansans') + 
  scale_radius(limits = c(10, NA), range = c(5, 50)) +
  scale_color_gradient(low = '#ffb3b3', high = '#b30000') +
  theme_minimal()
