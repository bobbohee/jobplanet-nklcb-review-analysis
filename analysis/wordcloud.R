# ------------------------------------------------------------------------------
# 0. 패키지 로드
# ------------------------------------------------------------------------------
library(ggwordcloud)
library(showtext)



# ------------------------------------------------------------------------------
# 1. 워드 클라우드 폰트 설정
# ------------------------------------------------------------------------------
font_add_google(name = 'Black Han Sans', family = 'blackhansans')
showtext_auto()



# ------------------------------------------------------------------------------
# 2. 워드 클라우드 데이터 불러오기
# ------------------------------------------------------------------------------
get_data <- function(company_name, col_name) {
  review <- read_data(company_name)
  review <- clean_data(review, col_name)
  review <- review %>% count(word, sort = T) %>% filter(str_count(word) > 1)
  return(review)
}



# ------------------------------------------------------------------------------
# 3. 워드 클라우드 생성
# ------------------------------------------------------------------------------
create_wordcloud <- function(review, low_color, high_color) {
  ggplot(review, aes(label = word, size = n, col = n)) +
    geom_text_wordcloud(seed = 1234, family = 'blackhansans') + 
    scale_radius(limits = c(10, NA), range = c(5, 40)) +
    scale_color_gradient(low = low_color, high = high_color) +
    theme_minimal()
}


wordcloud_pros <- function(company_name) {
  review <- get_data(company_name, 'pros')
  create_wordcloud(review, '#80c8ff', '#004880')
}


wordcloud_cons <- function(company_name) {
  review <- get_data(company_name, 'cons')
  create_wordcloud(review, '#ffb3b3', '#b30000')
}



# ------------------------------------------------------------------------------
# 4. 워드 클라우드 실행
# ------------------------------------------------------------------------------

# 4.1. 장점 워드 클라우드 ------------------------------------------------------
wordcloud_pros('naver')
wordcloud_pros('kakao')
wordcloud_pros('line')
wordcloud_pros('coupang')
wordcloud_pros('baemin')


# 4.2. 단점 워드 클라우드 ------------------------------------------------------
wordcloud_cons('naver')
wordcloud_cons('kakao')
wordcloud_cons('line')
wordcloud_cons('coupang')
wordcloud_cons('baemin')
