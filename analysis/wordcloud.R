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
# 2. 워드 클라우드 생성
# ------------------------------------------------------------------------------
create_wordcloud <- function(review, low_color, high_color) {
  ggplot(review, aes(label = word, size = n, col = n)) +
    geom_text_wordcloud(seed = 1234, family = 'blackhansans') + 
    scale_radius(limits = c(10, NA), range = c(5, 40)) +
    scale_color_gradient(low = low_color, high = high_color) +
    theme_minimal()
}

create_wordcloud_pros <- function(company_name) {
  review <- get_data(company_name, 'pros')
  review <- review %>% count(word, sort = T) %>% filter(str_count(word) > 1)
  create_wordcloud(review, '#80c8ff', '#004880')
}

create_wordcloud_cons <- function(company_name) {
  review <- get_data(company_name, 'cons')
  review <- review %>% count(word, sort = T) %>% filter(str_count(word) > 1)
  create_wordcloud(review, '#ffb3b3', '#b30000')
}


# ------------------------------------------------------------------------------
# 3. 워드 클라우드 실행
# ------------------------------------------------------------------------------

# 3.1. 장점 워드 클라우드 ------------------------------------------------------
create_wordcloud_pros('naver')
create_wordcloud_pros('kakao')
create_wordcloud_pros('line')
create_wordcloud_pros('coupang')
create_wordcloud_pros('baemin')


# 3.2. 단점 워드 클라우드 ------------------------------------------------------
create_wordcloud_cons('naver')
create_wordcloud_cons('kakao')
create_wordcloud_cons('line')
create_wordcloud_cons('coupang')
create_wordcloud_cons('baemin')
