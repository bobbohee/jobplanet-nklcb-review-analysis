# ------------------------------------------------------------------------------
# 0. 패키지 로드
# ------------------------------------------------------------------------------
library(ggplot2)
library(extrafont)



# ------------------------------------------------------------------------------
# 1. ggplot2 폰트 설정
# ------------------------------------------------------------------------------
# font_import()
theme_set(theme_grey(base_family = 'D2Coding'))



# ------------------------------------------------------------------------------
# 2. 감성 분석 데이터 불러오기
# ------------------------------------------------------------------------------
get_data <- function(company_name, col_name) {
  review <- read_data(company_name)
  review <- review %>% 
    unnest_tokens(input = !!sym(col_name), output = word, token = 'words', drop = F)
  return(review)
}



# ------------------------------------------------------------------------------
# 3. 감성 분석 생성
# ------------------------------------------------------------------------------
dic <- read_csv('data/knu_sentiment_lexicon.csv')


create_sentiment <- function(review) {
  review <- review %>% 
    left_join(dic, by = 'word') %>% 
    mutate(polarity = ifelse(is.na(polarity), 0, polarity))
  
  review <- review %>% mutate(sentiment = ifelse(polarity == 2, 'pos',
                                          ifelse(polarity == -2, 'neg', 'neu')))
  
  review$sentiment <- factor(review$sentiment, levels = c('pos', 'neg'))
  
  top10 <- review %>% 
    filter(sentiment != 'neu') %>% 
    count(sentiment, word) %>% 
    group_by(sentiment) %>% 
    slice_max(n, n = 10)
  
  ggplot(top10, aes(x = reorder(word, n), y = n, fill = sentiment)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ sentiment, scales = 'free') +
    geom_text(aes(label = n), hjust = -0.3) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    scale_fill_manual(values = c('neg' = '#ff3333', 'pos' = '#0091ff')) +
    labs(x = NULL)
}


sentiment_pros <- function(company_name) {
  review <- get_data(company_name, 'pros')
  create_sentiment(review)
}


sentiment_cons <- function(company_name) {
  review <- get_data(company_name, 'cons')
  create_sentiment(review)
}



# ------------------------------------------------------------------------------
# 4. 감성 분석 실행
# ------------------------------------------------------------------------------

# 4.1. 장점 감성 분석 ----------------------------------------------------------
sentiment_pros('naver')
sentiment_pros('kakao')
sentiment_pros('line')
sentiment_pros('coupang')
sentiment_pros('baemin')


# 4.2. 장점 감성 분석 ----------------------------------------------------------
sentiment_cons('naver')
sentiment_cons('kakao')
sentiment_cons('line')
sentiment_cons('coupang')
sentiment_cons('baemin')
