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
# 2. TF-IDF 데이터 불러오기
# ------------------------------------------------------------------------------
get_data <- function(col_name) {
  review_naver <- read_combine_data('naver')
  review_kakao <- read_combine_data('kakao')
  review_line <- read_combine_data('line')
  review_coupang <- read_combine_data('coupang')
  review_baemin <- read_combine_data('baemin')
  
  review <- rbind(review_naver, review_kakao, review_line, review_coupang, review_baemin)
  review <- clean_data(review, col_name)
  review <- review %>% count(company, word) %>% filter(str_count(word) > 1)
  
  return(review)
}



# ------------------------------------------------------------------------------
# 3. TF-IDF 생성
# ------------------------------------------------------------------------------
create_tf_idf <- function(review, company_name, fill_color) {
  review <- review %>% 
    bind_tf_idf(document = company,
                term = word,
                n = n) %>% 
    arrange(-tf_idf)

  top10 <- review %>% filter(company == company_name) %>% arrange(-tf_idf)
  top10 <- head(top10, n = 10)

  ggplot(top10, aes(x = reorder(word, tf_idf), y = tf_idf, fill = fill_color)) +
    geom_col(fill = fill_color) +
    coord_flip()  
}


tf_idf_pros <- function(review, company_name) {
  create_tf_idf(review, company_name, '#0091ff')
}


tf_idf_cons <- function(review, company_name) {
  create_tf_idf(review, company_name, '#ff3333')
}



# ------------------------------------------------------------------------------
# 4. TF-IDF 실행
# ------------------------------------------------------------------------------
review_pros <- get_data('pros')
review_cons <- get_data('cons')


# 4.1. 장점 TF-IDF -------------------------------------------------------------
tf_idf_pros(review_pros, 'naver')
tf_idf_pros(review_pros, 'kakao')
tf_idf_pros(review_pros, 'line')
tf_idf_pros(review_pros, 'coupang')
tf_idf_pros(review_pros, 'baemin')


# 4.2. 단점 TF-IDF -------------------------------------------------------------
tf_idf_cons(review_cons, 'naver')
tf_idf_cons(review_cons, 'kakao')
tf_idf_cons(review_cons, 'line')
tf_idf_cons(review_cons, 'coupang')
tf_idf_cons(review_cons, 'baemin')
