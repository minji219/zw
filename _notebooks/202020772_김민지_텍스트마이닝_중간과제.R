#########통계학과 202020772 김민지 텍스트 마이닝 중간과제##############
library(KoNLP)     
library(wordcloud) 
library(dplyr)
library(ggplot2)
library(readr)      
library(dplyr)         
library(tibble) 
library(tm)
library(tidytext)
library(rvest)
library(httr)
library(tm)    
library(stringr)   
library(koRpus)
library(tm.plugin.koRpus)
library(ggwordcloud)
library(tidyr)


#워킹디렉토리 지정
new_directory <- "C:/Users/user/Downloads" 
setwd(new_directory)

#1
#(1)

#데이터 읽는 함수, 티블구조로 읽기
read_csv_file <- function(file) { 
  data <- read.csv(file, header = T)
  return(data)
}

#vd를 함수로 읽기
vd <- read_csv_file("vd.csv")  

#(2)

# 새로운 데이터셋을 만드는 것 함수로만들기 
#subset함수 사용하여 조건에 따라 data 필터링, 
#year >= start_year 조건은 데이터 프레임에서 'year' 열(또는 변수)의 값이 start_year 이상인 행(row)만 선택
#select = selected_cols는 선택할 열(변수)을 지정
#즉, 함수는 'year' 열의 값이 start_year 이상이면서, selected_cols에 지정된 열만을 선택한 데이터 프레임을 생성
subset_data <- function(data, start_year, selected_cols) {
  subset_data <- subset(data, year >= start_year, select = selected_cols)
  return(subset_data)
}

# year, edu, income, expense 변수와 2020,2021년 데이터로만 이루어진 새로운 new_vd데이터셋을 만들기
new_vd <- subset_data(vd, 2020, c("year", "edu", "income", "expense"))
print(new_vd)

#(3) 새로운 데이터셋을 이용하여 교육수준별 수입의 합을 막대그래프로 그렸다.
#identity: 데이터 실제값 막대그래프로 표시 , 색상 red로지정
#제목이랑 x,y 이름 생성 
ggplot(data = new_vd, aes(x = edu, y = income)) +
  geom_bar(stat = "identity", fill = "red") + 
  labs(title = "교육수준별 수입의 합", x = "교육수준", y = "수입")



#(4)
# 새로운 data set을 이용하여 년도별 지출 평균을 구하여
#어떤 년도의 평균이 얼마나 큰지/작은지를 출력하는 code를 작성
#함수 정의
average_new_vd <- function(data) {
  # 연도별 평균 지출 계산
  avg_year <- aggregate(expense ~ year, data, FUN = mean)
  
  # 가장 큰 및 가장 작은 평균 지출 연도 찾기
  max_year <- avg_year[which.max(avg_year$expense), "year"]
  min_year <- avg_year[which.min(avg_year$expense), "year"]
  max_mean <- max(avg_year$expense)
  min_mean <- min(avg_year$expense)
  
  cat("가장 큰 평균 지출 연도:", max_year, "평균:", max_mean, "\n")
  cat("가장 작은 평균 지출 연도:", min_year, "평균:", min_mean, "\n")
  
  # 가장 큰 평균과 가장 작은 평균 비교
  if (max_mean > min_mean) {
    cat("가장 큰 평균이 가장 작은 평균보다", max_mean - min_mean, "큽니다.\n")
  } else if (max_mean < min_mean) {
    cat("가장 작은 평균이 가장 큰 평균보다", min_mean - max_mean, "작습니다.\n")
  } else {
    cat("가장 큰 평균과 가장 작은 평균이 같습니다.\n")
  }
}

# 함수 사용 예제
average_new_vd(new_vd)




#2번
scrape_jbnu_notices <- function(page_number) {
  # 페이지 주소
  base_url <- paste0("https://www.jbnu.ac.kr/kor/?menuID=139&pno=", page_number)
  
  # 웹 페이지 불러오기
  html <- read_html(base_url)
  
  # 공지사항 제목 읽기
  post <- html %>%  
    html_nodes("td") %>%  #td요소 선택
    html_nodes("a") %>%   #웹페이지를 다른쪽으로 이동 
    html_text()
  
  # 사전처리 : 태그 제거 및 빈칸 제거
  post_cleaned <- gsub("[\r\n\t]", "", post)
  post_cleaned <- post_cleaned[post_cleaned != ""] #공백제거거
  
  # 데이터프레임 생성
  df <- data.frame(Title = post_cleaned, stringsAsFactors = FALSE) 
  
  return(df)
}


# 함수 호출
page_number <- 100 #페이지 넘버 100pg
notices_df <- scrape_jbnu_notices(page_number) #함수 선언 
notices_df

#제목 앞에 빈칸 없애기 
notices = c()
for(i in 1:length(notices_df)){ 
  if(nchar(notices_df[i]) > 1) {
    notices = append(notices, notices_df[i])
  }
}

# 결과 데이터프레임 출력
print(notices)


#3번
#(1)
get_article_text <- function(news_url) {
  # 웹 페이지의 HTML 내용을 가져오기
  response <- GET(news_url)
  web_content <- content(response, "text", encoding = "UTF-8")
  
  # HTML 파싱
  parsed_page <- read_html(web_content)
  
  # 기사 내용 뽑아오기
  article_text <- parsed_page %>%
    html_node(".newsct_article") %>%
    html_text()
  
  return(article_text)
}

# 함수 호출 
news_url <- "https://n.news.naver.com/mnews/article/421/0006405379?sid=102"
raw_article <- get_article_text(news_url)


raw_article

# 사전처리하고 티블로 바꾸기
Custom_Tibble <- function(raw_article) {
  article <- raw_article %>% str_replace_all("[^가-힣]", " ") %>% str_squish() # 모든 특수문자 공백으로 대체 
  article <- as_tibble(article) #티블로 만들기 
  return (article) #함수 리턴
}

# 전처리하고 티블로 만든 article 불러내기
article <- Custom_Tibble(raw_article)
article


#(2)
#article 토큰화하기 
word_article <- article %>%
  unnest_tokens(input = value,   # tidytext 패키지 함수 단어로 구분함 , input은 토큰화 할 열 
                output = word,  #토큰화 저장 열열
                token = "words") #단어 단위로 토큰화 
word_article


#filter를 사용하여 한글자 단어 없애기 
filter_single_words <- function(data) {
  data %>%
    filter(nchar(word) > 1) #한가지 이상 단어 뽑기 
}

#filter_single_words 함수 사용하여 불러내기
filtered_word_space <- filter_single_words(word_article)


# 결과 출력
print(filtered_word_space)

#(3)
# 상위 출현 단어를 막대그래프로 시각화하는 함수
plot_top_words <- function(data, top_n = 10) {  #상위 출연 단어 top_n= 10개로 지정 
  data %>%
    count(word, sort = TRUE) %>% #단어의 빈도 계산 , 내림차순으로 정리 
    top_n(top_n) %>% #빈도수 기준으로 top_n개의 단어 선택 
    ggplot(aes(x = reorder(word, n), y = n)) + #ggplot 사용, reorder함수 사용하여 word의 열을 n 열의 빈도에 따라 재정렬 
    geom_bar(stat = "identity", fill = "blue") + #막대그래프 생성, 색상 blue로 지정 
    labs(title = "상위 출현 단어", x = "단어", y = "빈도") + #그래프의 레이블 , 축 지정 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # x축 텍스트라벨 회전 ,오른쪽 정렬 
}

# 함수를 사용하여 막대그래프 생성 top_n 사용하여 상위 10개 단어로 막대그래프 생성 
word_barplot <- plot_top_words(filtered_word_space, top_n = 10)
print(word_barplot)


# 워드 클라우드 생성 함수
#max_words 최대 단어수 설정 
create_wordcloud <- function(data, max_words = 50) {
  wordcloud(data$word, data$n, max.words = max_words, colors = brewer.pal(8, "Dark2"))
}

# 함수를 사용하여 워드 클라우드 생성
wordcloud_data <- filtered_word_space %>%
  count(word, sort = TRUE)
wordcloud_plot <- create_wordcloud(wordcloud_data, max_words = 50)
print(wordcloud_plot)

#4번
#(1)

# tibble로 press 데이터 불러내기, 깨져서 출력하여 euc-kr을 encoding으로 사용 
raw_press <- read_csv("press.csv", locale = locale(encoding = "euc-kr"))
class(raw_press)
raw_press


custom_DataSet <- function(raw_data) {
 # 불필요한 문자 및 공백 제거
  data <- raw_data %>%
    mutate(article = stringr::str_replace_all(article, "[^가-힣 & ^.]", " "),
           article = stringr::str_squish(article))
  
  return(data)
}


# 데이터에 custom_Dataset 함수를 적용
press <- custom_DataSet(raw_press)  
press


##토큰화하기
  tokenize_data <- function(data) {
  word_space <- data %>%
    unnest_tokens(input = article, #article을 토큰화 
                  output = word,
                  token = extractNoun)
  
#(2)  
# 빈도수로 정렬하고 두 글자 이상만 남김
  word_space_filtered <- word_space %>%
    count(press, word, sort = TRUE) %>% #내림차순으로 정렬 
    filter(nchar(word) > 1)#한글자인 단어 제외
  
  return(word_space_filtered)
}

# 함수 호출
word_space <- tokenize_data(press)
word_space #단어 빈도수 


#(3)
#TF-IDF(Term Frequency-Inverse Document Frequency) 값을 계산하는 함수
#word: 단어
#press: 텍스트의 구분 변수 (예: 언론사)
# n: 단어의 빈도
#term: 단어를 나타내는 열 (여기서는 word)
#document: 텍스트를 나타내는 열 (여기서는 press)
#n: 단어의 빈도를 나타내는 열 (여기서는 n)
calculate_TFIDF <- function(data) {
  data %>%
    bind_tf_idf(term = word, document = press, n = n) %>%
    arrange(desc(tf_idf))
}

tf_idf = calculate_TFIDF(word_space)




#TF-IDF를 이용하여 각 문서에서 상대적으로 중요한 단어를 출력하는 code
get_TopTFIDF_custom <- function(data) {
  top_words <- data %>%
    arrange(desc(tf_idf)) %>%
    group_by(press) %>% #press별로 그룹화 
    slice_head(n = 10) #그룹화된 각 언론사에서 상위 10개의 단어를 선택
  
  return(top_words) #최종 결과로 언론사별로 상위 10개의 TF-IDF 값이 높은 단어 목록을 반환
}

tf_idf_top10_custom <- get_TopTFIDF_custom(tf_idf)
#국민일보, 동아일보, 중아일보, 프레시안, 한겨레 순으로 각 언론사별 상위 10개 TF-IDF 값이 높은 단어 출력 
print(tf_idf_top10_custom, n = 50) 







