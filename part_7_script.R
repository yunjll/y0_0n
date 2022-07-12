#---------------------------------------------------
최단비 바보 
# 조작 편의성을 위해 열이름을 소문자로 변환 (결과 출력하지 않음)
colnames(customer_r) <- tolower(colnames(customer_r))   
colnames(reservation_r) <- tolower(colnames(reservation_r))
colnames(order_info_r) <- tolower(colnames(order_info_r))
colnames(item_r) <- tolower(colnames(item_r))


#---------------------------------------------------

# 7.1	dplyr 패키지 설치와 파이프 연산자

# 열 이름이 소문자로 변환되었는지 확인 (결과 출력)
head(customer_r)

# dplyr 패키지 설치
install.packages("dplyr") 

# 패키지 로딩
library(dplyr)    

#---------------------------------------------------

# 7.2	행 요약과 그룹화

# dplyr 패키지 count()함수는 데이터를 세는 함수
customer_r %>% count()   

summarise(order_info_r, avg = mean(sales))

summarise(order_info_r, min_value = min(sales), max_value = max(sales))


order_info_r %>% summarise(min_value = min(sales), max_value = max(sales))

reservation_r %>% group_by(customer_id) %>% summarise(avg = mean(visitor_cnt))

#---------------------------------------------------

# 7.3 행 조작

order_info_r %>% filter(item_id == "M0001")


order_info_r %>% filter(item_id == "M0001" & sales >= 150000)

head(order_info_r)

order_info_r %>% distinct(item_id)

order_info_r %>% slice(2:4)

order_info_r %>% slice(c(1, 3))

order_info_r %>% arrange(sales)

order_info_r %>% arrange(desc(sales))    # desc 내림차순

order_info_r %>% arrange(reserv_no, item_id)

table_added_row <- order_info_r %>% add_row(order_no = "1", item_id = "1", reserv_no = "1")

table_added_row %>% arrange(order_no)

order_info_r %>% sample_frac(0.1, replace = TRUE)

#---------------------------------------------------

# 7.4 열 조작

order_info_r %>% select(reserv_no, sales)

order_info_r %>% group_by(reserv_no) %>% mutate(avg = mean(sales))

order_info_r %>% group_by(reserv_no) %>% transmute(avg = mean(sales))

order_info_r %>% mutate_all(funs(max))

order_info_r %>% mutate_if(is.numeric, funs(log(.)))

order_info_r %>% mutate_at(vars(sales), funs(max))

order_info_r %>% rename(amt = sales)

#---------------------------------------------------

# 7.5 테이블 조작

tmp_order_info_r <- order_info_r
bind_cols(order_info_r, tmp_order_info_r)

bind_cols(order_info_r, reservation_r)

tmp_order_info_r <- order_info_r
bind_rows(order_info_r, tmp_order_info_r)

bind_rows(order_info_r, reservation_r)

inner_join(reservation_r, order_info_r, by = "reserv_no") %>% arrange(reserv_no, item_id)

inner_join(reservation_r, order_info_r, by = "reserv_no") %>% 
    arrange(reserv_no, item_id) %>% 
    select(reserv_no, customer_id, visitor_cnt, cancel, order_no, item_id, sales)

left_join(reservation_r, order_info_r, by = "reserv_no") %>% 
    arrange(reserv_no, item_id) %>% 
    select(reserv_no, customer_id, visitor_cnt, cancel, order_no, item_id, sales)

right_join(reservation_r, order_info_r, by = "reserv_no") %>% 
    arrange(reserv_no, item_id) %>% 
    select(reserv_no, customer_id, visitor_cnt, cancel, order_no, item_id, sales)

# 새로운 행 생성
table_added_row <- order_info_r %>% 
    add_row(order_no = "1", item_id = "1", reserv_no = "1", sales = 1)   

full_join(reservation_r, table_added_row, by = "reserv_no") %>% 
    arrange(reserv_no, item_id) %>% 
    select(reserv_no, customer_id, visitor_cnt, cancel, order_no, item_id, sales)

# reservation_r의 reserv_no 추출
reservation_r_rsv_no <- select(reservation_r, reserv_no)    

# order_info_r reserv_no 추출
order_info_r_rsv_no <- select(order_info_r, reserv_no)                     

# 양쪽 데이터 셋에 존재하는 reserv_no
intersect(reservation_r_rsv_no, order_info_r_rsv_no)    

setdiff(reservation_r_rsv_no, order_info_r_rsv_no)

union(reservation_r_rsv_no, order_info_r_rsv_no)

#---------------------------------------------------

# 7.6 dplyr 함수 잘 사용하는 법
reservation_r %>% 
    group_by(customer_id) %>% 
    summarise(avg = mean(visitor_cnt)) %>% 
    filter(avg >= 3) %>% 
    arrange(desc(avg))

reservation_r %>% 
    group_by(customer_id) %>%
    mutate(avg = mean(visitor_cnt)) %>% 
    select(customer_id, avg) %>% 
    filter(avg >= 3) %>% 
    distinct(customer_id, avg) %>% 
    arrange(desc(avg))

my_first_cook <- order_info_r %>% 
    mutate(reserv_month = substr(reserv_no, 1, 6)) %>% 
    group_by(item_id, reserv_month) %>% 
    summarise(avg_sales = mean(sales)) %>% 
    arrange(item_id, reserv_month)

my_first_cook

# 잠깐만요

iris
tb <- as_tibble(iris)    # as_tibble() : 티블 구조로 변환하는 함수
tb
print(as_tibble(iris), n = 15)
tb$Sepal.Length    # Sepal.Length의 데이터 값 전체를 출력
