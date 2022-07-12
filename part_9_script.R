#---------------------------------------------------

# 조작 편의성을 위해 열이름을 소문자로 변환
colnames(customer_r) <- tolower(colnames(customer_r))   
colnames(reservation_r) <- tolower(colnames(reservation_r))
colnames(order_info_r) <- tolower(colnames(order_info_r))
colnames(item_r) <- tolower(colnames(item_r))

library(dplyr)
library(ggplot2)

#---------------------------------------------------

# 9.2 빈도 분석 : 지점별 예약 건수와 매출은 어떻게 될까?

# 지점별 예약건수 빈도표
table(reservation_r$branch)

# 주문 취소되지 않은 경우만 선택함
no_cancel_data <- reservation_r %>% filter(cancel == "N")   

# 주문 취소되지 않은 예약건의 부서별 빈도표
table(no_cancel_data$branch)

# reserv_no를 키로 예약, 주문 테이블 연결
df_f_join_1 <- inner_join(reservation_r, order_info_r, by = "reserv_no")    

# item_id를 키로 df_f_join_1, 메뉴 정보 테이블 연결
df_f_join_2 <- inner_join(df_f_join_1, item_r, by = "item_id")    

head(df_f_join_2)    # 3개의 테이블이 이너조인 된 것을 확인

# 주요 지점만 선택
df_branch_sales <- df_f_join_2 %>% 
  filter(branch == "강남" | branch == "마포" | branch == "서초") %>%    
  group_by(branch, product_name) %>%    # 부서명과 메뉴이름으로 그룹화
  summarise(sales_amt = sum(sales)/1000 )    # 매출을 합산

# 누적 막대 그래프 그림
ggplot(df_branch_sales, aes(x = "", y = sales_amt, fill = product_name)) + 
  facet_grid(facets = . ~ branch) +    # 면 분할 함수 branch 기준으로 분할
  geom_bar(stat="identity") 

# 파이 차트로 그림
ggplot(df_branch_sales, aes(x = "", y = sales_amt, fill = product_name)) + 
  facet_grid(facets = . ~ branch) + 
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0)    # 파이차트 그림


#---------------------------------------------------

# 9.3 교차 빈도 분석 : 지점별 메뉴 아이템 주문 비율은

# reserv_no를 키로 예약, 주문 테이블 연결
df_f_join_1 <- inner_join(reservation_r, order_info_r, by = "reserv_no")    

# item_id를 키로 df_f_join_1, 메뉴 정보 테이블 연결
df_f_join_2 <- inner_join(df_f_join_1, item_r, by = "item_id")

# 주요 지점만 선택
df_branch_items <- df_f_join_2 %>% filter(branch == "강남" | branch == "마포" | branch == "서초")

# 교차 빈도표 만들기
table(df_branch_items$branch, df_branch_items$product_name)

# 데이터 프레임 형태로 구조형 변환
df_branch_items_table <- as.data.frame(table(df_branch_items$branch, df_branch_items$product_name))

# 데이터 분석을 위해 데이터를 가공
df_branch_items_percent <- df_branch_items_table %>% 
  group_by(df_branch_items_table$Var1) %>% 
  mutate(percent_items = Freq/sum(Freq) * 100)    # 주문 비율을 계산해서 열을 생성

head(df_branch_items_percent)    # percent_items 열이 생성 된 것을 확인

# 누적 막대 그래프를 그려서 gg 변수에 담음
gg <- ggplot(df_branch_items_percent, aes(x = Var1, y = percent_items, group = Var1, fill = Var2)) + 
  geom_bar(stat = "identity")

# 제목과 범례 이름 지정
gg <- gg + 
  labs(title = "지점별 주문 건수 그래프", x = "지점", y = "메뉴 아이템 판매비율", fill = "메뉴 아이템" )
gg


#---------------------------------------------------

# 9.4 RFM 분석 : 우리 회사의 고객현황은 어떨까?

# 테이블 조인
# reserv_no를 키로 예약, 주문 테이블 연결
df_rfm_join_1 <- inner_join(reservation_r, order_info_r, by = "reserv_no")    

head(df_rfm_join_1)     # 조인된 테이블 확인

# 고객 번호별 방문 횟수(F)와 매출(M)을 정리
df_rfm_data <- df_rfm_join_1 %>% 
  group_by(customer_id) %>%
  summarise(visit_sum = n_distinct(reserv_no), sales_sum = sum(sales)/1000) %>% 
  arrange(customer_id)

df_rfm_data    # 데이터 확인

summary(df_rfm_data)    # df_rfm_data 요약 통계값 확인

# 상자그림 그리기
ggplot(df_rfm_data, aes(x ="", y = visit_sum)) + 
  geom_boxplot(width= 0.8, outlier.size = 2,  outlier.colour = "red") +
  labs(title = "방문 횟수 상자그림", x = "빈도", y = "방문횟수")

ggplot(df_rfm_data, aes(x ="", y = sales_sum)) + 
  geom_boxplot(width=0.8, outlier.size=2,  outlier.colour="red") +
  labs(title = "매출 상자그림", x = "매출", y = "금액")

# 방문 횟수 60%, 90%에 해당하는 분위수 찾기
quantile(df_rfm_data$visit_sum, probs = c(0.6, 0.9))

# 매출 60%, 90%에 해당하는 분위수 찾기
quantile(df_rfm_data$sales_sum, probs = c(0.6, 0.9))

# 총 방문 횟수와 총 매출 합
total_sum_data <- df_rfm_data %>% 
  summarise(t_visit_sum = sum(visit_sum), t_sales_sum = sum(sales_sum))

# 우수 고객 이상의 방문 횟수와 매출 합
loyalty_sum_data <- df_rfm_data %>% 
  summarise(l_visit_sum = sum(ifelse(visit_sum > 2, visit_sum, 0)), l_sales_sum = sum(ifelse(sales_sum > 135, sales_sum, 0)))

loyalty_sum_data / total_sum_data


#---------------------------------------------------

# 9.5 상관 분석 : 스테이크와 와인은 관계가 있을까?

# reserv_no를 키로 예약, 주문 테이블 연결
df_f_join_1 <- inner_join(reservation_r, order_info_r, by = "reserv_no") 

# item_id를 키로 df_f_join_1, 메뉴 정보 테이블 연결
df_f_join_2 <- inner_join(df_f_join_1, item_r, by = "item_id")    

target_item <- c("M0005", "M0009")    # 스테이크와 와인

# 스테이크와 메뉴 아이템 동시 주문 여부확인
df_stime_order <- df_f_join_2 %>% 
  filter((item_id %in% target_item)) %>%    # 스테이크나 와인 주문한 경우를 선택
  group_by(reserv_no) %>%    # 예약 번호로 그룹화
  mutate(order_cnt = n()) %>%    # 그룹화된 행을 센다
  distinct(branch, reserv_no, order_cnt) %>%    # 중복 예약 번호는 하나만 출력 
  filter(order_cnt == 2) %>%     # 2인 경우를 선택(스테이크와 와인 동시주문 경우)
  arrange(branch)

# 동시 주문인 경우의 예약번호 데이터셋 (12건)
df_stime_order    

# 동시 주문한 예약 번호만 담는 stime_order 변수 생성
stime_order_rsv_no <- df_stime_order$reserv_no    

df_stime_sales <- df_f_join_2 %>%     
  filter((reserv_no %in% stime_order_rsv_no) & (item_id %in% target_item)) %>% # 동시주문 예약번호면서 스테이크와 와인일 경우만 선택
  group_by(reserv_no, product_name) %>%    # 예약 번호와 메뉴 아이템으로 그룹화
  summarise(sales_amt = sum(sales)/1000) %>%    # 매출 합계 요약 계산
  arrange(product_name, reserv_no)    # 메뉴 아이템, 예약 번호 기준으로 정렬

# 12건의 동시 주문건이므로 24개의 매출 합계가 생성(스테이크+와인)
df_stime_sales 

steak <- df_stime_sales %>% filter(product_name == "STEAK")    # 스테이크 정보만 담음
wine <- df_stime_sales %>% filter(product_name == "WINE")    # 와인 정보만 담음

plot(steak$sales_amt, wine$sales_amt)    # 스테이크와 와인 매출의 상관도를 그림

cor.test(steak$sales_amt, wine$sales_amt)    # 상관 관계 확인


#---------------------------------------------------

# 9.6 의사 결정 나무 : 어떤 고객이 스테이크를 주문할까?

# 고객별 스테이크 주문 여부를 확인
# (A) 모든 고객의 예약 번호 데이터셋 생성 
df_rsv_customer <- reservation_r %>% 
  select(customer_id, reserv_no) %>%    # 고객의 모든 예약 번호 선택
  arrange(customer_id, reserv_no)

head(df_rsv_customer)    # 고객별 예약 번호 확인

# (B) 스테이크 주문 예약 번호 데이터셋 생성
df_steak_order_rsv_no <- order_info_r %>% 
  filter(item_id == "M0005") %>%    # 스테이크 주문이면 
  mutate(steak_order = "Y") %>%    #steak_order열 데이터를 'Y로를 만듬 
  arrange(reserv_no)

head(df_steak_order_rsv_no)    # 데이터셋 확인

# 고객의 모든 예약 번호(A)에 대해 스테이크 주문한 예약 번호(B)를 레프트 조인 
df_steak_order_1 <- left_join(df_rsv_customer, df_steak_order_rsv_no, by = "reserv_no") %>%
  group_by(customer_id) %>%    # 고객 번호로 그룹화하여 (주문 고객 182명)
  mutate(steak_order = ifelse(is.na(steak_order), "N", "Y")) %>% # 주문여부 NA면 N, Y면 Y로 바꿈
  summarise(steak_order = max(steak_order)) %>%    #최대값만 취함
  arrange(customer_id)

# 최종 정리된 고객별 스테이크 주문여부
df_dpd_var <- df_steak_order_1  

# 종속변수, 최종 182명 고객의 스테이크 주문여부 결과 확인
df_dpd_var    

# 결측치 제거
df_customer <- customer_r %>% filter(!is.na(sex_code))    # 성별이 없으면(NA) 고객 번호 제거

# 고객 테이블과 예약 테이블 customer_id을 키로 이너 조인
df_table_join_1 <- inner_join(df_customer, reservation_r, by = "customer_id")

# df_table_join_1과 주문 테이블을 reserv_no을 키로 이너조인
df_table_join_2 <- inner_join(df_table_join_1, order_info_r, by = "reserv_no")

str(df_table_join_2)    # df_table_join_2 테이블 구조확인

# 고객정보, 성별 정보와 방문 횟수, 방문객수, 매출합을 요약 (코드 풀이에서 자세히 설명)
df_table_join_3 <- df_table_join_2 %>% 
  group_by(customer_id, sex_code, reserv_no, visitor_cnt) %>%    # ⓐ
  summarise(sales_sum = sum(sales)) %>%
  group_by(customer_id, sex_code) %>%    # ⓑ
  summarise(visit_sum = n_distinct(reserv_no), visitor_sum = sum(visitor_cnt), sales_sum = sum(sales_sum)/1000) %>%    # ⓒ
  arrange(customer_id)

df_idp_var <-  df_table_join_3    # 독립변수

df_idp_var    # 독립변수 확인(142행) 

# 독립 변수 데이터셋(①-2)에 종속 변수 데이터셋(①-1) 이너 조인
df_final_data <- inner_join(df_idp_var, df_dpd_var, by = "customer_id") 

# 의사 결정 나무 함수를 사용하기 위해 열 구조를 팩터형으로 바꿈
df_final_data$sex_code <- as.factor(df_final_data$sex_code)
df_final_data$steak_order <- as.factor(df_final_data$steak_order)

df_final_data <- df_final_data[, c(2:6)]    # 의사 결정 나무에 필요한 열만 선택
df_final_data    # 최종 분석용 데이터셋 확인

install.packages("rpart")    # 패키지 설치
library(rpart)    # 패키지 로딩

install.packages("caret")    # 패키지 설치
library(caret)    # 패키지 로딩

install.packages("e1071")    # 패키지 설치
library(e1071)    # 패키지 로딩

# 난수 생성 시 계속 무작위수를 생성하지 않고 10000번대 값을 고정으로 가져온다.
set.seed(10000) 

# 80%의 데이터는 train을 위해, 20%의 데이터는 test를 위해 준비
train_data <- createDataPartition(y = df_final_data$steak_order, p = 0.8, list = FALSE)    
train <- df_final_data[train_data, ]
test <- df_final_data[-train_data, ]

# rpart를 사용해서 의사 결정 나무 만들기
decision_tree <- rpart(steak_order~., data = train)

# decision_tree의 내용을 확인
decision_tree    

predicted <- predict(decision_tree, test, type='class')
confusionMatrix(predicted, test$steak_order)

plot(decision_tree, margin = 0.1)    # 의사결정나무 그리기
text(decision_tree)    # 의사결정나무 텍스트 쓰기

install.packages("rattle")    # 패키지 설치
library(rattle)    # 패키지 로딩

fancyRpartPlot(decision_tree)    # 의사 결정 나무 깔끔하게 그리기

