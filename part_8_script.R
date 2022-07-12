#---------------------------------------------------

# 조작 편의성을 위해 열이름을 소문자로 변환 (결과 출력하지 않음)
colnames(customer_r) <- tolower(colnames(customer_r))
colnames(reservation_r) <- tolower(colnames(reservation_r))
colnames(order_info_r) <- tolower(colnames(order_info_r))
colnames(item_r) <- tolower(colnames(item_r))


#---------------------------------------------------

# 8.1	ggplot2패키지 설치와 기본 문법

install.packages("ggplot2")    # ggplot2 패키지 설치
library(ggplot2)    # ggplot2 패키지 로딩

library(dplyr)    # dplyr 패키지 로딩


#---------------------------------------------------

# 8.3 산점도 : 흩어진 정도 확인 

# 소스 창에서
df_cfm_order <- inner_join(reservation_r, order_info_r, by = "reserv_no") %>%
    select(customer_id, reserv_no, visitor_cnt, cancel, order_no, item_id, sales) %>%
    arrange(customer_id, reserv_no, item_id)

head(df_cfm_order) # 데이터셋 확인

# 총 방문 고객수, 총 매출
df_sct_graph <- df_cfm_order %>% 
    group_by(customer_id) %>% 
    summarise( vst_cnt = sum(visitor_cnt), cust_amt = sum(sales/1000))

head(df_sct_graph)    # 데이터셋 확인

# 그래프 틀그리기
ggplot(data = df_sct_graph, aes(x = vst_cnt, y = cust_amt))

# 그래프 그리기
ggplot(data = df_sct_graph, aes(x = vst_cnt, y = cust_amt)) + 
    geom_point()

# 축 조정하기
ggplot(data = df_sct_graph, aes(x = vst_cnt, y = cust_amt)) + 
    geom_point() + 
    xlim(0, 50) + ylim(0, 500) 

head(customer_r)    # customer_r 테이블 정보 확인

# 성별 추가하기
df_sct_graph2 <- inner_join(df_sct_graph, customer_r, by = "customer_id") %>% 
    select(vst_cnt, cust_amt, sex_code)

head(df_sct_graph2)    # 데이터셋 확인

# 그룹별로 색상 적용하기
ggplot(data = df_sct_graph2, aes(x = vst_cnt, y = cust_amt, color = sex_code)) + 
    geom_point() + 
    xlim(0,50) + 
    ylim(0,500)


#---------------------------------------------------

# 8.4 막대 그래프 : 데이터 크기 비교

# 예약 완료, 주문 완료 데이터 연결하기
df_branch_sales_1 <- inner_join(reservation_r, order_info_r, by = "reserv_no") %>% 
    select(branch, sales) %>% 
    arrange(branch, sales)

# 지점별로 매출 합산하기
df_branch_sales_2 <-  df_branch_sales_1 %>% 
    group_by(branch) %>% 
    summarise(amt = sum(sales)/1000) %>% 
    arrange(desc(amt))

df_branch_sales_2    

# 막대 그래프로 그리기
ggplot(df_branch_sales_2, aes(x = branch, y = amt)) + geom_bar(stat = "identity")

ggplot(df_branch_sales_2, aes(x = reorder(branch, -amt), y = amt)) + 
    geom_bar(stat = "identity")

ggplot(df_branch_sales_2, aes(x = reorder(branch, -amt), y = amt, fill = branch)) + 
    geom_bar(stat="identity")

# 막대 그래프 일부 선택
gg <- ggplot(df_branch_sales_2, aes(x = reorder(branch, -amt), y = amt, fill = branch)) + 
    geom_bar(stat="identity") +
    xlim(c("강남", "영등포", "종로", "용산", "서초", "성북"))
gg

# 가로 막대 그래프 그리기
gg <- ggplot(df_branch_sales_2, aes(x = reorder(branch, -amt), y = amt, fill = branch)) + 
    geom_bar(stat="identity") +
    xlim(c("서초", "용산", "종로", "영등포", "강남"))

# X축과 Y축을 바꿈
gg <- gg + coord_flip()   
gg

# 범례 위치 바꾸기
gg <- gg + theme(legend.position = "bottom")
gg

# 범례 항목 순서 바꾸기
gg <- gg + scale_fill_discrete(breaks = c("강남", "영등포", "종로", "용산", "서초")) 
gg


#---------------------------------------------------

# 8.5	히스토그램 : 도수분포 확인

# 지점 예약 건수 히스토그램 
gg <- ggplot(data = reservation_r, aes(x = branch)) + geom_bar(stat = "count")
gg

# X축, Y축 이름 바꾸기
gg <- gg + labs(title = "지점별 예약 건수", x = "지점", y = "예약건")
gg

# 세부 사항 조정하기
gg <- gg + theme(axis.title.x = element_text(size = 15,
                                             color = "blue",
                                             face = 'bold',
                                             angle = 0) ,
                 axis.title.y = element_text(size = 15,
                                             color = 'red',
                                             angle = 90)
)
gg

ggplot(data = order_info_r, aes(x = sales/1000)) + geom_histogram(binwidth = 5)


#---------------------------------------------------

# 8.6 파이 차트 : 상대적 크기 확인

df_pie_graph <- inner_join(order_info_r, item_r, by = "item_id") %>% 
    group_by(item_id, product_name) %>%
    summarise(amt_item = sum(sales/1000)) %>% 
    select(item_id, amt_item, product_name)

df_pie_graph    # 데이터셋 확인

# 누적 막대 그래프로 그리기

ggplot(df_pie_graph, aes(x = "", y = amt_item, fill = product_name)) + 
    geom_bar(stat="identity") 

# 파이 차트 그리기
gg <- ggplot(df_pie_graph, aes(x = "", y = amt_item, fill = product_name)) + 
    geom_bar(stat = "identity") +
    coord_polar("y", start = 0)
gg

# Spectral 색상 팔렛트로 채움, direction = -1 를 적용하면 팔레트 순서가 바뀜 
gg <- gg + scale_fill_brewer(palette = "Spectral")
gg

# 수동으로 색상을 채움
gg <- gg + 
    scale_fill_manual(values = c("STEAK" = "red", "SPECIAL_SET" = "orange", "SEA_FOOD" = "skyblue", "SANDWICH" = "skyblue", "SALAD_BAR" = "skyblue", "SALAD" = "skyblue", "PIZZA" = "skyblue", "PASTA" = "skyblue", "JUICE" = "skyblue", "WINE" = "skyblue"), breaks = c("STEAK","SPECIAL_SET"))
gg


#---------------------------------------------------

# 8.7 선 그래프: 추세 확인

# 예약 번호(reserv_no) 별로 매출 합계를 구함
total_amt <- order_info_r %>% 
    group_by(reserv_no) %>% 
    summarise(amt_daily = sum(sales/1000)) %>% 
    arrange(reserv_no)

total_amt # 데이터셋 확인

# 예약 번호(reserv_no) 순서를 x축으로 선 그래프를 그림
ggplot(total_amt, aes(x = reserv_no, y = amt_daily, group=1)) + geom_line()


# 예약 번호(reserv_no) 1~6번째 자리까지 선택해서(월로 만듬) 그룹핑
total_amt <- order_info_r %>% 
    mutate(month = substr(reserv_no,1,6)) %>% 
    group_by(month) %>% 
    summarise(amt_monthly = sum(sales/1000))


total_amt # 데이터 확인

# 월별 전체 매출 선 그래프
ggplot(total_amt, aes(x = month, y = amt_monthly, group = 1)) + geom_line() 

# 점을 그림
ggplot(total_amt, aes(x = month, y = amt_monthly, group = 1)) + 
    geom_line() +
    geom_point()

# 선그래프 색상 추가, 레이블(텍스트 데이터)추가
ggplot(total_amt, aes(x = month, y = amt_monthly, group = 1, label = amt_monthly)) + 
    geom_line(color ="red", size = 1) +
    geom_point(color ="darkred", size = 3) +
    geom_text(vjust = 1.5, hjust = 0.5)


#---------------------------------------------------

# 8.8 상자 그림: 데이터 분포 확인하기

# 아이템 메뉴 이름 연결하기(조인)
df_boxplot_graph <- inner_join(order_info_r, item_r, by = "item_id")

# 상자그림 그리기
ggplot(df_boxplot_graph, aes(x = product_name, y = sales/1000)) + 
    geom_boxplot(width = 0.8, outlier.size = 2, outlier.color = "red") +
    labs(title = "메뉴아이템 상자그림", x = "메뉴", y = "매출")

#---------------------------------------------------

# 8.9 ggplot2 그래프 잘 활용하는 법

## 참고 my_first_cook 코드
my_first_cook <- order_info_r %>% 
    mutate(reserv_month = substr(reserv_no, 1, 6)) %>% 
    group_by(item_id, reserv_month) %>% 
    summarise(avg_sales = mean(sales)) %>% 
    arrange(item_id, reserv_month)

ggplot(my_first_cook, aes(x = reserv_month, y = avg_sales, group = item_id, color = item_id)) + 
    geom_line(size = 1) +
    geom_point(color ="darkorange", size=1.5) +
    scale_color_brewer(palette = "Paired") +
    labs(title = "메뉴 아이템별 월 평균 매출 추이", x = "월", y = "매출")
