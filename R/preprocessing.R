sj_15_final <- merge(sj_15_w, sj_op_sum_15, all.x = T)
sj_15_final %>% group_by(month=floor_date(date, "month")) %>% tally()
sj_outputs %>% filter(year == 2016) %>% group_by(month, day) %>% summarise(ssum = sum(output))
sj_outputs$date <- as.Date(sj_outputs$date, format = '%Y%m%d')
sj_outputs$date <- paste0(as.character(sj_outputs$year), sj_outputs$month, sj_outputs$day)
sj_16_w <- sj_points %>% filter(date > '2015-12-31' & date < '2017-01-01')
sj_points <- data00 %>% filter(point == 133)


presidential %>%
filter(start > 1973 & party == "Democratic") %>%
select(name)

mypresidents <- presidential %>%
mutate(term_length = end - start) #mutate variables to create new ones

mypresidents %>% arrange(desc(term_length), start)
#The function sort() will sort a vector, but not a data frame. The function that will sort a data frame is called arrange()

mypresidents %>%
group_by(party) %>%
summarize(
N = n(),
avg_term_length = mean(term_length),
std_term_length = sd(term_length)
)
# Compare the intermediate data.frame group_by(mypresidents,party) with mypresidents


q_1 <- merge(billing, member, all.x = T)

a_1 <-q_1 %>% 
  group_by(country_code, month = floor_date(billing_date, 'month')) %>%
  summarise(total_ex = sum(sales))

test <- q_1 %>%
  mutate(mm = format(billing_date, '%m'), yyyy = format(billing_date, '%Y')) %>%
  group_by(country_code, yyyy, mm) %>%
  summarise(tt = sum(sales))

#mutate의 중요성. 
#as.Date의 데이터를 mutate(mm = format(biiling_date, '%m'))

test_2 <- q_1 %>%
  group_by(country_code, user_id) %>%
  summarise(total = sum(sales)) %>%
  mutate(rank = rank(desc(total), ties.method = "max"))
a_2 <- test_2[test_2$rank < 6, ]

# desc로 역순으로 해야. 
# 즉, rank 매길 때, 마치 기록이라고 컴퓨터는 생각한다. 달리기 기록 빠르듯,
# 그래서 desc 해야

# rank: 동점자들은 같은 순위 매기고, 그 뒷 등수는 동점자 수 만큼 띄어 놓는다.
# dense_rank: 위와 같지만, 다음순위 바로 이어서
# row_number: 무조건 등수를 가른다. 

pool <- q_3 %>%
  group_by(country_code) %>%
  summarise(count = n_distinct(user_id))



#############################################
#reshape
library(MASS)
str(Cars93)

table(Cars93$Type)

Cars93_sample <- subset(Cars93, 
                         select = c(Type, Origin, MPG.city, MPG.highway), 
                         subset = (Type %in% c("Compact", "Van"))) 


Cars93_sample_melt <- melt(data = Cars93_sample, 
                            id.vars = c("Type", "Origin"), 
                            measure.vars = c("MPG.city", "MPG.highway"))


cast(data = Cars93_sample_melt, Type ~ variable, fun = mean)

# 한개의 id.var + variable (세로) & 다른 id.var (가로) 조합의 value 값에 mean 함수 적용
cast(data = Cars93_sample_melt, Type + variable ~ Origin, fun = mean)

#http://rfriend.tistory.com/80 [R, Python 분석과 프로그래밍 (by R Friend)]
