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
