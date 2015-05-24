setwd(path.git)
setwd("RepData_PeerAssessment1/")
rr1 <- read.csv("activity.csv")
datetime <- strptime(paste(rr1$date,str_pad(rr1$interval,4,pad = "0")), format = "%Y-%m-%d %H%M")
tidy <- cbind(rr1,datetime)[,c(1,2,4)]
by_date <- group_by(tidy, date)
summarize(by_date, steps = sum(steps))
ggplot(sbd, aes(date, steps, group = 1)) + geom_smooth() + geom_point() + geom_line()
