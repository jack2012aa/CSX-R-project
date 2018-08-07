library(ggplot2)
rank <- read.csv("./Data/College ranking.csv", stringsAsFactors = F, na.strings = c("NA", ""))
GDP <- read.csv("./Data/Country GDP.csv", stringsAsFactors = F, na.strings = c("NA",""))
data <- read.csv("./Data/edu_var.csv", stringsAsFactors = F, na.strings = c("NA",""))
expend <- read.csv("./Data/Government expenditure.csv", stringsAsFactors = F, na.strings = c("NA",""))
rank_2012 <- rank[rank$year == "2012",]
rank_2013 <- rank[rank$year == "2013",]
rank_2014 <- rank[rank$year == "2014",]
rank_2014 <- rank_2014[1:100,]
rank_2015 <- rank[rank$year == "2015",]
rank_2015 <- rank_2015[1:100,]
rank_total1 <- merge(rank_2012, rank_2013, by = "institution")
rank_total2 <- merge(rank_2014, rank_2015, by = "institution")
rank_total <- merge(rank_total1, rank_total2, by = "institution")
ggplot(rank_total, aes(x = score.x.x)) + geom_histogram() + facet_grid(~country.x.x) + theme_bw() + stat_bin(bins = 100)

#計算各國各年分數
countrymean <- function(x,y,z){
  eval.parent(substitute(z[z$country == y, paste("x",x[1,"year"], sep = "")] <- mean(x[x$country == y,2])))
}
country <- c("Australia", "Canada", "Denmark", "France", "Germany", 'Israel','Japan','Netherlands','South Korea','Sweden','Switzerland','United Kingdom','USA')
x2012 <- c(1:13)
x2013 <- c(1:13)
x2014 <- c(1:13)
x2015 <- c(1:13)
score <- data.frame(country, x2012, x2013,x2014,x2015)


for (n in c(2012:2015)){
  paste("score", n, sep = "_") <- as.data.frame(paste("rank", n, sep = "_")[c(3,13,14)])
}
countrymean(score_2012, "USA", score)
countrymean(score_2013, "USA", score)
countrymean(score_2014, "USA", score)
countrymean(score_2015, "USA", score)
countrymean(score_2012, "Australia", score)
countrymean(score_2013, "Australia", score)
countrymean(score_2014, "Australia", score)
countrymean(score_2015, "Australia", score)
countrymean(score_2012, "Canada", score)
countrymean(score_2013, "Canada", score)
countrymean(score_2014, "Canada", score)
countrymean(score_2015, "Canada", score)
countrymean(score_2012, "Denmark", score)
countrymean(score_2013, "Denmark", score)
countrymean(score_2014, "Denmark", score)
countrymean(score_2015, "Denmark", score)
countrymean(score_2012, "France", score)
countrymean(score_2013, "France", score)
countrymean(score_2014, "France", score)
countrymean(score_2015, "France", score)
countrymean(score_2012, "Germany", score)
countrymean(score_2013, "Germany", score)
countrymean(score_2014, "Germany", score)
countrymean(score_2015, "Germany", score)
countrymean(score_2012, "Israel", score)
countrymean(score_2013, "Israel", score)
countrymean(score_2014, "Israel", score)
countrymean(score_2015, "Israel", score)
countrymean(score_2012, "Japan", score)
countrymean(score_2013, "Japan", score)
countrymean(score_2014, "Japan", score)
countrymean(score_2015, "Japan", score)
countrymean(score_2012, "Netherlands", score)
countrymean(score_2013, "Netherlands", score)
countrymean(score_2014, "Netherlands", score)
countrymean(score_2015, "Netherlands", score)
countrymean(score_2012, "South Korea", score)
countrymean(score_2013, "South Korea", score)
countrymean(score_2014, "South Korea", score)
countrymean(score_2015, "South Korea", score)
countrymean(score_2012, "Sweden", score)
countrymean(score_2013, "Sweden", score)
countrymean(score_2014, "Sweden", score)
countrymean(score_2015, "Sweden", score)
countrymean(score_2012, "Switzerland", score)
countrymean(score_2013, "Switzerland", score)
countrymean(score_2014, "Switzerland", score)
countrymean(score_2015, "Switzerland", score)
countrymean(score_2012, "United Kingdom", score)
countrymean(score_2013, "United Kingdom", score)
countrymean(score_2014, "United Kingdom", score)
countrymean(score_2015, "United Kingdom", score)
score$avesc <- apply(score[2:5], 1, mean)

score.GPI <- merge(score, data[data$topic == "School enrollment, tertiary (gross), gender parity index (GPI)",], by = "country")
#國與國
score.GPI$avescG <- apply(score.GPI[8:11], 1, mean)
model.GPI <- lm(avesc~avescG, data = score.GPI)
summary(model.GPI)
ggplot(score.GPI, aes(avesc, avescG)) + geom_point() + geom_smooth(method ="lm")
#各年
year.GPI <- data.frame(c(1:48),c(1:48))
year.GPI[1:48,1] <- score.GPI[,2:5]
year.GPI[1:12,2] <- score.GPI[,8]
year.GPI[13:24,2] <- score.GPI[,9]
year.GPI[25:36,2] <- score.GPI[,10]
year.GPI[37:48,2] <- score.GPI[,11]
colnames(year.GPI) <- c("score","GPI")
summary(lm(GPI~score,data = year.GPI), data = year.GPI)
ggplot(year.GPI, aes(GPI, score)) + geom_point() + geom_smooth(method = "lm")

score.expend <- merge(score, data[data$topic == "Expenditure on tertiary education (% of government expenditure on education)",], by = "country")
#國
score.expend$avescG <- apply(score.expend[8:10], 1, mean)
summary(lm(avesc~avescG, data = score.expend))
ggplot(score.expend, aes(avesc, avescG)) + geom_point() + geom_smooth(method ="lm")
#年
year.expend <- data.frame(c(1:48),c(1:48))
year.expend[1:12,1] <- score.expend[,2]
year.expend[13:24,1] <- score.expend[,3]
year.expend[25:36,1] <- score.expend[,4]
year.expend[37:48,1] <- score.expend[,5]
year.expend[1:12,2] <- score.expend[,8]
year.expend[13:24,2] <- score.expend[,9]
year.expend[25:36,2] <- score.expend[,10]
year.expend[37:48,2] <- score.expend[,11]
colnames(year.expend) <- c("score","expend")
summary(lm(expend~score, data = year.expend))
ggplot(year.expend, aes(expend,score)) + geom_point() +geom_smooth(method = "lm")


score.expend.std <- merge(score, data[data$topic == "Government expenditure per student, tertiary (% of GDP per capita)",], by = "country")
#國
score.expend.std$avescG <- apply(score.expend.std[8:10], 1, mean)
summary(lm(avesc~avescG, data = score.expend.std))
ggplot(score.expend.std, aes(avesc, avescG)) + geom_point() + geom_smooth(method ="lm")
#年
year.expend.std <- data.frame(c(1:48),c(1:48))
year.expend.std[1:12,1] <- score.expend.std[,2]
year.expend.std[13:24,1] <- score.expend.std[,3]
year.expend.std[25:36,1] <- score.expend.std[,4]
year.expend.std[37:48,1] <- score.expend.std[,5]
year.expend.std[1:12,2] <- score.expend.std[,8]
year.expend.std[13:24,2] <- score.expend.std[,9]
year.expend.std[25:36,2] <- score.expend.std[,10]
year.expend.std[37:48,2] <- score.expend.std[,11]
colnames(year.expend.std) <- c("score","expend.per.students")
summary(lm(expend.per.students~score, data = year.expend.std))
ggplot(year.expend.std, aes(expend.per.students,score)) + geom_point() +geom_smooth(method = "lm")

score.current <- merge(score, data[data$topic == "Current education expenditure, tertiary (% of total expenditure in tertiary public institutions)",], by = "country")
#國
score.current$avescG <- apply(score.current[8:10], 1, mean)
summary(lm(avesc~avescG, data = score.current))
ggplot(score.current, aes(avesc, avescG)) + geom_point() + geom_smooth(method ="lm")
#年
year.current <- data.frame(c(1:48),c(1:48))
year.current[1:12,1] <- score.current[,2]
year.current[13:24,1] <- score.current[,3]
year.current[25:36,1] <- score.current[,4]
year.current[37:48,1] <- score.current[,5]
year.current[1:12,2] <- score.current[,8]
year.current[13:24,2] <- score.current[,9]
year.current[25:36,2] <- score.current[,10]
year.current[37:48,2] <- score.current[,11]
colnames(year.current) <- c("score","current.expend")
summary(lm(current.expend~score, data = year.current))
ggplot(year.current, aes(current.expend,score)) + geom_point() +geom_smooth(method = "lm")

score.gross <- merge(score,data[data$topic == "School enrollment, tertiary (% gross)",], by = "country")
#國
score.gross$avescG <- apply(score.gross[8:10], 1, mean)
summary(lm(avesc~avescG, data = score.gross))
ggplot(score.gross, aes(avesc, avescG)) + geom_point() + geom_smooth(method ="lm")
#年
year.gross <- data.frame(c(1:48),c(1:48))
year.gross[1:12,1] <- score.gross[,2]
year.gross[13:24,1] <- score.gross[,3]
year.gross[25:36,1] <- score.gross[,4]
year.gross[37:48,1] <- score.gross[,5]
year.gross[1:12,2] <- score.gross[,8]
year.gross[13:24,2] <- score.gross[,9]
year.gross[25:36,2] <- score.gross[,10]
year.gross[37:48,2] <- score.gross[,11]
colnames(year.gross) <- c("score","School.enrollment")
summary(lm(School.enrollment~score, data = year.gross))
ggplot(year.gross, aes(School.enrollment,score)) + geom_point() +geom_smooth(method = "lm")

score.stdratio <- merge(score, data[data$topic == "Pupil-teacher ratio, tertiary",], by = "country")
#國
score.stdratio$avescG <- apply(score.stdratio[8:10], 1, mean)
summary(lm(avesc~avescG, data = score.stdratio))
ggplot(score.stdratio, aes(avesc, avescG)) + geom_point() + geom_smooth(method ="lm")
#年
year.stdratio <- data.frame(c(1:48),c(1:48))
year.stdratio[1:12,1] <- score.stdratio[,2]
year.stdratio[13:24,1] <- score.stdratio[,3]
year.stdratio[25:36,1] <- score.stdratio[,4]
year.stdratio[37:48,1] <- score.stdratio[,5]
year.stdratio[1:12,2] <- score.stdratio[,8]
year.stdratio[13:24,2] <- score.stdratio[,9]
year.stdratio[25:36,2] <- score.stdratio[,10]
year.stdratio[37:48,2] <- score.stdratio[,11]
colnames(year.stdratio) <- c("score","pupil.teacher.ratio")
summary(lm(pupil.teacher.ratio~score, data = year.stdratio))
ggplot(year.stdratio, aes(pupil.teacher.ratio,score)) + geom_point() +geom_smooth(method = "lm")

score.attain <- merge(score, data[data$topic == "Educational attainment, at least completed short-cycle tertiary, population 25+, total (%) (cumulative)",], by = "country")
#國與國
score.attain$avescG <- apply(score.attain[8:11], 1, mean)
model.attain <- lm(avesc~avescG, data = score.attain)
summary(model.attain)
ggplot(score.attain, aes(avesc, avescG)) + geom_point() + geom_smooth(method ="lm")
#各年
year.attain <- data.frame(c(1:48),c(1:48))
year.attain[1:12,1] <- score.attain[,2]
year.attain[13:24,1] <- score.attain[,3]
year.attain[25:36,1] <- score.attain[,4]
year.attain[37:48,1] <- score.attain[,5]
year.attain[1:12,2] <- score.attain[,8]
year.attain[13:24,2] <- score.attain[,9]
year.attain[25:36,2] <- score.attain[,10]
year.attain[37:48,2] <- score.attain[,11]
colnames(year.attain) <- c("score","attainment")
summary(lm(attainment~score,data = year.attain), data = year.attain)
ggplot(year.attain, aes(attainment, score)) + geom_point() + geom_smooth(method = "lm")
