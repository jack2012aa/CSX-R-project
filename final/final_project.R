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

score_2012 <- as.data.frame(rank_2012[,c(3,13,14)])
score_2013 <- as.data.frame(rank_2013[,c(3,13,14)])
score_2014 <- as.data.frame(rank_2014[,c(3,13,14)])
score_2015 <- as.data.frame(rank_2015[,c(3,13,14)])
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
year.GPI <- cbind(as.data.frame(t(score.GPI[,2:5])),as.data.frame(t(score.GPI[,8:11])))
colnames(year.GPI) <- paste("V", c(1:24), sep = "")
summary(lm(as.numeric(score.GPI[3,2:5]) ~ as.numeric(score.GPI[3,8:11])))
ggplot(year.GPI, aes(V3,V15)) + geom_point() +geom_smooth(method = "lm")

score.expend <- merge(score, data[data$topic == "Expenditure on tertiary education (% of government expenditure on education)",], by = "country")

score.expend.std <- merge(score, data[data$topic == "Expenditure on tertiary education (% of government expenditure on education)",], by = "country")

score.current <- merge(score, data[data$topic == "Current education expenditure, tertiary (% of total expenditure in tertiary public institutions)",], by = "country")

score.gross <- merge(score,data[data$topic == "School enrollment, tertiary (% gross)",], by = "country")

score.stdratio <- merge(score, data[data$topic == "Pupil-teacher ratio, tertiary",], by = "country")

score.attain <- merge(score, data[data$topic == "Educational attainment, at least completed short-cycle tertiary, population 25+, total (%) (cumulative)",], by = "country")

