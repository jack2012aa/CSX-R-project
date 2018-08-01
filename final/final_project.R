rank <- read.csv("./Data/College ranking.csv", stringsAsFactors = F, na.strings = c("NA", ""))
GDP <- read.csv("./Data/Country GDP.csv", stringsAsFactors = F, na.strings = c("NA",""))
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
library(ggplot2)
ggplot(rank_total, aes(x = score.x.x)) + geom_histogram() + facet_grid(~country.x.x) + theme_bw() + stat_bin(bins = 100)
