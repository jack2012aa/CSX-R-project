library("tm")
library("tmcn")
library("jiebaRD")
library("jiebaR")
library("devtools")
library("ggbiplot")
library("factoextra")
d.corpus <- Corpus(DirSource("./Data_TDIDF_PCA_KMEANS"))
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word){
  gsub("[A-Za-z0-9]", "", word)
})


mixseg <- worker()
jieba_tokenizer <- function(d){
  unlist(segment(d[[1]], mixseg))
}
#將文字分段
seg <- lapply(d.corpus, jieba_tokenizer)
count_token <- function(d){
  as.data.frame(table(d))
}
#將分段文字做成table同時計算頻率
tokens <- lapply(seg, count_token)
#總共有幾份文件
n <- length(seg)
#宣告TDM備用
TDM <- tokens[[1]]
#保存名稱的變數
colNames <- names(seg)
colNames <- gsub(".txt", "", colNames)
#將所有文件整合
for(id in c(2:n)){
  TDM <- merge(TDM, tokens[[id]], by = "d", all = TRUE)
  names(TDM) <- c("d", colNames[1:id])
}
#將NA變成0
TDM[is.na(TDM)] <- 0


#在個文檔裡出現了多少詞
#"2"代表以column為對象使用sum
tf <- apply(as.matrix(TDM[, 2:(n+1)]), 2, sum)
library("Matrix")
#用詞出現在各文件的次數計算權重(TF-IDF)
idfCal <- function(word_doc){
  log2((n+1)/nnzero(word_doc))
}
idf <- apply(as.matrix(TDM[,2:(n+1)]), 1, idfCal)
doc.tfidf <- TDM
#各文件中詞的數量的矩陣
tempY <- matrix(rep(c(as.matrix(tf)), each = length(idf)), nrow = length(idf))
#各詞的權重的矩陣
tempX <- matrix(rep(c(as.matrix(idf)), each = length(tf)), ncol = length(tf), byrow = TRUE)
#詞在各文件的實際輕重
doc.tfidf[, 2:(n+1)] <- (doc.tfidf[,2:(n+1)] / tempY) * tempX


#轉置矩陣
t = as.data.frame(t(doc.tfidf))
t = t[-1,]
t = apply(t, 2, as.numeric)
pcat = prcomp(t)
g <- ggbiplot(pcat, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)
g
fviz_pca_ind(pcat, geom.ind = c("point"), col.ind = "cos2")
fviz_eig(pcat)
fviz_pca_var(pcat, col.var =  "contrib")
fviz_pca_biplot(pcat, geom.ind = "point")


kmeansData = pcat$x[, 1:2]
c1 <- kmeans(kmeansData, 3)
plot(kmeansData, col = c1$cluster)
points(c1$centers, col = 1:2, pch = 8, cex = 2)
