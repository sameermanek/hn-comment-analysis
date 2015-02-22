source('functions.R')
dt.articles.all <- getArticles(1e3)
dt.articles <- dt.articles.all[sample(nrow(dt.articles.all))]

lst.comments <- getComments(dt.articles)
mat.comments <- processAllComments(lst.comments)


# First, how about a simple wordcloud?
mat.wc <- as.matrix(removeSparseTerms(mat.comments, 0.99))
num.freq <- as.numeric(matrix(1, nrow(mat.wc), nrow = 1) %*% mat.wc)
names(num.freq) <- colnames(mat.wc)
num.freq <- num.freq/sum(num.freq)

# destem the words? Need to handle punctuation better if I wanted to do this
# chr.unstemmed <- stemCompletion(names(num.freq), dictionary = unlist(str_split(unlist(lst.comments), '[[:space:]+]')), type = "prevalent")

pdf('wordcloud_all.pdf', width = 10, height = 10)
wordcloud(names(num.freq), num.freq, min.freq = 0.0002, colors = brewer.pal(8,'Dark2'), max.words = 3000, random.order = FALSE, use.r.layout = FALSE)
dev.off()
ggplot(, aes(x = num.freq)) + stat_ecdf() + scale_x_log10(labels = percent)

# Then let's do some PCA to see whether that's useful (reduce the dimensionality and can plot the weighting of different PCAs)
# first, get rid of sparse terms such that I am left with fewer variables than trials
# I should do a binary search rather than linear...
num.term <- 0.99
while(ncol(as.matrix(removeSparseTerms(mat.comments, num.term))) > nrow(mat.freq)) num.term <- num.term - 0.01
mat.pca <- as.matrix(removeSparseTerms(mat.comments, num.term))
pca <- princomp(mat.pca)
mat.loadings <- pca$loadings
# weird. I'd like to examine these in more detail (maybe make a wordcloud out of the first couple)

num.dim <- c(2,2)
pdf('wordcloud.pdf', width = 10, height = 10)
par(mfrow=num.dim)
for(i in 1:prod(num.dim)) {
  
  wordcloud(rownames(mat.loadings), abs(mat.loadings[, i])/sum(abs(mat.loadings[, i])), random.order = FALSE, use.r.layout = FALSE, max.words = 300, colors = brewer.pal(8, 'Dark2'))
  Sys.sleep(1)
  # cat('press [enter] to continue')
  #  line <- readline()
}
dev.off()
# Weird. the PCA analysis seems useless. How about normal clustering using kmeans?

num.dim <- c(4,4)
mat.kmeans <- as.matrix(mat.comments)
a <- kmeans(mat.kmeans, centers = prod(num.dim))
mat.freq <- rowsum(mat.kmeans, group = a$cluster)
pdf('wordcloud.pdf', width = 10, height = 10)
par(mfrow=num.dim)
for(i in 1:prod(num.dim)) {
  wordcloud(colnames(mat.comments), mat.freq[i, ], random.order = FALSE, use.r.layout = FALSE, max.words = 300, colors = brewer.pal(8, 'Dark2'))
  #  cat('press [enter] to continue')
  #  line <- readline()
}
dev.off()

