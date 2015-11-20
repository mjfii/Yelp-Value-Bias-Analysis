
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(gtable)
library(pander) 
library(graph)
library(Rgraphviz)
library(wordcloud)

# interrogate frequent terms and their relationship to the corpus
cat('building frequent terms image\n')
frequent.terms <- findFreqTerms(tdm, lowfreq = 10000)
png(file='images/frequent-terms.png',width=850,height=450)
plot(tdm, main = 'Figure 1: Association of Term Frequency Greater than 10k',
     term = frequent.terms, corThreshold = 0.17, weighting = TRUE)
x <- dev.off(); remove(x); remove(frequent.terms)

# cluster terms by removing sparse terms
cat('building clustered term image\n')
m <- as.matrix(removeSparseTerms(tdm, sparse = 0.80))
distMatrix <- dist(scale(m))
fit <- hclust(distMatrix, method = 'ward.D2')
png(file='images/clustered-terms.png',width=850,height=450)
plot(fit, main = 'Figure 2: Clustered Terms with Minimum Term Density of 20%', sub = '', 
     xlab = 'Term Clusters', ylab = 'Correlated Height')
rect.hclust(fit, k = 4)
x <- dev.off(); remove(x); remove(m); remove(fit); remove(distMatrix)

# so how frequent is it, what terms are associated with 'price'
cat('calculating price term frequency\n')
term.freq <- rowSums(as.matrix(removeSparseTerms(tdm, sparse = 0.95)))
term.freq <- subset(term.freq, term.freq >= 15)
term.freq <- data.frame(term = names(term.freq), freq = term.freq)['price', ]
term.freq <- data.frame(term_frequency = term.freq[ , 2])
pander(term.freq)
cat('calculating price review frequency\n')
review.freq <- data.frame(review_frequency = length(subset(working.set, price_flag == TRUE)$awarded_stars))
pander(review.freq)

cat('calculating price associations\n')
assoc <- findAssocs(tdm, 'price', 0.15)
pander(data.frame(assoc[1]))
remove(term.freq); remove(review.freq); remove(assoc)

# so while a limited subset of reviews have price included, what do the rating look 
# like against the average ratings, and how are the restaurant priced?
cat('building price review heatmap\n')
dt1 <- working.set[working.set$price_flag == FALSE, ]
dt1 <- data.table(dt1)
dt1 <- dt1[, length(business_id), by = list(awarded_stars, price_label)]
g1 <- ggplot(dt1, aes(x = awarded_stars, y = price_label))
g1 <- g1 + labs(fill = 'Review Count')
g1 <- g1 + geom_tile(aes(fill = V1), colour = "#CCCCCC")
g1 <- g1 + scale_fill_gradient(low = "#CCCCCC", high = "steelblue")
g1 <- g1 + geom_text(aes(label = paste(round(dt1$V1 / sum(dt1$V1) * 100, 1),'%', sep = ''), size = 10), show_guide = FALSE)
g1 <- g1 + ylab('Price Ranage')
g1 <- g1 + xlab('Awarded Stars')
g1 <- g1 + ggtitle('Price Not Mentioned In Review')

dt2 <- working.set[working.set$price_flag == TRUE, ]
dt2 <- data.table(dt2)
dt2 <- dt2[, length(business_id), by = list(awarded_stars,price_label)]
g2 <- ggplot(dt2, aes(x = awarded_stars, y = price_label))
g2 <- g2 + labs(fill = 'Review Count')
g2 <- g2 + geom_tile(aes(fill = V1), colour = "#CCCCCC") 
g2 <- g2 + scale_fill_gradient(low = "#CCCCCC", high = "steelblue")
g2 <- g2 + geom_text(aes(fill = dt2$V1, label = paste(round(dt2$V1 / sum(dt2$V1) * 100, 1),'%', sep = ''), size = 10), show_guide = FALSE)
g2 <- g2 + ylab('Price Ranage')
g2 <- g2 + xlab('Awarded Stars')
g2 <- g2 + ggtitle('Price Mentioned In Review')

png(file='images/rating-price-comparison.png', width=850, height=350)
grid.arrange(g1, g2, nrow = 1, top = textGrob('Figure 3: Restaurant Price Range Compared to Review Awarded Stars',
                                              gp = gpar(fontsize = 20)))
x <- dev.off(); remove(x); remove(dt1); remove(dt2); remove(g1); remove(g2)

# makes sense, or is it inconculsive, so does that affect price (inference)
cat('building statistical inference image\n')
t1 <- subset(working.set, price_flag == TRUE & price_range == 1)$awarded_stars
t2 <- subset(working.set, price_flag == FALSE & price_range==1)$awarded_stars
t <- t.test(t1, t2, paired=FALSE, var.equal=FALSE)
df1 <- data.frame('Price Range' = '$', 'Confidence Interval' = paste(round(t$conf.int[1],4),round(t$conf.int[2],4), sep = ' through '), 'P-Value' = round(t$p.value, 4))

t1 <- subset(working.set, price_flag == TRUE & price_range == 2)$awarded_stars
t2 <- subset(working.set, price_flag == FALSE & price_range == 2)$awarded_stars
t <- t.test(t1, t2, paired=FALSE, var.equal=FALSE)
df2 <- data.frame('Price Range' = '$$', 'Confidence Interval' = paste(round(t$conf.int[1],4),round(t$conf.int[2],4), sep = ' through '), 'P-Value' = round(t$p.value, 4))

t1 <- subset(working.set, price_flag == TRUE & price_range == 3)$awarded_stars
t2 <- subset(working.set, price_flag == FALSE & price_range == 3)$awarded_stars
t <- t.test(t1, t2, paired=FALSE, var.equal=FALSE)
df3 <- data.frame('Price Range' = '$$$', 'Confidence Interval' = paste(round(t$conf.int[1],4),round(t$conf.int[2],4), sep = ' through '), 'P-Value' = round(t$p.value, 4))

t1 <- subset(working.set, price_flag == TRUE & price_range == 4)$awarded_stars
t2 <- subset(working.set, price_flag == FALSE & price_range == 4)$awarded_stars
t <- t.test(t1, t2, paired=FALSE, var.equal=FALSE)
df4 <- data.frame('Price Range' = '$$$$', 'Confidence Interval' = paste(round(t$conf.int[1],4),round(t$conf.int[2],4), sep = ' through '), 'P-Value' = round(t$p.value, 4))

t1 <- subset(working.set, price_flag == TRUE)$awarded_stars
t2 <- subset(working.set, price_flag == FALSE)$awarded_stars
t <- t.test(t1, t2, paired=FALSE, var.equal=FALSE)
df5 <- data.frame('Price Range' = 'All', 'Confidence Interval' = paste(round(t$conf.int[1],4),round(t$conf.int[2],4), sep = ' through '), 'P-Value' = round(t$p.value, 4))

df <- rbind(df4, df3, df2, df1, df5)
pander(df)

png(file='images/confidence-intervals.png',width=800,height=200)
table <- tableGrob(df, rows = NULL, cols = c('Price Range Tier','Confidence Intervals','P-Value'))

title <- textGrob('Table 1: Price Bias Confidence Intervals',gp=gpar(fontsize=15))
footnote <- textGrob('', x=0, hjust=0,
                     gp=gpar( fontface="italic"))

padding <- unit(0.5,"line")
table <- gtable_add_rows(table, 
                         heights = grobHeight(title) + padding,
                         pos = 0)
table <- gtable_add_rows(table, 
                         heights = grobHeight(footnote)+ padding)
table <- gtable_add_grob(table, list(title, footnote),
                         t=c(1, nrow(table)), l=c(1,2), 
                         r=ncol(table))
grid.newpage()
grid.draw(table)
x <- dev.off(); remove(x); remove(df1); remove(df2); remove(df3); remove(df4); remove(df5); remove(df); remove(t); remove(t1); remove(t2)
remove(table); remove(title); remove(footnote); remove(padding)

# so, on average (this goes in the R markdown)
cat('building average image\n')
dt1 <- working.set[working.set$price_flag == FALSE, ]
dt1 <- data.table(dt1)
dt1 <- dt1[, .(average_awarded_stars = mean(awarded_stars), sd_awarded_stars = sd(awarded_stars))]
dt1$type <- 'Not Mentioned'

dt2 <- working.set[working.set$price_flag == TRUE, ]
dt2 <- data.table(dt2)
dt2 <- dt2[, .(average_awarded_stars = mean(awarded_stars), sd_awarded_stars = sd(awarded_stars))]
dt2$type <- 'Mentioned'

dt3 <- working.set
dt3 <- data.table(dt3)
dt3 <- dt3[, .(average_awarded_stars = mean(awarded_stars), sd_awarded_stars = sd(awarded_stars))]
dt3$type <- 'All'

dt <- rbind(dt1, dt2, dt3)
dt <- dt[, c(3,1,2), with=FALSE]
colnames(dt) <- c('Price Mention Type', 'Average','Standard Deviation')

pander(dt)

png(file='images/averages.png', width=850, height=150)
table <- tableGrob(dt, rows = NULL, cols = c('Price Mention Type', 'Average','Standard Deviation'))

title <- textGrob('Table 2: Price Bias Averages',gp=gpar(fontsize=15))
footnote <- textGrob('', x=0, hjust=0,
                     gp=gpar( fontface="italic"))

padding <- unit(0.5,"line")
table <- gtable_add_rows(table, 
                         heights = grobHeight(title) + padding,
                         pos = 0)
table <- gtable_add_rows(table, 
                         heights = grobHeight(footnote)+ padding)
table <- gtable_add_grob(table, list(title, footnote),
                         t=c(1, nrow(table)), l=c(1,2), 
                         r=ncol(table))
grid.newpage()
grid.draw(table)
x <- dev.off(); remove(x);
remove(table); remove(title); remove(footnote); remove(padding)
remove(dt); remove(dt1); remove(dt2); remove(dt3);

# word cloud for deck
cat('building word cloud image\n')
m <- as.matrix(removeSparseTerms(tdm, sparse = 0.90))
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = TRUE)
# colors
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]
# plot word cloud
png(file='images/word-cloud.png', width=400, height=400)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = FALSE, colors = pal)
x <- dev.off(); remove(x); remove(m); remove(pal); remove(word.freq)