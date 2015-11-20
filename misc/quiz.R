
# View(review[100,])
# nrow(business[ which(business$wifi=='free'),]) / nrow(business[ which(!is.na(business$wifi)),])
# nrow(review[which(review$stars == 5),]) / nrow(review)
# View(user[which(user$compliments$funny > 10000),-6])

# v <- data.frame(user[,c(5,6)], 'compliment'  = user$compliments$funny )
# v[is.na(v)] <- 0L
# v$type <- ifelse(v$fans>1,'greater','less than')
# v$funny <- ifelse(v$compliment>1,'funny','not funny')
# fisher.test(as.matrix(table(v$type,v$funny)))
