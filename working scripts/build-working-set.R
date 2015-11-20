suppressMessages(library(tm))
suppressMessages(library(SnowballC))
suppressMessages(library(data.table))

business.wrk <- business[business$business_id %in% category[category$category == 'Restaurants', 1], ]
business.wrk <- business.wrk[business.wrk$business_id %in% business.wrk[business.wrk$city == 'Pittsburgh', 1], ]
business.wrk <- business.wrk[!business.wrk$business_id %in% category[category$category == 'Fast Food', 1], ]
business.wrk <- business.wrk[business.wrk$open == TRUE, ]
business.wrk <- business.wrk[!is.na(business.wrk$price_range), ]
review.wrk <- review[review$business_id %in% business.wrk$business_id, ]
review.wrk <- review.wrk[!is.na(review.wrk$text), ]
remove(category)

# build a working data set
cat('getting usable working set\n')
review.wrk <- review.wrk[ , c(3,6,7,8,9,5)]
colnames(review.wrk) <- c('business_id','awarded_stars','votes_funny','votes_useful','votes_cool','raw_text')
business.wrk <- business.wrk[ , c(1,6,30,9)]
colnames(business.wrk) <- c('business_id','restaurant_name','price_range','average_stars')
working.set <- data.table(review.wrk)
setkey(working.set, business_id)
business.wrk <- data.table(business.wrk)
setkey(business.wrk, business_id)
working.set <- data.frame(merge(business.wrk, working.set))
working.set$awarded_stars <- as.numeric(working.set$awarded_stars)
working.set$price_range <- as.factor(working.set$price_range)
working.set$votes_funny <- as.factor(ifelse(working.set$votes_funny > 0, 'Yes', 'No'))
working.set$votes_useful <- as.factor(ifelse(working.set$votes_useful > 0, 'Yes', 'No'))
working.set$votes_cool <- as.factor(ifelse(working.set$votes_cool > 0, 'Yes', 'No'))
working.set$price_label <- as.factor(ifelse(working.set$price_range == 1, '$', ifelse(working.set$price_range == 2, '$$', ifelse(working.set$price_range == 3, '$$$', '$$$$'))))


cat('developing text corpus for text analytics\n')
corp <- Corpus(VectorSource(working.set$raw_text))
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stopwords('english'))
corp <- tm_map(corp, tolower)
corp <- tm_map(corp, stemDocument)
corp <- tm_map(corp, removeWords, c('the','i','we','restaurant','pittsburgh'))
corp <- tm_map(corp, PlainTextDocument)  

corp <- tm_map(corp, content_transformer(gsub), pattern = "prices", replacement = "price")
corp <- tm_map(corp, content_transformer(gsub), pattern = "pricey", replacement = "price")
corp <- tm_map(corp, content_transformer(gsub), pattern = "priced", replacement = "price")
corp <- tm_map(corp, content_transformer(gsub), pattern = "expensive", replacement = "price")
corp <- tm_map(corp, content_transformer(gsub), pattern = "reasonably", replacement = "reasonable")

cat('building working set\n')
working.set <- data.frame(working.set, cleaned_text = unlist(sapply(corp, '[', 'content')), stringsAsFactors=FALSE)
working.set$price_flag <- sapply(working.set$cleaned_text, function(x) grepl('price',x))
remove(business); remove(business.wrk); remove(review); remove(review.wrk)

cat('building term document matrix\n')
tdm <- TermDocumentMatrix(corp, control = list(wordLengths = c(1, 10)))
gc()