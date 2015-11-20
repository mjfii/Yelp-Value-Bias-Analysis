library(jsonlite)
library(reshape2)
library(lubridate)

cat('loading library of functions to get and clean the Yelp dataset...\n')

get.checkin <- function(tidy.file = character(), raw.file = character()) {

  tidy.file <- paste('tidy data/', tidy.file, sep = '')
  raw.file <- paste('raw data/', raw.file, sep = '')
  
  if (!file.exists(tidy.file)) {
    
    cat('tidy "checkin" file does not exist, building dataset\n')
      
    checkin <- fromJSON(sprintf('[%s]', paste(readLines(raw.file), collapse=',')))
    checkin <- data.frame('business_id' = checkin[,c(3)], checkin$checkin_info)
    checkin <- melt(checkin, id.vars = c(1))
    checkin$variable <- gsub('\\.', ':', gsub('X', '', checkin$variable))
    checkin$hour <- as.numeric(sapply(strsplit(checkin$variable,':'), function(x) x[1]))
    checkin$day <- as.numeric(sapply(strsplit(checkin$variable,':'), function(x) x[2]))
    checkin <- data.frame(checkin[,c(1,4,5,3)])
    checkin$value[is.na(checkin$value)] <- 0L
    checkin <- dcast(checkin, business_id + hour ~ day, sum,value.var = 'value')
    checkin$hour <- as.factor(checkin$hour)
    colnames(checkin) <- c('business_id','hour','sun','mon','tue','wed','thu','fri','sat')
    
    checkin$weekend <- checkin$sun + checkin$sat
    checkin$workweek <- checkin$mon + checkin$tue + checkin$wed + checkin$thu + checkin$fri
    
    saveRDS(checkin, tidy.file)

  } else {
    cat('tidy "checkin" file has been found, reading rds\n')
    checkin <- readRDS(tidy.file)
  }
  
  return(checkin)
}

get.tip <- function(tidy.file = character(), raw.file = character()) {
  
  tidy.file <- paste('tidy data/', tidy.file, sep = '')
  raw.file <- paste('raw data/', raw.file, sep = '')
    
  if (!file.exists(tidy.file)) {
    
    cat('tidy "tip" file does not exist, building dataset\n')
    
    tip <- fromJSON(sprintf('[%s]', paste(readLines(raw.file), collapse=',')))
    tip <- tip[,c(1,3,5,2,4)]
    tip$user_id <- as.factor(tip$user_id)
    tip$business_id <- as.factor(tip$business_id)
    tip$date <- ymd(tip$date)
    colnames(tip) <- c('user_id','business_id','tip_date','tip','tip_likes')
    
    saveRDS(tip, tidy.file) 
    
  } else {
    cat('tidy "tip" file has been found, reading rds\n')
    tip <- readRDS(tidy.file)
  }
  
  return(tip)
}

get.business <- function(tidy.file = character(), raw.file = character()) {
  
  tidy.file <- paste('tidy data/', tidy.file, sep = '')
  raw.file <- paste('raw data/', raw.file, sep = '')
  
  if (!file.exists(tidy.file)) {
    
    cat('tidy "business" file does not exist, building dataset\n')
    
    business <- fromJSON(sprintf('[%s]', paste(readLines(raw.file), collapse=',')))
    business <- business[, -15] # type    
    
    # normalize categories
    category <- business[,c(1,5)]

    for (i in 1:max(sapply(category$categories , function(x) length(x)))) {
      category[, paste('category', i, sep = '_')] <- sapply(category$categories , function(x) x[i])
    }; remove(i)
    
    category <- melt(category[,-2], id.vars = c(1), value.name = 'category')
    category <- category[complete.cases(category), c(1,3)]    
    business <- business[,-5]
    
    # normalize neighborhoods
    neighborhood <- business[,c(1,8)]    

    for (i in 1:max(sapply(neighborhood$neighborhoods, function(x) length(x)))) {
      neighborhood[, paste('neighborhood', i, sep = '_')] <- sapply(neighborhood$neighborhoods , function(x) x[i])
    }; remove(i)
    
    neighborhood <- melt(neighborhood[,-2], id.vars = c(1), value.name = 'neighborhood')
    neighborhood <- neighborhood[complete.cases(neighborhood), c(1,3)]        
    
    business <- business[,-8]
    
    # clean up rest of attributes
    business.hours <- data.frame('sun_open' = business$hours$Sunday$open, 'sun_close' = business$hours$Sunday$close)
    business.hours <- data.frame(business.hours, 'mon_open' = business$hours$Monday$open, 'mon_close' = business$hours$Monday$close)
    business.hours <- data.frame(business.hours, 'tue_open' = business$hours$Tuesday$open, 'tue_close' = business$hours$Tuesday$close)
    business.hours <- data.frame(business.hours, 'wed_open' = business$hours$Wednesday$open, 'wed_close' = business$hours$Wednesday$close)
    business.hours <- data.frame(business.hours, 'thu_open' = business$hours$Thursday$open, 'thu_close' = business$hours$Thursday$close)
    business.hours <- data.frame(business.hours, 'fri_open' = business$hours$Friday$open, 'fri_close' = business$hours$Friday$close)
    business.hours <- data.frame(business.hours, 'sat_open' = business$hours$Saturday$open, 'sat_close' = business$hours$Saturday$close)
    business <- data.frame(business, business.hours)
    business <- business[,-3]
    remove(business.hours)
    
    # odd data in this attributes, list with 'null' assignment and other values
    acc <- business$attributes$`Accepts Credit Cards`
    acc[sapply(acc, is.null)] <- NA
    acc[sapply(acc, function(x) !is.logical(x))] <- NA
    acc <- unlist(acc)
    business$attributes$`Accepts Credit Cards` <- acc
    remove(acc)
    
    attr <- business$attributes
    colnames(attr) <- tolower(colnames(attr))
    colnames(attr) <- gsub(' ', '_', colnames(attr))
    colnames(attr) <- gsub('-', '', colnames(attr))
    colnames(attr) <- gsub('/', '_with_', colnames(attr))
    business <- data.frame(business[,-11], attr)
    remove(attr)
    
    # align the rest of the nested datasets
    ambience <- business$ambience
    colnames(ambience) <- paste('ambience', colnames(ambience), sep = '_')
    business <- data.frame(business[,-36], ambience)
    
    good.for <- business$good_for
    colnames(good.for) <- paste('good_for', colnames(good.for), sep = '_')
    business <- data.frame(business[,-45], good.for)
    
    parking <- business$parking
    colnames(parking) <- paste('parking', colnames(parking), sep = '_')
    business <- data.frame(business[,-45], parking)
    
    music <- business$music
    colnames(music) <- paste('music', colnames(music), sep = '_')
    business <- data.frame(business[,-45], music)
    
    hair <- business$hair_types_specialized_in
    colnames(hair) <- paste('hair_types_specialized', colnames(hair), sep = '_')
    business <- data.frame(business[,-54], hair)
    
    payment <- business$payment_types
    colnames(payment) <- paste('payment_type', colnames(payment), sep = '_')
    business <- data.frame(business[,-56], payment)
    
    dietary <- business$dietary_restrictions
    colnames(dietary) <- paste('dietary_restriction', colnames(dietary), sep = '_')
    business <- data.frame(business[,-56], dietary)
    
    business$alcohol <- as.factor(business$alcohol)
    business$noise_level <- as.factor(business$noise_level)
    business$attire <- as.factor(business$attire)
    business$smoking <- as.factor(business$smoking)
    business$wifi <- as.factor(business$wifi)
    business$byob_with_corkage <- as.factor(business$byob_with_corkage)
    business$ages_allowed <- as.factor(business$ages_allowed)
    business$state <- as.factor(business$state)
    
    saveRDS(business, tidy.file) 
    saveRDS(category, 'tidy data/category.rds')
    saveRDS(neighborhood, 'tidy data/neighborhood.rds')
    
  } else {
    cat('tidy "business" file has been found, reading rds\n')
    business <- readRDS(tidy.file)
    category <- readRDS('tidy data/category.rds')
    neighborhood <- readRDS('tidy data/neighborhood.rds')
  }
  
  return(list('business' = business, 'category' = category, 'neighborhood' = neighborhood))
}

get.review <- function(tidy.file = character(), raw.file = character()) {
  
  tidy.file <- paste('tidy data/', tidy.file, sep = '')
  raw.file <- paste('raw data/', raw.file, sep = '')
  
  if (!file.exists(tidy.file)) {
    
    cat('tidy "review" file does not exist, building dataset\n')
    
    review <- fromJSON(sprintf('[%s]', paste(readLines(raw.file), collapse=',')))

    review <- review[,c(3,5,8,2,6,4,1)]
    colnames(review$votes) <- paste('votes', colnames(review$votes), sep = '_')
    review <- data.frame(review[,-7], review$votes)
    
    saveRDS(review, tidy.file) 
    
  } else {
    cat('tidy "review" file has been found, reading rds\n')
    review <- readRDS(tidy.file)
  }
  
  return(review)
}

get.user <- function(tidy.file = character(), raw.file = character()) {
  
  tidy.file <- paste('tidy data/', tidy.file, sep = '')
  raw.file <- paste('raw data/', raw.file, sep = '')
  
  if (!file.exists(tidy.file)) {
    
    cat('tidy "user" file does not exist, building dataset\n')
    
    user <- fromJSON(sprintf('[%s]', paste(readLines(raw.file), collapse=',')))
    colnames(user$votes) <- paste('votes', colnames(user$votes), sep = '_')
    user <- data.frame(user[,-2], user$votes)
    colnames(user$compliments) <- paste('compliments', colnames(user$compliments), sep = '_')
    user <- data.frame(user[,-9], user$compliments)
    
    friend <- user[,c(4,5)]
    user <- user[,-5]
    elite <- user[c(4,8)]
    user <- user[,-8]
    user <- user[,-7]    
    
    saveRDS(friend,'tidy data/friend.rds')
    saveRDS(elite,'tidy data/elite.rds')
        
    saveRDS(user, tidy.file) 
    
  } else {
    cat('tidy "user" file has been found, reading rds\n')
    user <- readRDS(tidy.file)
  }
  
  return(user)
}

profile.data.frame <- function (pdf = data.frame()) {
  
  density <- sapply(pdf, function(y) sum(length(which(!is.na(y)))))
  sparsity <- sapply(pdf, function(y) sum(length(which(is.na(y)))))
  unique.vals <- sapply(pdf, function(y) length(unique(y)))
  
  profile <- data.frame(density,sparsity,unique.vals)
  profile$cardnality <- round((profile$unique.vals / (profile$density + profile$sparsity)),5)
  profile$class <- sapply(pdf, function(y) class(y))
  
  profile$NonNumbers <- sapply(pdf, function(y) sum(length(which(is.nan(y)))))
  profile$InfinateValues <- sapply(pdf, function(y) sum(length(which(is.infinite(y)))))
  
  
  return(profile)
}

#checkin <- get.checkin('checkin.rds', 'checkin.json')
#tip <- get.tip('tip.rds', 'tip.json')
business <- get.business('business.rds', 'business.json')
category <- business$category
#neighborhood <- business$neighborhood
business <- business$business
review <- get.review('review.rds', 'review.json')
#user <- get.user('user.rds', 'user.json')


