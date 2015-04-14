## server.R calls this .R file which contain the guts of the lookup mechanism
library(shiny); library(stringr) 

# load in lookup tables
load(".data/blogs.biDT7.RData"); load(".data/blogs.triDT7.RData"); load(".data/blogs.tetraDT7.RData")
load(".data/news.biDT7.RData"); load(".data/news.triDT7.RData"); load(".data/news.tetraDT7.RData")
load(".data/tweets.biDT7.RData"); load(".data/tweets.triDT7.RData"); load(".data/tweets.tetraDT7.RData")

# as a fail safe, take the 3 most common words across corpora
fail.safe <- c("the", "and", "to")

# parse string to make it as similar to our lookup tables
profanity_list <- readLines("http://www.bannedwordlist.com/lists/swearWords.txt", warn = FALSE)

test.string.1 <-tolower(test.string); test.string.2 <- str_replace_all(test.string.1, "[^[:alnum:][:space:]'|’]", ""); test.string.3 <- iconv(test.string.2, from="UTF-8", to="ascii", sub=""); test.string.4 <- iconv(test.string.3, to="ASCII//TRANSLIT"); test.string.5 <- str_replace_all(test.string.4, "[[:digit:]]+", ""); test.string.6 <- str_replace_all(test.string.5, paste(profanity_list, collapse = "|"), replacement = "")
test.string.7 <- strsplit(test.string.6, " ")

# clean up
rm(test.string); rm(test.string.1); rm(test.string.2); rm(test.string.3); rm(test.string.4); rm(test.string.5); rm(test.string.6)


# create ngrams for lookup
test.string_gram4 <- sapply(test.string.7, tail, 3)
test.string_gram4.gram <- test.string_gram4[1:3, ]
test.string_gram4.gram <- as.character(paste(test.string_gram4.gram, collapse = " "))

test.string_gram3 <- sapply(test.string.7, tail, 2)
test.string_gram3.gram <- test.string_gram3[1:2, ]t
test.string_gram3.gram <- as.character(paste(test.string_gram3.gram, collapse = " "))

test.string_gram2 <- sapply(test.string.7, tail, 1)




## Look-up in blogs
setkey(blogs.tetraDT7); blogs.tetra_test <- blogs.tetraDT7[list(test.string_gram4.gram)][1]; blogs.tetra_test$p[is.na(blogs.tetra_test$p)] <- 0
setkey(blogs.triDT7); blogs.tri_test <- blogs.triDT7[list(test.string_gram3.gram)][1]; blogs.tri_test$p[is.na(blogs.tri_test$p)] <- 0
setkey(blogs.biDT7); blogs.bi_test <- blogs.biDT7[list(test.string_gram2)][1]; blogs.bi_test$p[is.na(blogs.bi_test$p)] <- 0

blogs.tetra_lambda <- (1/3)
blogs.tri_lambda <- (1/3)
blogs.bi_lambda <- (1/3)

blogs_ps <- c((blogs.tetra_test$p * blogs.tetra_lambda), (blogs.tri_test$p * blogs.tri_lambda), (blogs.bi_test$p * blogs.bi_lambda))
blogs_targets <- c(blogs.tetra_test$target, blogs.tri_test$target, blogs.bi_test$target)

blogs_targetDF <- data.frame(blogs_ps, blogs_targets); blogs_target <- as.character(with(blogs_targetDF, blogs_targets[blogs_ps== max(blogs_ps)]))

print(blogs_target)



## Look-up in news
setkey(news.tetraDT7); news.tetra_test <- news.tetraDT7[list(test.string_gram4.gram)][1]; news.tetra_test$p[is.na(news.tetra_test$p)] <- 0
setkey(news.triDT7); news.tri_test <- news.triDT7[list(test.string_gram3.gram)][1]; news.tri_test$p[is.na(news.tri_test$p)] <- 0
setkey(news.biDT7); news.bi_test <- news.biDT7[list(test.string_gram2)][1]; news.bi_test$p[is.na(news.bi_test$p)] <- 0

news.tetra_lambda <- (1/3)
news.tri_lambda <- (1/3)
news.bi_lambda <- (1/3)

news_ps <- c((news.tetra_test$p * news.tetra_lambda), (news.tri_test$p * news.tri_lambda), (news.bi_test$p * news.bi_lambda))
news_targets <- c(news.tetra_test$target, news.tri_test$target, news.bi_test$target)

news_targetDF <- data.frame(news_ps, news_targets); news_target <- as.character(with(news_targetDF, news_targets[news_ps== max(news_ps)]))

print(news_target)



## Look-up in tweets
setkey(tweets.tetraDT7); tweets.tetra_test <- tweets.tetraDT7[list(test.string_gram4.gram)][1]; tweets.tetra_test$p[is.na(tweets.tetra_test$p)] <- 0
setkey(tweets.triDT7); tweets.tri_test <- tweets.triDT7[list(test.string_gram3.gram)][1]; tweets.tri_test$p[is.na(tweets.tri_test$p)] <- 0
setkey(tweets.biDT7); tweets.bi_test <- tweets.biDT7[list(test.string_gram2)][1]; tweets.bi_test$p[is.na(tweets.bi_test$p)] <- 0

tweets.tetra_lambda <- (1/3)
tweets.tri_lambda <- (1/3)
tweets.bi_lambda <- (1/3)

tweets_ps <- c((tweets.tetra_test$p * tweets.tetra_lambda), (tweets.tri_test$p * tweets.tri_lambda), (tweets.bi_test$p * tweets.bi_lambda))
tweets_targets <- c(tweets.tetra_test$target, tweets.tri_test$target, tweets.bi_test$target)

tweets_targetDF <- data.frame(tweets_ps, tweets_targets); tweets_target <- as.character(with(tweets_targetDF, tweets_targets[tweets_ps== max(tweets_ps)]))

print(tweets_target)
