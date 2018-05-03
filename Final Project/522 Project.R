expedia <- read.csv("~/Downloads/search_trans.csv")
View(expedia)

# rename columns
promo <- expedia[,31]
reviews <- expedia[,29]
country <- expedia[,1]

# creat new dataframe with just the columns i need
expedia2 <- data.frame(promo, reviews, country)

# turn reviews into a numeric value 
expedia2$reviews <- as.numeric(levels(reviews))[reviews]
# clean data by removing \N and blank values from both columns 
expediaclean <- subset(expedia2, promo == "Y" | promo == "N")
expediaclean1 <- subset(expediaclean, country == "Manhattan")
# remove null values from reviews
expediaclean2 <- subset(expediaclean1, reviews >= 0)
View(expediaclean2)

# sample 1000 or else the model won't run
expediasample <- expediaclean2[sample(nrow(expediaclean2), 1000), ]
View(expediasample)

# run logistic regression 
summary(glm(promo ~ reviews, data = expediasample, family = "binomial"))

par(mrow(2,1))
boxplot(expediasample$review, main = "Boxplot of # of Reviews per Hotel")
