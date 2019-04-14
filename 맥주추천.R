# 맥주추천

options(width = 100)
library(data.table)

# download.file("http://dl.dropboxusercontent.com/u/8686172/beer_reviews.zip", 
#              destfile = "beer_reviews.zip", mode = "wb", cacheOK = FALSE)
beers <- read.csv(unz("beer_reviews.zip", "beer_reviews.csv"), stringsAsFactors = FALSE)
beers <- data.table(beers)

review_df <- beers[, list(review_overall, review_aroma, review_appearance, review_palate, 
                          review_taste), by = c("review_profilename", "beer_name", "beer_style")]
setkeyv(review_df, "review_profilename")

# 맥주 추천엔진 구현 방법

# 먼저 입력된 맥주쌍을 모두 리뷰한 사용자들의 리스트를 구한다.
# 각 리뷰어의 맥주쌍간의 리뷰에 대한 유클리드언 거리를 구한다.
# 모든 리뷰어들의 리뷰거리에 대해서 weighted mean을 한다. 
# weight function은 정규분포를 기반으로 하는데 이 이유는 같은 리뷰인데 많은 수치 차이를 보이는 몇몇의 사용자들의 존재 때문이다.
# 유사도 수치를 기반으로 맥주를 나열한다.

beer_similarity <- function(beer1, beer2) {
  reviewers <- review_df[beer_name %chin% c(beer1, beer2), list(num_uniq = length(unique(.SD$beer_name))), 
                         by = "review_profilename"][num_uniq == 2]
  if (nrow(reviewers) <= 1) 
    return(NA)
  reviews <- review_df[reviewers][beer_name %chin% c(beer1, beer2)]
  setkeyv(reviews, c("review_profilename", "beer_name"))
  beer1_reviews <- reviews[, list(review_overall, review_aroma, review_appearance, review_palate, review_taste)]
  beer_dist <- as.matrix(dist(scale(beer1_reviews), diag = TRUE, upper = TRUE))
  two_dist <- vector("numeric", length = (nrow(beer_dist) - 1)/2)
  idx <- 1
  for (i in seq(1, nrow(beer_dist) - 1, by = 2)) {
    two_dist[idx] <- beer_dist[i, i + 1]
    idx <- idx + 1
  }
  names(two_dist) <- unique(reviews$review_profilename)
  sum(dnorm(two_dist, mean = mean(two_dist), sd = sd(two_dist)))/sum(two_dist * dnorm(two_dist, mean = mean(two_dist), 
                                                                                      sd = sd(two_dist)))
}

# http://nbviewer.ipython.org/20a18d52c539b87de2af 결과와 비교
beer1 <- "Coors Light"
beer2 <- c("Natural Light", "Michelob Ultra", "Bud Light", "Blue Moon Belgian White", "Fat Tire Amber Ale", 
           "Dale's Pale Ale", "Guinness Draught", "60 Minute IPA", "Sierra Nevada Pale Ale")
sims <- sapply(beer2, function(x) beer_similarity(beer1, x))
sims[order(sims, decreasing = T)]


beer1 <- "Sierra Nevada Pale Ale"
beer2 <- c("Sierra Nevada Stout", "Samuel Adams Boston Lager", "Indica India Pale Ale", "Suntory The Premium Malt's", 
           "Budweiser", "Guinness Draught", "Pilsner Urquell", "Smithwick's", "Stella Artois", "Tsingtao", "Bud Light")
sims <- sapply(beer2, function(x) beer_similarity(beer1, x))
sims[order(sims, decreasing = T)]

# top 50 맥주만 리뷰
no_of_rev <- beers[, list(num_of_rev = .N), by = c("beer_name", "beer_style")]

no_of_rev <- no_of_rev[order(num_of_rev, decreasing = T)]

sims <- sapply(no_of_rev$beer_name[1:50], function(x) beer_similarity("Samuel Adams Boston Lager", x))

no_of_rev[1:50, `:=`(sims, sims)]

no_of_rev[order(sims, decreasing = T)][1:50]
