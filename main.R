if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(wordVectors)) install_github("bmschmidt/wordVectors"); library(wordVectors)# Install & load data munging packages
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(caret)) install.packages("caret"); library(caret)
install.packages("matrixStats")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("reshape")
install.packages("caret")
install.packages("ROCR")
install.packages("matrixStats")
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape)
library(caret)
library(ROCR)
library(matrixStats)

#####################
source("masters_파일생성.r")

##############################################################################################1번변수candidatr feature
cs <- read.delim("train_clickstreams.tab", stringsAsFactors = F)
sk <- read.delim("train_searchkeywords.tab", stringsAsFactors = F)
profile <- read.csv("train_profiles.csv", stringsAsFactors = F, header = T)

cs_t <- read.delim("test_clickstreams.tab", stringsAsFactors = F)
sk_t <- read.delim("test_searchkeywords.tab", stringsAsFactors = F)

#########Module 1 : Create Training Set#########

####총 페이지뷰 횟수
cs.V1 <- cs %>% 
  group_by(CUS_ID) %>%
  summarize(ttl_pv = sum(SITE_CNT))

####웹사이트 카테고리별 페이지뷰 비율
cs.V2.1 <- cs %>%
  group_by(CUS_ID, BACT_NM) %>%
  summarize(cs = n()) %>%
  cast(CUS_ID ~ BACT_NM, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V2.1[,2:23] <- cs.V2.1[,2:23]/cs.V2.1[,24]
cs.V2.1 <- cs.V2.1[,-24]
names(cs.V2.1)[-1] <- paste("pv", names(cs.V2.1)[-1], sep="_")

cs.V2.2 <- cs %>%
  group_by(CUS_ID, MACT_NM) %>%
  summarize(cs = n()) %>%
  cast(CUS_ID ~ MACT_NM, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V2.2[,2:208] <- cs.V2.2[,2:208]/cs.V2.2[,209]
cs.V2.2 <- cs.V2.2[,-209]
names(cs.V2.2)[-1] <- paste("mact", names(cs.V2.2)[-1], sep="_")

cs.V2 <- inner_join(cs.V2.1, cs.V2.2, by=("CUS_ID"))

####카테고리에 대한 변동계수
cs.V3 <- cs %>%
  group_by(CUS_ID, BACT_NM) %>%
  summarize(cs = n()) %>%
  cast(CUS_ID ~ BACT_NM, sum) %>%
  mutate(avg_pv = rowMeans(.[2:23]), sd_pv = rowSds(as.matrix(.[2:23]))) %>%
  mutate(coef_var_cat = sd_pv/avg_pv) %>%
  select(CUS_ID, coef_var_cat)

####날짜계산용 전처리
cs$TIME_ID <- ymd_h(cs$TIME_ID)
cs$date <- date(cs$TIME_ID)
cs$year <- year(cs$TIME_ID)
cs$month <- month(cs$TIME_ID)
cs$day <- day(cs$TIME_ID)
cs$time <- hour(cs$TIME_ID)
cs$wkday <- wday(cs$TIME_ID)

####총 방문 일수
cs.V4 <- cs %>%
  group_by(CUS_ID, date) %>%
  summarize(cs = n()) %>%
  group_by(CUS_ID) %>%
  summarize(ttl_vis_day = n())

####월별, 요일별, 시간대별 페이지뷰 비율 
#월별
cs.V5.mth <- cs %>%
  group_by(CUS_ID, month) %>%
  summarize(pv_mth = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ month, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V5.mth[,2:13] <- cs.V5.mth[,2:13]/cs.V5.mth[,14]
cs.V5.mth <- cs.V5.mth[,-14]
names(cs.V5.mth)[-1] <- paste("mth", names(cs.V5.mth)[-1], sep="_")

#요일별
cs.V5.day <- cs %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V5.day[,2:8] <- cs.V5.day[,2:8]/cs.V5.day[,9]
cs.V5.day <- cs.V5.day[,-9]
names(cs.V5.day) <- c("CUS_ID", "sun", "mon", "tue", "wed", "thu", "fri", "sat")

#시간대별
cs.V5.time <- cs %>%
  group_by(CUS_ID, time) %>%
  summarize(pv_time = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ time, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V5.time[,2:25] <- cs.V5.time[,2:25]/cs.V5.time[,26]
cs.V5.time <- cs.V5.time[,-26]
names(cs.V5.time)[-1] <- paste("time", names(cs.V5.time)[-1], sep="_")

#merge
cs.V5 <- inner_join(cs.V5.mth, cs.V5.day, by=("CUS_ID")) %>%
  left_join(cs.V5.time, by="CUS_ID")

####월, 요일, 시간대에 대한 변동계수
#월별
cs.V6.mth <- cs %>%
  group_by(CUS_ID, month) %>%
  summarize(pv_mth = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ month, sum) %>%
  mutate(avg_mth = rowMeans(.[2:13]), sd_mth = rowSds(as.matrix(.[2:13]))) %>%
  mutate(coef_var_mth = sd_mth/avg_mth) %>%
  select(CUS_ID, coef_var_mth)

#요일별
cs.V6.day <- cs %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(avg_day = rowMeans(.[2:8]), sd_day = rowSds(as.matrix(.[2:8]))) %>%
  mutate(coef_var_day = sd_day/avg_day) %>%
  select(CUS_ID, coef_var_day)

#시간대별
cs.V6.time <- cs %>%
  group_by(CUS_ID, time) %>%
  summarize(pv_time = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ time, sum) %>%
  mutate(avg_time = rowMeans(.[2:25]), sd_time = rowSds(as.matrix(.[2:25]))) %>%
  mutate(coef_var_time = sd_time/avg_time) %>%
  select(CUS_ID, coef_var_time)

#merge
cs.V6 <- inner_join(cs.V6.mth, cs.V6.day, by=("CUS_ID")) %>%
  left_join(cs.V6.time, by="CUS_ID")

####뉴스사이트 카테고리별 페이지뷰 비율
cs.V7 <- cs %>% subset(BACT_NM=="뉴스/미디어") %>%
  group_by(CUS_ID, MACT_NM) %>%
  summarize(cs = n()) %>%
  cast(CUS_ID ~ MACT_NM, sum) %>%
  mutate(ttl_pv_news = rowSums(.[2:10]))

cs.V7[,2:10] <- cs.V7[,2:10]/cs.V7[,11]
cs.V7 <- cs.V7[,-11]
names(cs.V7)[-1] <- paste("pv_news", names(cs.V7)[-1], sep="_")

####NA점검
anyNA(cs.V1)
anyNA(cs.V2)
anyNA(cs.V3)
anyNA(cs.V4)
anyNA(cs.V5)
anyNA(cs.V6)
anyNA(cs.V7)

#######최종merge
custsig.train <- profile %>%
  left_join (cs.V1) %>%
  left_join (cs.V2) %>%
  left_join (cs.V3) %>%
  left_join (cs.V4) %>%
  left_join (cs.V5) %>%
  left_join (cs.V6) %>%
  left_join (cs.V7) 

custsig.train[is.na(custsig.train)] <- 0

####변수명 조정
names(custsig.train) <- gsub(" ", "", names(custsig.train))
names(custsig.train) <- gsub("/", "", names(custsig.train))

write.csv(custsig.train, file="3500custsig_train.csv",row.names = F)
head(custsig.train)
#########Module 2 : Create Testing Set#########
####총 페이지뷰 횟수
cs_t.V1 <- cs_t %>% 
  group_by(CUS_ID) %>%
  summarize(ttl_pv = sum(SITE_CNT))

####웹사이트 카테고리별 페이지뷰 비율
cs_t.V2.1 <- cs_t %>%
  group_by(CUS_ID, BACT_NM) %>%
  summarize(cs_t = n()) %>%
  cast(CUS_ID ~ BACT_NM, sum) %>%
  mutate(ttl_pv = cs_t.V1$ttl_pv)

cs_t.V2.1[,2:23] <- cs_t.V2.1[,2:23]/cs_t.V2.1[,24]
cs_t.V2.1 <- cs_t.V2.1[,-24]
names(cs_t.V2.1)[-1] <- paste("pv", names(cs_t.V2.1)[-1], sep="_")

cs_t.V2.2 <- cs_t %>%
  group_by(CUS_ID, MACT_NM) %>%
  summarize(cs_t = n()) %>%
  cast(CUS_ID ~ MACT_NM, sum) %>%
  mutate(ttl_pv = cs_t.V1$ttl_pv)

cs_t.V2.2[,2:208] <- cs_t.V2.2[,2:208]/cs_t.V2.2[,209]
cs_t.V2.2 <- cs_t.V2.2[,-209]
names(cs_t.V2.2)[-1] <- paste("mact", names(cs_t.V2.2)[-1], sep="_")

cs_t.V2 <- inner_join(cs_t.V2.1, cs_t.V2.2, by=("CUS_ID"))

####카테고리에 대한 변동계수
cs_t.V3 <- cs_t %>%
  group_by(CUS_ID, BACT_NM) %>%
  summarize(cs_t = n()) %>%
  cast(CUS_ID ~ BACT_NM, sum) %>%
  mutate(avg_pv = rowMeans(.[2:23]), sd_pv = rowSds(as.matrix(.[2:23]))) %>%
  mutate(coef_var_cat = sd_pv/avg_pv) %>%
  select(CUS_ID, coef_var_cat)

####날짜계산용 전처리
cs_t$TIME_ID <- ymd_h(cs_t$TIME_ID)
cs_t$date <- date(cs_t$TIME_ID)
cs_t$year <- year(cs_t$TIME_ID)
cs_t$month <- month(cs_t$TIME_ID)
cs_t$day <- day(cs_t$TIME_ID)
cs_t$time <- hour(cs_t$TIME_ID)
cs_t$wkday <- wday(cs_t$TIME_ID)

####총 방문 일수
cs_t.V4 <- cs_t %>%
  group_by(CUS_ID, date) %>%
  summarize(cs_t = n()) %>%
  group_by(CUS_ID) %>%
  summarize(ttl_vis_day = n())

####월별, 요일별, 시간대별 페이지뷰 비율 
#월별
cs_t.V5.mth <- cs_t %>%
  group_by(CUS_ID, month) %>%
  summarize(pv_mth = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ month, sum) %>%
  mutate(ttl_pv = cs_t.V1$ttl_pv)

cs_t.V5.mth[,2:13] <- cs_t.V5.mth[,2:13]/cs_t.V5.mth[,14]
cs_t.V5.mth <- cs_t.V5.mth[,-14]
names(cs_t.V5.mth)[-1] <- paste("mth", names(cs_t.V5.mth)[-1], sep="_")

#요일별
cs_t.V5.day <- cs_t %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(ttl_pv = cs_t.V1$ttl_pv)

cs_t.V5.day[,2:8] <- cs_t.V5.day[,2:8]/cs_t.V5.day[,9]
cs_t.V5.day <- cs_t.V5.day[,-9]
names(cs_t.V5.day) <- c("CUS_ID", "sun", "mon", "tue", "wed", "thu", "fri", "sat")

#시간대별
cs_t.V5.time <- cs_t %>%
  group_by(CUS_ID, time) %>%
  summarize(pv_time = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ time, sum) %>%
  mutate(ttl_pv = cs_t.V1$ttl_pv)

cs_t.V5.time[,2:25] <- cs_t.V5.time[,2:25]/cs_t.V5.time[,26]
cs_t.V5.time <- cs_t.V5.time[,-26]
names(cs_t.V5.time)[-1] <- paste("time", names(cs_t.V5.time)[-1], sep="_")

#merge
cs_t.V5 <- inner_join(cs_t.V5.mth, cs_t.V5.day, by=("CUS_ID")) %>%
  left_join(cs_t.V5.time, by="CUS_ID")

####월, 요일, 시간대에 대한 변동계수
#월별
cs_t.V6.mth <- cs_t %>%
  group_by(CUS_ID, month) %>%
  summarize(pv_mth = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ month, sum) %>%
  mutate(avg_mth = rowMeans(.[2:13]), sd_mth = rowSds(as.matrix(.[2:13]))) %>%
  mutate(coef_var_mth = sd_mth/avg_mth) %>%
  select(CUS_ID, coef_var_mth)

#요일별
cs_t.V6.day <- cs_t %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(avg_day = rowMeans(.[2:8]), sd_day = rowSds(as.matrix(.[2:8]))) %>%
  mutate(coef_var_day = sd_day/avg_day) %>%
  select(CUS_ID, coef_var_day)

#시간대별
cs_t.V6.time <- cs_t %>%
  group_by(CUS_ID, time) %>%
  summarize(pv_time = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ time, sum) %>%
  mutate(avg_time = rowMeans(.[2:25]), sd_time = rowSds(as.matrix(.[2:25]))) %>%
  mutate(coef_var_time = sd_time/avg_time) %>%
  select(CUS_ID, coef_var_time)

#merge
cs_t.V6 <- inner_join(cs_t.V6.mth, cs_t.V6.day, by=("CUS_ID")) %>%
  left_join(cs_t.V6.time, by="CUS_ID")


####뉴스사이트 카테고리별 페이지뷰 비율
cs_t.V7 <- cs_t %>% subset(BACT_NM=="뉴스/미디어") %>%
  group_by(CUS_ID, MACT_NM) %>%
  summarize(cs_t = n()) %>%
  cast(CUS_ID ~ MACT_NM, sum) %>%
  mutate(ttl_pv_news = rowSums(.[2:10]))

cs_t.V7[,2:10] <- cs_t.V7[,2:10]/cs_t.V7[,11]
cs_t.V7 <- cs_t.V7[,-11]
names(cs_t.V7)[-1] <- paste("pv_news", names(cs_t.V7)[-1], sep="_")


####NA점검
anyNA(cs_t.V1)
anyNA(cs_t.V2)
anyNA(cs_t.V3)
anyNA(cs_t.V4)
anyNA(cs_t.V5)
anyNA(cs_t.V6)
anyNA(cs_t.V7)

custsig.test <- cs_t.V1 %>%
  left_join (cs_t.V2) %>%
  left_join (cs_t.V3) %>%
  left_join (cs_t.V4) %>%
  left_join (cs_t.V5) %>%
  left_join (cs_t.V6) %>%
  left_join (cs_t.V7) 

custsig.test[is.na(custsig.test)] <- 0

####변수명 조정
names(custsig.test) <- gsub(" ", "", names(custsig.test))
names(custsig.test) <- gsub("/", "", names(custsig.test))

write.csv(custsig.test, file="3500custsig_test.csv",row.names = F)

rm(list=ls())
custsig.train <- read.csv("3500custsig_train.csv", stringsAsFactors = F)
custsig.test <- read.csv("3500custsig_test.csv", stringsAsFactors = F)

####################################################################################2번변수 여성이들어간 ACT_NM별 비율

install.packages("readxl")
library("readxl") 
train_profiles<- read.csv("train_profiles.csv", stringsAsFactors = F, header = T)
train_clickstreams = read.delim('train_clickstreams.tab', stringsAsFactors = FALSE)
test_clickstreams = read.delim('test_clickstreams.tab', stringsAsFactors = FALSE)

merge_data = merge(train_clickstreams, train_profiles, by = 'CUS_ID')

# head(merge_data)

select_data <- subset(merge_data, merge_data$ACT_NM %in% unique(merge_data$ACT_NM)[grep("여성" , unique(merge_data$ACT_NM))])
head(select_data)


#비율
library(reshape2)
one_cast <- dcast(select_data, CUS_ID ~ ACT_NM, value.var = "ST_TIME" , sum)
# head(one_cast)
merge_one <- merge (x = unique(train_clickstreams[1]), y = one_cast, all.x = TRUE)
merge_one[is.na(merge_one)]<-0
# head(merge_one)
ratio_data <- prop.table(as.matrix(merge_one[-1]), margin = 1)
ratio_data[is.nan(ratio_data)] <-0
head(ratio_data)
str(ratio_data)
merge_twice <- cbind (unique(train_clickstreams[1]), ratio_data)
head(merge_twice)
str(merge_twice)


#변동계수
library(plyr)
cv = function(x){
  sd(x)/mean(x)
}

two <- ddply(select_data, "CUS_ID", summarise, asseumewomen_cv = cv(ST_TIME))
merge_data <- merge (x= unique(train_clickstreams[1]), y =  two, all.x = TRUE, by = 'CUS_ID')

merge_data[is.na(merge_data)] <- 0
head(merge_data)
str(merge_data)

train_assumewomen_data<-merge(merge_twice,merge_data,by="CUS_ID")
write.csv(train_assumewomen_data, file="3500train_assumewomen_data.csv",row.names = F)
#with Test_clickstreams

select_data <- subset(test_clickstreams,test_clickstreams$ACT_NM %in% unique(test_clickstreams$ACT_NM)[grep("여성" , unique(test_clickstreams$ACT_NM))])

#비율
one_cast <- dcast(select_data, CUS_ID ~ ACT_NM, value.var = "ST_TIME" , sum)
# head(one_cast)
merge_one <- merge (x = unique(test_clickstreams[1]), y = one_cast, all.x = TRUE)
merge_one[is.na(merge_one)]<-0
# head(merge_one)
ratio_data <- prop.table(as.matrix(merge_one[-1]), margin = 1)
ratio_data[is.nan(ratio_data)] <-0
head(ratio_data)
str(ratio_data)
merge_twice <- cbind (unique(test_clickstreams[1]), ratio_data)
head(merge_twice)
str(merge_twice)

#변동계수

cv = function(x){
  sd(x)/mean(x)
}
two <- ddply(select_data, "CUS_ID", summarise, asseumewomen_cv = cv(ST_TIME))
merge_data <- merge (x= unique(test_clickstreams[1]), y =  two, all.x = TRUE, by = 'CUS_ID')
merge_data[is.na(merge_data)] <- 0
head(merge_data)
str(merge_data)

test_assumewomen_data<-merge(merge_twice,merge_data,by="CUS_ID")
write.csv(test_assumewomen_data, file="3500test_assumewomen_data.csv",row.names = F)

rm(list=ls())
train_assumewomen_data <- read.csv("train_assumewomen_data.csv", stringsAsFactors = F)
test_assumewomen_data <- read.csv("test_assumewomen_data.csv", stringsAsFactors = F)

#################################################################################4번변수 site(vector200)+act(vector200)
# train data
cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)

# test data
tr.t.dt <- fread("test_clickstreams.tab"); tr.t.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(tr.t.dt, CUS_ID)

###### Make Corpus (sites sentences)

f <- function(x, min) {
  # Select sites accessed min times and more  
  grp <- cs.dt[CUS_ID==x, GROUP]
  itemfreq <- table(md.dt[CUS_ID==x, ACT_NM])
  fitems <- itemfreq[itemfreq >= min]
  act <- names(fitems)
  # Replace blanks in ACT_NM with underscore
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(123)
  # Boost transactions 
  as.vector((sapply(1:50, function(x) c(grp, sample(act)))))
}
items <- unlist(sapply(cs.dt$CUS_ID, f, 1)) # best performed when min = 1
write.table(items, "items50_actnm.txt", eol = " ", quote = F, row.names = F, col.names = F)

##### Build trans2vec model
set.seed(123)
model = train_word2vec("items50_actnm.txt","50vec200_actnm.bin",vectors=200,threads=1,window=5,cbow=1,negative_samples=10,iter=5,force = T)
model <- read.binary.vectors("50vec200_actenm.bin") # reload the pre-trained word2vec model 

" 
(word2vec performance) The main choices to make are:
- architecture: skip-gram (slower, better for infrequent words) vs CBOW (fast)
- the training algorithm: hierarchical softmax (better for infrequent words) vs negative sampling (better for frequent words, better with low dimensional vectors)
- sub-sampling of frequent words: can improve both accuracy and speed for large data sets (useful values are in range 1e-3 to 1e-5)
- dimensionality of the word vectors: usually more is better, but not always
- context (window) size: for skip-gram usually around 10, for CBOW around 5
"

##### Prediction using trans2vec + classifaction methods

### Make features (mean vector)
# Get mean vector
g <- function(x, dt) {
  items <- dt[CUS_ID==x, ACT_NM]
  mvector <- model[[items, average=T]]
  return(mvector)
}
# for train data
fv <- t(sapply(cs.dt$CUS_ID, g, md.dt))
train <- cbind(data.frame(CUS_ID=cs.dt$CUS_ID), as.data.frame(fv), data.frame(GROUP=make.names(cs.dt$GROUP)))
# for test data
test.CUS_ID <- unique(tr.t.dt$CUS_ID)
fv <- t(sapply(test.CUS_ID, g, tr.t.dt))
test <- cbind(data.frame(CUS_ID=test.CUS_ID), as.data.frame(fv))

###### Make Corpus (sites sentences)

f <- function(x, min) {
  # Select sites accessed min times and more  
  grp <- cs.dt[CUS_ID==x, GROUP]
  itemfreq <- table(md.dt[CUS_ID==x, SITE_NM])
  fitems <- itemfreq[itemfreq >= min]
  act <- names(fitems)
  # Replace blanks in ACT_NM with underscore
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(123)
  # Boost transactions 
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))
}
items <- unlist(sapply(cs.dt$CUS_ID, f, 1)) # best performed when min = 1
write.table(items, "items20_sitenm.txt", eol = " ", quote = F, row.names = F, col.names = F)

##### Build trans2vec model
set.seed(123)
model = train_word2vec("items20_sitenm.txt","20vec200_sitenm.bin",vectors=200,threads=1,window=5,cbow=1,negative_samples=10,iter=5,force = T)
model <- read.binary.vectors("20vec200_sitenm.bin") # reload the pre-trained word2vec model 

" 
(word2vec performance) The main choices to make are:
- architecture: skip-gram (slower, better for infrequent words) vs CBOW (fast)
- the training algorithm: hierarchical softmax (better for infrequent words) vs negative sampling (better for frequent words, better with low dimensional vectors)
- sub-sampling of frequent words: can improve both accuracy and speed for large data sets (useful values are in range 1e-3 to 1e-5)
- dimensionality of the word vectors: usually more is better, but not always
- context (window) size: for skip-gram usually around 10, for CBOW around 5
"

##### Prediction using trans2vec + classifaction methods

### Make features (mean vector)
# Get mean vector
g <- function(x, dt) {
  items <- dt[CUS_ID==x, SITE_NM]
  mvector <- model[[items, average=T]]
  return(mvector)
}
# for train data
fv <- t(sapply(cs.dt$CUS_ID, g, md.dt))
train1 <- cbind(data.frame(CUS_ID=cs.dt$CUS_ID), as.data.frame(fv), data.frame(GROUP=make.names(cs.dt$GROUP)))
# for test data
test.CUS_ID <- unique(tr.t.dt$CUS_ID)
fv <- t(sapply(test.CUS_ID, g, tr.t.dt))
test1 <- cbind(data.frame(CUS_ID=test.CUS_ID), as.data.frame(fv))
###데이터정리
train<-train[-202]
train1<-train1[-202]
train_actsite<-merge(train,train1,by="CUS_ID")
test_actsite<-merge(test,test1,by="CUS_ID")

train_actsite<-write.csv(train_actsite, file="3500train_actsite.csv",row.names = F)
test_actsite<-write.csv(test_actsite, file="3500test_actsite.csv",row.names = F)

rm(list=ls())
train_actsite <- read.csv("train_actsite.csv", stringsAsFactors = F)
test_actsite <- read.csv("test_actsite.csv", stringsAsFactors = F)


#################################################################################################7번변수키워드 변수정리
#######트레인 키워드 정리#######

cs.dt <- read.csv("train_profiles.csv")
train_searchkeywords<-read.delim("train_searchkeywords.tab", stringsAsFactors = F)
head(train_searchkeywords)
ts<-train_searchkeywords
a<-aggregate(ts[2],by=list(CUS_ID=ts$CUS_ID),length)
colnames(a)[2]<-"TOTS"
b<-aggregate(ts[2],by=list(CUS_ID=ts$CUS_ID),sd)
b$QRY_CNT<-ifelse(is.na(b$QRY_CNT),0,b$QRY_CNT)
c<-aggregate(ts[2],by=list(CUS_ID=ts$CUS_ID),mean)
d<-data.frame(CUS_ID=a$CUS_ID,SCOV=b$QRY_CNT/c$QRY_CNT)

trainkey<-merge(cs.dt,a,by="CUS_ID",all=T)
trainkey<-merge(trainkey,d,by="CUS_ID",all=T)
trainkey<-trainkey[-2]
trainkey[is.na(trainkey)]<-0
sum(is.na(trainkey))


#######테스트 키워드 정리#######

test_searchkeywords<-read.delim("test_searchkeywords.tab", stringsAsFactors = F)
ts1<-test_searchkeywords
a<-aggregate(ts1[2],by=list(CUS_ID=ts1$CUS_ID),length)
colnames(a)[2]<-"TOTS"
library(dplyr)
SCOV<- ts1 %>% group_by(CUS_ID) %>% summarise(SCOV=sd(QRY_CNT)/mean(QRY_CNT))
testkey<-data.frame(2501:5000)
names(testkey)<-"CUS_ID"
testkey<-merge(testkey,a,by="CUS_ID",all=T)
testkey<-merge(testkey,SCOV,by="CUS_ID",all.x=T)
testkey[is.na(testkey)]<-0
sum(is.na(testkey))


write.csv(trainkey, file="3500trainkey.csv",row.names = F)
write.csv(testkey, file="3500testkey.csv",row.names = F)
rm(list=ls())
trainkey <- read.csv("trainkey.csv", stringsAsFactors = F)
testkey <- read.csv("testkey.csv", stringsAsFactors = F)

#########################################################################################8번카테고리별 자주가는 사이트
#for train
library(data.table)
library(dplyr)
library(reshape)
train_clickstreams<-fread("train_clickstreams.tab");train_clickstreams[,CUS_ID:= as.numeric(CUS_ID)]
tr<-train_clickstreams
cate1<-data.frame(CUS_ID=1:2500)
c1<-tr %>% filter(BACT_NM=="쇼핑") %>% group_by(CUS_ID,ACT_NM) %>% summarise(N=n())
c11<-aggregate(c1[3],by=list(CUS_ID=c1$CUS_ID),max)
summary(c11$N)
c12<-c11[!c11$N<=mean(c11$N),]
am<-merge(c12,c1,by.x=c("CUS_ID","N"),by.y = c("CUS_ID","N"),all=F,all.x=T,all.y=F)
dim(table(am$ACT_NM))
am<-am[order(am$CUS_ID),]
rownames(am)<-1:dim(am)[1]
am<-am[!duplicated(am$CUS_ID),]
am<-am[,-2]
b<-c("오픈마켓","여성의류쇼핑몰","소셜커머스","종합쇼핑몰","남성의류쇼핑몰","중고차쇼핑몰","종합도서쇼핑몰")
am<-am[am$ACT_NM %in% b,]
colnames(am)[2]<-"SITE11"
cate1<-merge(cate1,am,by="CUS_ID",all.x=T)
cate1$SITE11<-ifelse(is.na(cate1$SITE11),"NONE",cate1$SITE11)
cate1$SITE11<-factor(cate1$SITE11,labels=1:8)
head(cate1)
#for test
test_clickstreams<-read.table("test_clickstreams.tab",sep="\t",header = T,stringsAsFactors = F)
tr<-test_clickstreams
cate2<-data.frame(CUS_ID=2501:5000)

c1<-tr %>% filter(BACT_NM=="쇼핑") %>% group_by(CUS_ID,ACT_NM) %>% summarise(N=n())
c11<-aggregate(c1[3],by=list(CUS_ID=c1$CUS_ID),max)
summary(c11$N)
c12<-c11[!c11$N<=mean(c11$N),]
am<-merge(c12,c1,by.x=c("CUS_ID","N"),by.y = c("CUS_ID","N"),all=F,all.x=T,all.y=F)
dim(table(am$ACT_NM))
am<-am[order(am$CUS_ID),]
rownames(am)<-1:dim(am)[1]
am<-am[!duplicated(am$CUS_ID),]
am<-am[,-2]
b<-c("오픈마켓","여성의류쇼핑몰","소셜커머스","종합쇼핑몰","남성의류쇼핑몰","중고차쇼핑몰","종합도서쇼핑몰")
am<-am[am$ACT_NM %in% b,]
colnames(am)[2]<-"SITE11"
cate2<-merge(cate2,am,by="CUS_ID",all.x=T)
cate2$SITE11<-ifelse(is.na(cate2$SITE11),"NONE",cate2$SITE11)
cate2$SITE11<-factor(cate2$SITE11,labels=1:8)
head(cate2)

tp<-read.csv("test_profiles.csv", stringsAsFactors = F)
tp2<-read.csv("train_profiles.csv", stringsAsFactors = F)
cate3 <- subset(cate2, cate2$CUS_ID %in% tp$CUS_ID)
cate4 <- subset(cate2, cate2$CUS_ID %in% tp2$CUS_ID)
cate1<-rbind(cate1,cate4)
cate2<-cate3
write.csv(cate1,"3500tr_fre.csv",row.names = F)
write.csv(cate2,"3500tt_fre.csv",row.names = F)
rm(list=ls())
train8<- read.csv("3500tr_fre.csv", stringsAsFactors = F)
test8 <- read.csv("3500tt_fre.csv", stringsAsFactors = F)
###############################################################################################13 site2vec site_time
cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)


###### Make sites sentences ## 방문한 소분류를 모음
f <- function(x, t) {
  grp <- md.dt[CUS_ID==x, GROUP][1]
  itemfreq <- table(md.dt[CUS_ID==x,  SITE_NM])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))  ##방문한 사이트를 20번 샘플링 --> 5만개의 문장을 만들어서 학습시킴.
}
items <- unlist(sapply(cs.dt$CUS_ID, f, 2))
write.table(items, "items.txt", eol = " ", quote = F, row.names = F, col.names = F)


##### Train site2vec model
model = train_word2vec("items.txt","20vec300_siten.bin",vectors=300,threads=1,window=5,cbow=1,iter=5,negative_samples=10, force = T)
model <- read.binary.vectors("20vec300_sitenm.bin") # reload the model. 
df<-data.frame()
for (i in 1:2500 ){
  itemfreq <- table(tr.dt[CUS_ID==i, SITE_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","AF20","AF30","AF40","AM20","AM30","AM40")


####테스트 사이트 워투백 모델 적용####
tr2<-fread("test_clickstreams.tab"); tr2[,CUS_ID:= as.numeric(CUS_ID)]
df1<-data.frame()
for (i in 2501:5000 ){
  itemfreq <- table(tr2[CUS_ID==i, SITE_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df1<-rbind(df1,sim1)
}
colnames(df1)<-c("CUS_ID","SF20","SF30","SF40","SM20","SM30","SM40")

train_site_site2vec<-df
test_site_site2vec<-df1
train_site_site2vec<-read.csv("train_site_site2vec.csv", stringsAsFactors = F)
test_site_site2vec<-read.csv("test_site_site2vec.csv", stringsAsFactors = F)
tp<-read.csv("test_profiles.csv", stringsAsFactors = F)
tp2<-read.csv("train_profiles.csv", stringsAsFactors = F)
test_site_site2vec2 <- subset(test_site_site2vec, test_site_site2vec$CUS_ID %in% tp$CUS_ID)
train_site_site2vec2 <- subset(test_site_site2vec, test_site_site2vec$CUS_ID %in% tp2$CUS_ID)
train13<-rbind(train_site_site2vec,train_site_site2vec2)
test13<-test_site_site2vec2

############################################################################################################
train1<- read.csv("3500custsig_train.csv", stringsAsFactors = F)
test1<- read.csv("3500custsig_test.csv", stringsAsFactors = F)
train2 <- read.csv("3500train_assumewomen_data.csv", stringsAsFactors = F)
test2 <- read.csv("3500test_assumewomen_data.csv", stringsAsFactors = F)
train4 <- read.csv("3500train_actsite.csv", stringsAsFactors = F)
test4<- read.csv("3500test_actsite.csv", stringsAsFactors = F)
train7<- read.csv("3500trainkey.csv", stringsAsFactors = F)
test7<- read.csv("3500testkey.csv", stringsAsFactors = F)
train8<- read.csv("3500tr_fre.csv", stringsAsFactors = F)
test8<- read.csv("3500tt_fre.csv", stringsAsFactors = F)
head(train4[401])

################################################################################################################
train_final<-merge(train1,train2,by="CUS_ID")
train_final<-merge(train_final,train4,by="CUS_ID")
train_final<-merge(train_final,train7,by="CUS_ID")
train_final<-merge(train_final,train8,by="CUS_ID")
train_final<-merge(train_final,train13,by="CUS_ID")

test_final<-merge(test1,test2,by="CUS_ID")
test_final<-merge(test_final,test4,by="CUS_ID")
test_final<-merge(test_final,test7,by="CUS_ID")
test_final<-merge(test_final,test8,by="CUS_ID")
test_final<-merge(test_final,test13,by="CUS_ID")


write.csv(train_final, file="3500train_final.csv",row.names = F)
write.csv(test_final, file="3500test_final.csv",row.names = F)
sum(is.na(test_final))
test_final[is.na(test_final)]<-0


########################################################################################################################################

############################################################################################################################

trainsig<-train
testsig<-test
testsig[is.na(testsig)]<-0
sum(is.na(trainsig))
sum(is.na(testsig))
trainsig$GROUP = as.character(trainsig$GROUP)
trainsig$GROUP[trainsig$GROUP == "F20-"] = "F20"
trainsig$GROUP[trainsig$GROUP == "F40+"] = "F40"
trainsig$GROUP[trainsig$GROUP == "M20-"] = "M20"
trainsig$GROUP[trainsig$GROUP == "M40+"] = "M40"
str(trainsig)
control = trainControl(method = "repeatedcv", number = 5, repeats = 1, 
                       classProbs=T, summaryFunction=mnLogLoss, verboseIter = FALSE)

#train XGboost model and save
model_xgb <- caret::train(GROUP ~ ., data = trainsig, 
                          method = "glmnet", metric = "logLoss", trControl = control)


model_xgb
xgbImp <- varImp(model_xgb, scale = FALSE)
predicted <- predict(model_xgb, testsig, type="prob")
write.csv(cbind(testsig$CUS_ID,predicted), "train_xgb1247813.csv", row.names = F)


source("PV_DT_modeling.r")
source("word2vec.r")
source("최종-1조-앙상블.r")