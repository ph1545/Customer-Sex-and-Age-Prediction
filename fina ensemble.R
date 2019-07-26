#########################################################################
# Creating ensembles from submission files: Geometric averaging
# For more details, refer to https://mlwave.com/kaggle-ensembling-guide/
########################################################################\
#
test_public <- read.csv("test_public.csv", stringsAsFactors = F)

#
en1<-read.csv("4차-3조.csv")
en1<-en1[!(en1$CUS_ID %in% test_public$CUS_ID),]
dim(en1)
head(en1)
str(en1)
write.csv(en1, "4차-3조-ens.csv",row.names =F)

en2<-read.csv("4차-7조.csv")
en2<-en2[!(en2$CUS_ID %in% test_public$CUS_ID),]
dim(en2)
write.csv(en2, "4차-7조-ens.csv",row.names =F)

en3<-read.csv("4차-9조.csv")
en3<-en3[!(en3$CUS_ID %in% test_public$CUS_ID),]
dim(en3)
write.csv(en3, "4차-9조-ens.csv",row.names =F)

en4<-read.csv("5차-7조.csv")
en4<-en4[!(en4$CUS_ID %in% test_public$CUS_ID),]
dim(en4)
write.csv(en4, "5차-7조-ens.csv",row.names =F)

en5<-read.csv("5차-9조.csv")
en5<-en5[!(en5$CUS_ID %in% test_public$CUS_ID),]
dim(en5)
write.csv(en5, "5차-9조-ens.csv", row.names =F)

##ENS1
##4차-submissions-앙상블
fnames = c("4차-3조-ens.csv","4차-7조-ens.csv","4차-9조-ens.csv")
nf <- 0

for (f in fnames ) {
  if (nf == 0) pred <- read.csv(f)
  else pred <- merge(pred, read.csv(f), by="CUS_ID")
  nf = nf + 1
}

### Calculate geometric mean
nc <- ncol(pred)
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}

### Divide each row by the row sum (required to sum to one)
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

### Save your ensemble submission file
names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred, "ENS1.csv", row.names = F)

 
### Merge your submission files
#A_best
#act2vec 앙상블
fnames = c("act_submission_nnet.csv","act_submission_pda.csv","act_submission_rf.csv")
nf <- 0

for (f in fnames ) {
  if (nf == 0) pred <- read.csv(f)
  else pred <- merge(pred, read.csv(f), by="CUS_ID")
  nf = nf + 1
}

### Calculate geometric mean
nc <- ncol(pred)
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}

### Divide each row by the row sum (required to sum to one)
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

### Save your ensemble submission file
names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred, "A_best.csv", row.names = F)

#S_best1
#site2vec 앙상블1
fnames = c("site_submission_glmnet.csv","site_submission_nnet.csv", "site_submission_rf.csv")

nf <- 0
for (f in fnames ) {
  if (nf == 0) pred <- read.csv(f)
  else pred <- merge(pred, read.csv(f), by="CUS_ID")
  nf = nf + 1
}

### Calculate geometric mean
nc <- ncol(pred)
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}

### Divide each row by the row sum (required to sum to one)
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

### Save your ensemble submission file
names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred, "S_best1.csv", row.names = F)

##S_best
##site2vec 앙상블2
fnames = c("S_best1.csv", "site_submission_xgbTree.csv")

nf <- 0
for (f in fnames ) {
  if (nf == 0) pred <- read.csv(f)
  else pred <- merge(pred, read.csv(f), by="CUS_ID")
  nf = nf + 1
}

### Calculate geometric mean
nc <- ncol(pred)
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}

### Divide each row by the row sum (required to sum to one)
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

### Save your ensemble submission file
names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred, "S_best.csv", row.names = F)


#w2v_best
fnames = c("A_best.csv","S_best.csv")
nf <- 0

for (f in fnames ) {
  if (nf == 0) pred <- read.csv(f)
  else pred <- merge(pred, read.csv(f), by="CUS_ID")
  nf = nf + 1
}

### Calculate geometric mean
nc <- ncol(pred)
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}

### Divide each row by the row sum (required to sum to one)
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

### Save your ensemble submission file
names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred, "w2v_best.csv", row.names = F)

##ENS2
#5차-submissons-앙상블
fnames = c("5차-7조-ens.csv","5차-9조-ens.csv")
nf <- 0

for (f in fnames ) {
  if (nf == 0) pred <- read.csv(f)
  else pred <- merge(pred, read.csv(f), by="CUS_ID")
  nf = nf + 1
}

### Calculate geometric mean
nc <- ncol(pred)
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}

### Divide each row by the row sum (required to sum to one)
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

### Save your ensemble submission file
names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred, "ENS2.csv", row.names = F)

#ENS3
#setwd("C:/한지연/최종/최종앙상블데이터")
#DT,PV-앙상블
fnames = c("PV_xgbTree.csv") 
nf <- 0

for (f in fnames ) {
  if (nf == 0) pred <- read.csv(f)
  else pred <- merge(pred, read.csv(f), by="CUS_ID")
  nf = nf + 1
}

### Calculate geometric mean
nc <- ncol(pred)
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}

### Divide each row by the row sum (required to sum to one)
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

### Save your ensemble submission file
names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred, "ENS3.csv", row.names = F)

##
#ENS4
#ENS1+ENS2-앙상블
fnames = c("ENS1.csv","ENS2.csv")  
nf <- 0

for (f in fnames ) {
  if (nf == 0) pred <- read.csv(f)
  else pred <- merge(pred, read.csv(f), by="CUS_ID")
  nf = nf + 1
}

### Calculate geometric mean
nc <- ncol(pred)
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}

### Divide each row by the row sum (required to sum to one)
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

### Save your ensemble submission file
names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred, "ENS4.csv", row.names = F)


###ENS5
#"ENS3.csv" + "w2v_best.csv" + "train_xgb1247813.csv"
fnames = c("ENS3.csv", "w2v_best.csv","train_xgb1247813.csv")  
nf <- 0

for (f in fnames ) {
  if (nf == 0) pred <- read.csv(f)
  else pred <- merge(pred, read.csv(f), by="CUS_ID")
  nf = nf + 1
}

### Calculate geometric mean
nc <- ncol(pred)
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}

### Divide each row by the row sum (required to sum to one)
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

### Save your ensemble submission file
names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred, "ENS5.csv", row.names = F)

###ENS6
#ENS4
#ENS1+ENS2-앙상블
###ENS5
#"ENS3.csv" + "w2v_best.csv" + "train_xgb1247813.csv"

##ENS6
#ENS4 + ENS5
fnames = c("ENS4.csv", "ENS5.csv")  
nf <- 0

for (f in fnames ) {
  if (nf == 0) pred <- read.csv(f)
  else pred <- merge(pred, read.csv(f), by="CUS_ID")
  nf = nf + 1
}

### Calculate geometric mean
nc <- ncol(pred)
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}

### Divide each row by the row sum (required to sum to one)
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

### Save your ensemble submission file
names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred, "ENS6.csv", row.names = F)

##final
#ENS4 + 5차-7조-ens-앙상블
fnames = c("ENS6.csv","5차-7조-ens.csv") 
nf <- 0

for (f in fnames ) {
  if (nf == 0) pred <- read.csv(f)
  else pred <- merge(pred, read.csv(f), by="CUS_ID")
  nf = nf + 1
}

### Calculate geometric mean
nc <- ncol(pred)
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}

### Divide each row by the row sum (required to sum to one)
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

### Save your ensemble submission file
names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred, "최종-1조-submission.csv", row.names = F)






























































































































































































































































































