source("../auxiliary/mylib.R")
mylib(c("data.table", "magrittr", "summarytools", "ggplot2", "ROCR"))
load("data/data.RData")

# TBN_CC_APPLY_BY_DT----
dt1 <- TBN_CC_APPLY[, .(N=.N, 
                        n.date=uniqueN(TXN_DT)), by = .(CUST_NO)]
TBN_CC_APPLY_BY_DT <- TBN_CC_APPLY[, .(N=.N), by = .(CUST_NO, TXN_DT)]
setorder(TBN_CC_APPLY_BY_DT, CUST_NO, TXN_DT)
TBN_CC_APPLY_BY_DT[dt1, n.date:=n.date, on = c("CUST_NO")]
# TBN_RECENT_DT----
fnm <- fls[6]
TBN_RECENT_DT <- fread(fnm)
TBN_CC_RECENT_APPLY <- TBN_RECENT_DT[!is.na(CC_RECENT_DT), .(CUST_NO, CC_RECENT_DT)]
setnames(TBN_CC_RECENT_APPLY, "CC_RECENT_DT", "TXN_DT")
dt2 <- dt1[TBN_CC_RECENT_APPLY, on = c("CUST_NO")][!is.na(n.date)][, N:=0][, .(CUST_NO, TXN_DT, N, n.date)]
dt3 <- rbindlist(list(TBN_CC_APPLY_BY_DT, dt2))
setorder(dt3, CUST_NO, TXN_DT)
dt3[, `:=` (last=shift(TXN_DT),
            nth=1:.N,
            n.date.new=uniqueN(TXN_DT)), by = CUST_NO]
dt3[, interval:=TXN_DT-last]
dt4 <- dt3[, .N, by = interval]
setorder(dt4,interval)
dt4
hist(dt3$interval[dt3$interval!=0&!is.na(dt3$interval)], br=8000, right = F)


# date period of training and testing setting----
t.period <- 30
p.period <- t.period
train.st.dt <- 9447
train.ed.dt <- train.st.dt + t.period
train.Y.ed.dt <- train.ed.dt + p.period

test.st.dt <- train.ed.dt
test.ed.dt <- test.st.dt + t.period
test.Y.ed.dt <- test.ed.dt + p.period

# training----
trn.dt2 <- TBN_CUST_BEHAVIOR[VISITDATE>=train.st.dt & VISITDATE<train.ed.dt]
trn <- trn.dt2[, .(N=.N,
                   n.date=uniqueN(VISITDATE),
                   r.days=train.ed.dt-max(VISITDATE)), by = CUST_NO]
trn.dt1 <- dt3[TXN_DT<test.st.dt, .(r.days = test.st.dt - max(TXN_DT)), by = CUST_NO]
trn.Y <- TBN_CC_APPLY_BY_DT[TXN_DT>=train.ed.dt & TXN_DT<train.Y.ed.dt, .(Y=1), by=CUST_NO]
trn <- trn.Y[trn, on =c("CUST_NO")]
trn[is.na(Y), Y:=0]
train <- trn[, .(Y,N,n.date,r.days)]

# testing----
tst.dt2 <- TBN_CUST_BEHAVIOR[VISITDATE>=test.st.dt & VISITDATE<test.ed.dt]
tst <- tst.dt2[, .(N=.N,
                   n.date=uniqueN(VISITDATE),
                   r.days=test.ed.dt-max(VISITDATE)), by = CUST_NO]
# tst.dt1 <- dt3[TXN_DT<test.st.dt, .(r.days = test.st.dt - max(TXN_DT)), by = CUST_NO]
tst.Y <- TBN_CC_APPLY_BY_DT[TXN_DT>=test.ed.dt & TXN_DT<test.Y.ed.dt, .(Y=1), by=CUST_NO]
tst <- tst.Y[tst, on =c("CUST_NO")]
tst[is.na(Y), Y:=0]
test <- tst[, .(N,n.date,r.days)]


# # logistic regression
# model <- glm(Y ~.,family=binomial(link='logit'),data=train)
# summary(model)

fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != tst$Y)
print(paste('Accuracy',1-misClasificError))

pr <- prediction(fitted.results, tst$Y)
# ROC curve
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Recall-Precision curve             
prf <- performance(pr, measure = "f")
