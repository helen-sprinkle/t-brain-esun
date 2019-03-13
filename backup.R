source("dirs.R")
source(file.path(aux.dir, "mylib.R"))
mylib(c("data.table", "magrittr"))
# todo:
# v 1) PATH row join
# 2) date 7/v30 days before join
# v3) Y (30 days aggreagtion)
# v4) xgboost
# 5) lstm (???)
load("data/data.RData")
load("data/y.30.RData")


# cust -> path categories
dt <- TBN_CUST_BEHAVIOR[, .(CUST_NO, VISITDATE, PATH1, PATH2, PATH3, PATH4, n)]
dt1 <- melt.data.table(dt, id.vars = c("CUST_NO", "VISITDATE", "n"), 
                       measure.vars = c("PATH1", "PATH2", "PATH3", "PATH4"), 
                       variable.name = "path.level", value.name = "category")
dt2 <- dt1[!is.na(category)&category!="", .(CUST_NO, VISITDATE, category, n)]
dt5 <- date.list[, .(date, ed.date)]
dt2 <- dt2[dt5, on = c("VISITDATE"="date")]
dt2 <- dt2[, .(n=sum(n)), by = .(CUST_NO, ed.date, category)]
dt3 <- dcast(dt2, CUST_NO+ed.date~category, value.var = "n", fun.aggregate = sum)


y.cc <- y.cc[, .(CUST_NO, st.date, Y)]
# dt4 <- dt2[y.cc, Y:=Y, on = c("CUST_NO"="CUST_NO", "ed.date"="st.date")][is.na(Y), Y:=0]
# y <- dt4$Y
# dt4 <- dt4[, -c("CUST_NO", "ed.date", "Y")]


# 1st try:----
# # dt3 <- y.cc[dt3, on = c("CUST_NO"="CUST_NO", "st.date"="ed.date")][is.na(Y), Y:=0][, -c("CUST_NO", "st.date")]
# 1. the less depth (1) the better (fscore)
# 2. lower error results in higher accuracy but lower auc and fscorex10 (best=0.48)
# 2nd try:----
# v6. make path 0/1 feature instead of n (fscorex10=0.52 improved)
# dt3 <- y.cc[dt3, on = c("CUST_NO"="CUST_NO", "st.date"="ed.date")][, Y:=ifelse(is.na(Y), 0, 1)][, -c("CUST_NO", "st.date")]
# 
# 3rd try:----
# v2. trim paths (importance not high)
# dt2 <- dt2[, .(n=sum(n)), by = .(CUST_NO, ed.date, category)]
# dt2[, n.cust:=uniqueN(CUST_NO), by = .(category, ed.date)]
# dt2 <- dt2[n.cust>10]
# dt3 <- dcast(dt2, CUST_NO+ed.date~category, value.var = "n", fun.aggregate = sum)
# dt3 <- y.cc[dt3, on = c("CUST_NO"="CUST_NO", "st.date"="ed.date")][, Y:=ifelse(is.na(Y), 0, 1)][, -c("CUST_NO", "st.date")]
# 4th try:----
# # 1. add features (cust_info, cc.rfm, and other transcations)
# cust_info
y.cc <- y.cc[, .(CUST_NO, Y)]
dt3 <- y.cc[TBN_CIF, on = "CUST_NO"][is.na(Y), Y:=0]
dt3 <- na.omit(dt3)
# bstSparse <- xgboost(data = training, label = train.label, max.depth = 10, scale_pos_weight = 49,
#                      eta = 1, nthread = 4, nrounds = 2000, objective = "binary:logistic",
#                      early_stopping_rounds = 50)
# > Fmeasure
# [1] 0.2721179
# > recall
# [1] 0.4230417
# > precision
# [1] 0.2005647
# # cust_info + behavior
# dt4 <- dt2[category %in% beh.feature]
# dt4 <- dcast(dt4, CUST_NO+ed.date~category, value.var = "n", fun.aggregate = sum)
# dt4 <- dt4[TBN_CIF, on = c("CUST_NO"="CUST_NO")]
# dt4 <- y.cc[dt4, on = c("CUST_NO"="CUST_NO", "st.date"="ed.date")][, Y:=ifelse(is.na(Y), 0, 1)][, -c("CUST_NO", "st.date")]
# dt4[, 2:12:=lapply(.SD,function(x) {ifelse(is.na(x), 0, 1)}), .SDcol=2:12]
# dt4[1:3, 1:5]
# dim(dt4)
# dt4[, .N, by=Y]
# y <- dt4$Y
# x <- dt4[, -c("Y")]
# # > Fmeasure
# # [1] 0.05352961
# # > recall
# # [1] 0.1151079
# # > precision
# # [1] 0.03487358

dt3[1:3, 1:5]
dim(dt3)
dt3[, .N, by=Y]
y <- dt3$Y
x <- dt3[, -c("Y")]
# dt4 <- dt2[, .(n.cust=uniqueN(CUST_NO)), by=category]
# hist(dt4$n.cust)
# setorder(dt4, n.cust, category)

# todo: check Y coverage (table(0/1))

# first try:----
# 1. the less depth (1) the better (fscore)
# 2. lower error results in higher accuracy but lower auc and fscore (best=0.48)
# 
# next possible testing:
# 1. add features (cust_info, cc.rfm, behavior.rfm, and other transcations, most recent xxx)
# v2. trim paths (importance not high)
# 3. different period (60+30 instead of 30+30 or 7+30)
# 4. normalization or take the logarithm 
# 5. try other error function (evaluation)
# v6. make path 0/1 feature instead of n (fscore=0.52 improved)
# 7. consider time series (weekly)
# 8. cross validation k-fold
# 9. testing cut by customer (now by row)
# 10. handle missing values
# 11. PCA
# 12. voting (ensamble)
# 13. ensamble features