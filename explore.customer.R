source("dirs.R")
source(file.path(aux.dir, "mylib.R"))
mylib(c("data.table", "magrittr"))
load(file.path(data.dir, "data.RData"))

# TBN_CUST_BEHAVIOR (customer behavior)----
dt1 <- TBN_CUST_BEHAVIOR[, .(n.date=uniqueN(VISITDATE),
                        N=.N), by = CUST_NO]
dt1[n.date!=N]
dt1[, avg.n.page:=N/n.date]
# plot(dt1$n.date, dt1$avg.n.page)
dt2 <- dt1[, .N, by = n.date] %>% setorder(., n.date)
dt2
# TBN_CC_APPLY----
dt1 <- TBN_CC_APPLY[, .(n.date=uniqueN(TXN_DT),
                        N=.N), by = CUST_NO]
# dt1[n.date!=N]
dt2 <- dt1[, .N, by = n.date] %>% setorder(., n.date)
dt2
# dt2[, `:=` (last=shift(TXN_DT),
#             nth=1:.N), by = CUST_NO]
# dt2[, interval:=TXN_DT-last]
# dt3 <- dt2[, .N, by = interval] 
# setorder(dt3,interval)
# dt3
# hist(dt2$interval[dt2$interval!=0&!is.na(dt2$interval)], br=120, right = F)
# TBN_LN_APPLY----
dt1 <- TBN_LN_APPLY[, .(n.date=uniqueN(TXN_DT),
                        N=.N), by = CUST_NO]
dt1[n.date!=N]
dt2 <- dt1[, .N, by = n.date] %>% setorder(., n.date)
dt2
# TBN_FX_TXN----
dt1 <- TBN_FX_TXN[, .(n.date=uniqueN(TXN_DT),
                        N=.N), by = CUST_NO]
dt1[n.date!=N]
dt2 <- dt1[, .N, by = n.date] %>% setorder(., n.date)
dt2
# TBN_WM_TXN----
dt1 <- TBN_WM_TXN[, .(n.date=uniqueN(TXN_DT),
                      N=.N), by = CUST_NO]
dt1[n.date!=N]
dt2 <- dt1[, .N, by = n.date] %>% setorder(., n.date)
dt2




# specific customer
# dt1 <- TBN_CC_APPLY[, .(n.date=uniqueN(TXN_DT),
#                         N=.N), by = CUST_NO]
# dt1[n.date>2 | N>2]
# cust <- "J2CXL-NRIFNJLFPI"
# dt1 <- TBN_CC_APPLY[, .(n.date=uniqueN(TXN_DT),
#                         N=.N), by = CUST_NO]
# dt1
# cust <- "J2CXL-NRIFNJLFPI"
cust <- "HXD9GCY3SPIYLJS0"

TBN_CIF[CUST_NO==cust]
TBN_RECENT_DT[CUST_NO==cust]
TBN_CUST_BEHAVIOR[CUST_NO==cust]
TBN_CC_APPLY[CUST_NO==cust]
TBN_LN_APPLY[CUST_NO==cust]
TBN_FX_TXN[CUST_NO==cust]
TBN_WM_TXN[CUST_NO==cust]


#
tmp <- TBN_CUST_BEHAVIOR[, .(N=.N,n.cust=uniqueN(CUST_NO)), by=PAGE.new]
setorder(tmp, -n.cust)
View(tmp)

#
tmp1 <- TBN_CUST_BEHAVIOR[, .(N=.N,n.cust=uniqueN(CUST_NO)), by=.(PATH1, PATH2)]
setorder(tmp1, n.cust)
View(tmp1)

tmp2 <- TBN_CUST_BEHAVIOR[, .(N=.N,n.cust=uniqueN(CUST_NO)), by=.(PATH2, PATH3)]
setorder(tmp2, n.cust)
View(tmp2)


source("dirs.R")
source(file.path(aux.dir, "mylib.R"))
mylib(c("data.table", "magrittr", "xgboost", "summarytools"))
# - raw input: "fls","TBN_RECENT_DT","TBN_CIF","TBN_CUST_BEHAVIOR",
#                    "TBN_CC_APPLY","TBN_LN_APPLY","TBN_FX_TXN","TBN_WM_TXN"
load(file.path(data.dir, "data.RData"))

# see all data distribution among customers
dt.beh <- TBN_CUST_BEHAVIOR[, .(HasBehavior.N=.N), by=.(CUST_NO)]
dt <- merge(TBN_CIF, dt.beh, all=TRUE)
dt.cc <- TBN_CC_APPLY[, .(HasCC.N=.N), by=.(CUST_NO)]
dt.fx <- TBN_FX_TXN[, .(HasFX.N=.N, FX_TXN_AMT=sum(FX_TXN_AMT)), by=.(CUST_NO)]
setorder(TBN_LN_APPLY, CUST_NO, LN_USE)
dt.ln <- TBN_LN_APPLY[, .(HasLN.N=.N, LN_AMT=sum(LN_AMT), LN_USE=paste(unique(LN_USE), collapse = "_")), by=.(CUST_NO)]
dt.wm <- TBN_WM_TXN[, .(HasWM.N=.N, WM_TXN_AMT=sum(WM_TXN_AMT)), by=.(CUST_NO)]
dt <- merge(dt, dt.cc, all=TRUE)
dt <- merge(dt, dt.fx, all=TRUE)
dt <- merge(dt, dt.ln, all=TRUE)
dt <- merge(dt, dt.wm, all=TRUE)
dt <- merge(dt, TBN_RECENT_DT, all=TRUE)
# dt[, n:=.N, by=.(CUST_NO)]
# dt[n>1]
# dt[,n:=NULL]
dfs <- dfSummary(dt)
view(dfs)
output.fnm <- "TBN_all.html"
print(dfs, file = output.fnm)
file.rename(output.fnm, file.path("summary_and_reports", output.fnm))


# see all data distribution among y_zero customers (to be predicted)
y_zero <- fread(file.path(data.dir, "raw_input", "TBN_Y_ZERO.csv"))
dt.beh <- TBN_CUST_BEHAVIOR[, .(HasBehavior.N=.N), by=.(CUST_NO)]
dt <- merge(TBN_CIF, dt.beh, all=TRUE)
dt.cc <- TBN_CC_APPLY[, .(HasCC.N=.N), by=.(CUST_NO)]
dt.fx <- TBN_FX_TXN[, .(HasFX.N=.N, FX_TXN_AMT=sum(FX_TXN_AMT)), by=.(CUST_NO)]
setorder(TBN_LN_APPLY, CUST_NO, LN_USE)
dt.ln <- TBN_LN_APPLY[, .(HasLN.N=.N, LN_AMT=sum(LN_AMT), LN_USE=paste(unique(LN_USE), collapse = "_")), by=.(CUST_NO)]
dt.wm <- TBN_WM_TXN[, .(HasWM.N=.N, WM_TXN_AMT=sum(WM_TXN_AMT)), by=.(CUST_NO)]
dt <- merge(dt, dt.cc, all=TRUE)
dt <- merge(dt, dt.fx, all=TRUE)
dt <- merge(dt, dt.ln, all=TRUE)
dt <- merge(dt, dt.wm, all=TRUE)
dt <- merge(dt, TBN_RECENT_DT, all=TRUE)
dt1 <- merge(dt, y_zero, by = "CUST_NO", all.y=T)
uniqueN(dt1$CUST_NO)
dfs <- dfSummary(dt1)
view(dfs)
output.fnm <- "TBN_y_zero.html"
print(dfs, file = output.fnm)
file.rename(output.fnm, file.path("summary_and_reports", output.fnm))

save(list = c("dt", "dt1"), file=file.path(data.dir, "data.coverage.RData"))

# see recent action
# concate action
dt.beh <- TBN_CUST_BEHAVIOR[, .(n=sum(n), type="01browse"), by=.(CUST_NO, VISITDATE)]
setnames(dt.beh, "VISITDATE", "TXN_DT")
dt.cc <- TBN_CC_APPLY[, .(n=.N, type="02cc"), by=.(CUST_NO,TXN_DT)]
dt.fx <- TBN_FX_TXN[, .(n=.N, type="03fx"), by=.(CUST_NO,TXN_DT)]
dt.ln <- TBN_LN_APPLY[, .(n=.N, type="05ln"), by=.(CUST_NO,TXN_DT)]
dt.wm <- TBN_WM_TXN[, .(n=.N, type="04wm"), by=.(CUST_NO,TXN_DT)]
dt.st <- TBN_CIF[,.(CUST_NO,TXN_DT=CUST_START_DT,n=1,type="st.dt")]
setnames(TBN_RECENT_DT, colnames(TBN_RECENT_DT), c("CUST_NO","02cc","03fx","05ln","04wm"))
dt.recent <- melt.data.table(TBN_RECENT_DT,id.vars = "CUST_NO",
                             variable.name = "type",value.name = "TXN_DT")
dt.recent <- dt.recent[,n:=1][!is.na(TXN_DT), .(CUST_NO, TXN_DT,n,type)]
dt <- rbindlist(list(dt.beh,dt.cc,dt.ln,dt.fx,dt.wm,dt.recent,dt.st))
setorder(dt, CUST_NO, TXN_DT, type)
# 問題：
# 1) 資料缺失分佈(如何處理?)
# 2) 行為先後關係
# 3) date-wise (weekdays, high-low)
# feature
# 

tmp <- dt[,target.cc:=ifelse(TXN_DT>=9448&type=="02cc", 1, 0)]
dt[,`:=`(Has.cc=0,first.is.cc=0)]
dt[CUST_NO %in% tmp[target.cc==1]$CUST_NO, Has.cc:=1]
dt[type!="st.dt", nth:=1:.N, by=CUST_NO]
dt[CUST_NO %in% dt[type=="02cc"&nth==1]$CUST_NO,first.is.cc:=1]
dt[type=="02cc", uniqueN(CUST_NO)]
dt[type=="02cc", .N, by=first.is.cc]
View(dt[Has.cc==1&first.is.cc==0])
tmp2 <- dt[Has.cc==1&nth==1]
tmp2[,.N, by=first.is.cc]

dt1 <- dcast.data.table(dt, CUST_NO+TXN_DT~type, value.var = "n")

# whether cc before (recency)
# whether other action before (recency)

# (cc, wm, ln, fx, br, st)
# y.cc feature:
# cust_info
# previous action
# n weeks x (4 )
# holidays
