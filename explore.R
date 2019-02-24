source("dirs.R")
source("../auxiliary/mylib.R")
mylib(c("data.table", "magrittr", "summarytools", "ggplot2"))
load(file.path(data.dir, "data.RData"))

# TBN_CIF (customer info)----
fnm <- fls[2]
dfs <- dfSummary(TBN_CIF)
# view(dfs)
output.fnm <- sprintf("%s.html", gsub(".csv", "", basename(fnm)))
print(dfs, file = output.fnm)
file.rename(output.fnm, file.path("summary_and_reports", output.fnm))





# TBN_CUST_BEHAVIOR (customer behavior)----
fnm <- fls[3]
ttl <- gsub(".csv", "", basename(fnm))
TBN_CUST_BEHAVIOR[, N:=.N, by = CUST_NO]
setorder(TBN_CUST_BEHAVIOR, CUST_NO, VISITDATE)
dt4 <- TBN_CUST_BEHAVIOR[, .(n.cust=uniqueN(CUST_NO)), by=VISITDATE]
p <- ggplot(dt4, aes(x = VISITDATE, y = n.cust)) + 
    geom_line() + 
    geom_point() +
    labs(title = ttl)
ggsave(sprintf("./summary_and_reports/%s.png", ttl), width = 10, height = 5)
# explore page path from TBN_CUST_BEHAVIOR (customer behavior)----
View(TBN_CUST_BEHAVIOR)
dt1 <- TBN_CUST_BEHAVIOR[,.(PATH0,PATH1,PATH2,PATH3,PATH4)]
dfs <- dfSummary(dt1)
view(dfs)
fnm <- fls[3]
output.fnm <- sprintf("%s_path.html", gsub(".csv", "", basename(fnm)))
print(dfs, file = )
file.rename(output.fnm, file.path("summary_and_reports", output.fnm))
# dt1[PATH1=="edrn", .N, by=PATH2]
# dt1[PATH1=="gygrt", .N, by=PATH2]
# dt1[PATH1=="s", .N, by=PATH2]
# dt2 <- TBN_CUST_BEHAVIOR[, uniqueN(CUST_NO), by=.(PATH1, PATH2)]
# setorder(dt2, PATH1, PATH2)
# View(dt2)





# TBN_CC_APPLY (credit card application)
fnm <- fls[1]
ttl <- gsub(".csv", "", basename(fnm))
TBN_CC_APPLY[, N:=.N, by = CUST_NO]
setorder(TBN_CC_APPLY, CUST_NO, TXN_DT)
TBN_CC_APPLY
# dt1 <- TBN_CC_APPLY[N==1]
# dt2 <- TBN_CC_APPLY[N>1]
# dt2[, `:=` (last=shift(TXN_DT),
#             nth=1:.N), by = CUST_NO]
# dt2[, interval:=TXN_DT-last]
# dt3 <- dt2[, .N, by = interval] 
# setorder(dt3,interval)
# dt3
# hist(dt2$interval[dt2$interval!=0&!is.na(dt2$interval)], br=120, right = F)
dt4 <- TBN_CC_APPLY[, .(n.cust=uniqueN(CUST_NO)), by=TXN_DT]
p <- ggplot(dt4, aes(x = TXN_DT, y = n.cust)) + 
    geom_line() + 
    geom_point() +
    labs(title = ttl)
ggsave(sprintf("./summary_and_reports/%s.png", ttl), width = 10, height = 5)





# TBN_FX_TXN (foreign exchange transaction)----
fnm <- fls[4]
ttl <- gsub(".csv", "", basename(fnm))
TBN_FX_TXN[, N:=.N, by = CUST_NO]
setorder(TBN_FX_TXN, CUST_NO, TXN_DT)
TBN_FX_TXN
# dt1 <- TBN_FX_TXN[N==1]
# dt2 <- TBN_FX_TXN[N>1]
# dt2[, `:=` (last=shift(TXN_DT),
#             nth=1:.N), by = CUST_NO]
# dt2[, interval:=TXN_DT-last]
# dt3 <- dt2[, .N, by = interval] 
# setorder(dt3,interval)
# dt3
# hist(dt2$interval[dt2$interval!=0&!is.na(dt2$interval)], br=120, right = F)
dt4 <- TBN_FX_TXN[, .(n.cust=uniqueN(CUST_NO)), by=TXN_DT]
p <- ggplot(dt4, aes(x = TXN_DT, y = n.cust)) + 
    geom_line() + 
    geom_point() +
    labs(title = ttl)
ggsave(sprintf("./summary_and_reports/%s.png", ttl), width = 10, height = 5)





# TBN_LN_APPLY (loan application)----
fnm <- fls[5]
ttl <- gsub(".csv", "", basename(fnm))
TBN_LN_APPLY[, N:=.N, by = CUST_NO]
setorder(TBN_LN_APPLY, CUST_NO, TXN_DT)
TBN_LN_APPLY
# dt1 <- TBN_LN_APPLY[N==1]
# dt2 <- TBN_LN_APPLY[N>1]
# dt2[, `:=` (last=shift(TXN_DT),
#             nth=1:.N), by = CUST_NO]
# dt2[, interval:=TXN_DT-last]
# dt3 <- dt2[, .N, by = interval] 
# setorder(dt3,interval)
# dt3
# hist(dt2$interval[dt2$interval!=0&!is.na(dt2$interval)], br=120, right = F)
dt4 <- TBN_LN_APPLY[, .(n.cust=uniqueN(CUST_NO)), by=TXN_DT]
p <- ggplot(dt4, aes(x = TXN_DT, y = n.cust)) + 
    geom_line() + 
    geom_point() +
    labs(title = ttl)
ggsave(sprintf("./summary_and_reports/%s.png", ttl), width = 10, height = 5)





# TBN_WM_TXN (wealth mangagement)----
fnm <- fls[7]
ttl <- gsub(".csv", "", basename(fnm))
TBN_WM_TXN[, N:=.N, by = CUST_NO]
setorder(TBN_WM_TXN, CUST_NO, TXN_DT)
TBN_WM_TXN
# dt1 <- TBN_WM_TXN[N==1]
# dt2 <- TBN_WM_TXN[N>1]
# dt2[, `:=` (last=shift(TXN_DT),
#             nth=1:.N), by = CUST_NO]
# dt2[, interval:=TXN_DT-last]
# dt3 <- dt2[, .N, by = interval] 
# setorder(dt3,interval)
# dt3
# hist(dt2$interval[dt2$interval!=0&!is.na(dt2$interval)], br=120, right = F)
dt4 <- TBN_WM_TXN[, .(n.cust=uniqueN(CUST_NO)), by=TXN_DT]
p <- ggplot(dt4, aes(x = TXN_DT, y = n.cust)) + 
    geom_line() + 
    geom_point() +
    labs(title = ttl)
ggsave(sprintf("./summary_and_reports/%s.png", ttl), width = 10, height = 5)
