source("dirs.R")
source(file.path(aux.dir, "mylib.R"))
mylib(c("data.table", "magrittr", "xgboost"))
# - raw input: "fls","TBN_RECENT_DT","TBN_CIF","TBN_CUST_BEHAVIOR",
#                    "TBN_CC_APPLY","TBN_LN_APPLY","TBN_FX_TXN","TBN_WM_TXN"
load(file.path(data.dir, "data.RData"))

# prepare data
# ## corr.path
# feature.tag <- "corr.path"
# dtn <- TBN_CUST_BEHAVIOR[, .(nn=1), by=.(CUST_NO,PAGE.new)]
# dtnn <- dcast.data.table(dtn, CUST_NO~PAGE.new, value.var = "nn", fill = 0)
## corr.path.category
feature.tag <- "corr.path.category"
dt.long <- TBN_CUST_BEHAVIOR[, .(CUST_NO, VISITDATE, PATH1, PATH2, PATH3, PATH4, n)] %>% 
    melt.data.table(., id.vars = c("CUST_NO", "VISITDATE", "n"), 
                    measure.vars = c("PATH1", "PATH2", "PATH3", "PATH4"), 
                    variable.name = "path.level", value.name = "category")
dt.long[, n.cust:=uniqueN(CUST_NO), by=.(category)]
dt.long <- dt.long[n.cust>=30]
### spread to every path category
dtnn <- dcast(dt.long[, -c("path.level")], CUST_NO+VISITDATE~category, value.var = "n", fun.aggregate = sum)


X <- dtnn[, -c("CUST_NO")] %>% as.matrix()
# find important path or category
# calculate correlation between path and y
# TBN_CC_APPLY
Y.cc <- ifelse(dtnn$CUST_NO %in% TBN_CC_APPLY$CUST_NO, 1, 0) %>% as.integer()
corr.cc <- sapply(1:ncol(X), function(n, y) cor(X[,n], y), Y.cc)
# TBN_LN_APPLY
Y.ln <- ifelse(dtnn$CUST_NO %in% TBN_LN_APPLY$CUST_NO, 1, 0) %>% as.integer()
corr.ln <- sapply(1:ncol(X), function(n, y) cor(X[,n], y), Y.ln)
# TBN_FX_TXN
Y.fx <- ifelse(dtnn$CUST_NO %in% TBN_FX_TXN$CUST_NO, 1, 0) %>% as.integer()
corr.fx <- sapply(1:ncol(X), function(n, y) cor(X[,n], y), Y.fx)
# TBN_WM_TXN
Y.wm <- ifelse(dtnn$CUST_NO %in% TBN_WM_TXN$CUST_NO, 1, 0) %>% as.integer()
corr.wm <- sapply(1:ncol(X), function(n, y) cor(X[,n], y), Y.wm)

df <- data.frame(path=colnames(X), 
                 corr.cc=corr.cc, 
                 corr.ln=corr.ln,
                 corr.fx=corr.fx,
                 corr.wm=corr.wm)
out.fnm <- sprintf("%s.csv", feature.tag)
write.csv(df, file.path("summary_and_reports", out.fnm), row.names = F)

print(feature.tag)
colnames(X)[corr.cc>0.1]
colnames(X)[corr.ln>0.1]
colnames(X)[corr.fx>0.1]
colnames(X)[corr.wm>0.1]
