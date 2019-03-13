source("dirs.R")
source(file.path(aux.dir, "mylib.R"))
mylib(c("data.table", "magrittr", "xgboost", "summarytools"))
# - raw input: "fls","TBN_RECENT_DT","TBN_CIF","TBN_CUST_BEHAVIOR",
#                    "TBN_CC_APPLY","TBN_LN_APPLY","TBN_FX_TXN","TBN_WM_TXN"
load(file.path(data.dir, "data.RData"))



# first try:----
# 1. the less depth (1) the better (fscore)
# 2. lower error results in higher accuracy but lower auc and fscore (best=0.48)
# 
# next possible testing:
# 1. add features (v cust_info, cc.rfm, behavior.rfm, and other transcations, v most recent xxx)
# v2. trim paths (importance not high)
# v3. different period (60+30 instead of 30+30 or 7+30)
# 4. normalization or take the logarithm 
# v5. try other error function (evaluation)
# v6. make path 0/1 feature instead of n (fscore=0.52 improved)
# 7. consider time series (weekly)
# 8. cross validation k-fold
# 9. testing cut by customer (row by row)
# 10. handle missing values
# 11. PCA (encoder)
# 12. voting (ensamble)
# 13. ensamble features
# 14. x calculate way back
# v15. check coverage
# 16. add holiday feature


# 問題：
# 1) 資料缺失分佈(如何處理?)
# 2) 行為先後關係
# 3) date-wise (weekdays, high-low)


