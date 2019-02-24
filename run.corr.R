source("dirs.R")
source(file.path(aux.dir, "mylib.R"))
mylib(c("data.table", "magrittr", "corrplot"))
load(file.path(data.dir, "data.RData"))

TBN_CIF <- na.omit(TBN_CIF)
TBN_CIF[, GENDER_CODE:=as.integer(as.factor(GENDER_CODE))]
setnames(TBN_CIF, colnames(TBN_CIF)[-1], c("AGE", "CHILDREN", "CUST_ST_DT", "EDU", 
                                           "GENDER", "INCOME", "WORK_MTHS"))
TBN_CIF[,`:=` (CC_APPLY=ifelse(CUST_NO %in% TBN_CC_APPLY$CUST_NO, 1, 0),
               LN_APPLY=ifelse(CUST_NO %in% TBN_LN_APPLY$CUST_NO, 1, 0),
               FX_TXN=ifelse(CUST_NO %in% TBN_FX_TXN$CUST_NO, 1, 0),
               WM_TXN=ifelse(CUST_NO %in% TBN_WM_TXN$CUST_NO, 1, 0))]
M <- select(TBN_CIF, -CUST_NO) %>% cor(.)
pdf(file = "./summary_and_reports/corr.pdf")
corrplot.mixed(M, lower.col = "black", tl.cex=0.5)
dev.off()
