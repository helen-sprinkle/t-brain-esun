source("dirs.R")
source(file.path(aux.dir, "mylib.R"))
mylib(c("data.table", "magrittr"))

# custInfo
y.tag <- "zero"
y_zero <- fread(file.path(data.dir, "raw_input","TBN_Y_ZERO.csv"))

fls.raw <- list.files(file.path(data.dir, "validation"), pattern = "dt.raw")
fnm1 <- fls.raw[1]
# ex. dt.raw_9538_x_ben_recent_wayback_wm_y.train30.test30.roll30.RData
load(file.path(data.dir, "validation", fnm1), verbose = T)
idx <- which(dt.raw$CUST_NO %in% y_zero$CUST_NO)

fls.dt <- list.files(file.path(data.dir, "validation"), pattern = "dt_")
fnm2 <- fls.dt[1]
# ex. dt_9568_x_ben_recent_wayback_cc_y.train30.test30.roll30.RData
load(file.path(data.dir, "validation", fnm2), verbose = T)
n <- obj[["x"]][idx,]

fls.model <- list.files(file.path(data.dir, "validation/models"))
for (fnm in fls.model) {
    indicator <- strsplit(fnm, "_") %>% unlist() %>% "["(4) %>% toupper() %>% grep(., colnames(y_zero))
    load(file.path(data.dir, "validation/models", fnm), verbose = T)
    bstSparse <- ret_model[[4]][2]
    pred <- predict(bstSparse, n)
    pred1 <- ifelse(unlist(pred) > 0.5,1,0)
    tmp <- data.table(CUST_NO=dt.raw$CUST_NO[idx], pred1=pred1)
    cat("\n", indicator, "\n")
    y_zero[tmp, pred:=pred1, on = "CUST_NO"]
    y_zero[, eval(indicator):=pred]
}
y_zero[, pred:=NULL]
y_zero[, .N, by=.(CC_IND, FX_IND, LN_IND, WM_IND)]
y_zero.cust <- y_zero
setnames(y_zero.cust, colnames(y_zero.cust), paste("cust", colnames(y_zero.cust), sep = "_"))
setnames(y_zero.cust, "cust_CUST_NO", "CUST_NO")
# write.csv(y_zero.cust, "TBN_Y_ZERO.csv", row.names = F)


# behavior_encoder + recent_wayback
y.tag <- "zero"
y_zero <- fread(file.path(data.dir, "raw_input","TBN_Y_ZERO.csv"))

fls.raw <- list.files(file.path(data.dir, "validation_1"), pattern = "dt.raw")
fnm1 <- fls.raw[1]
# dt_9538_x_ben_recent_wayback_wm_y.train30.test30.roll30.RData
load(file.path(data.dir, "validation_1", fnm1), verbose = T)
idx <- which(dt.raw$CUST_NO %in% y_zero$CUST_NO)

fls.dt <- list.files(file.path(data.dir, "validation_1"), pattern = "dt_")
fnm2 <- fls.dt[1]
# dt_9568_x_ben_recent_wayback_cc_y.train30.test30.roll30.RData
load(file.path(data.dir, "validation_1", fnm2), verbose = T)
n <- obj[["x"]][idx,]

fls1 <- list.files(file.path(data.dir, "validation_1/models"), pattern = "9538_x_ben.recent.wayback")
for (fnm in fls1) {
    indicator <- strsplit(fnm1, "_") %>% unlist() %>% "["(7) %>% toupper() %>% grep(., colnames(y_zero))
    load(file.path(data.dir, "validation_1/models", fnm), verbose = T)
    bstSparse <- ret_model[[4]][2]
    pred <- predict(bstSparse, n)
    pred1 <- ifelse(unlist(pred) > 0.5,1,0)
    tmp <- data.table(CUST_NO=dt.raw$CUST_NO[idx], pred1=pred1)
    cat("\n", indicator, "\n")
    y_zero[tmp, pred:=pred1, on = "CUST_NO"]
    y_zero[, eval(indicator):=pred]
}
y_zero[, pred:=NULL]
y_zero[, .N, by=.(CC_IND, FX_IND, LN_IND, WM_IND)]


tmp1 <- y_zero.cust[y_zero, on="CUST_NO"]
tmp1[cust_CC_IND==1, CC_IND:=1]
tmp1[cust_FX_IND==1, FX_IND:=1]
tmp1[cust_WM_IND==1, WM_IND:=1]
tmp1[cust_LN_IND==1, LN_IND:=1]
tmp2 <- tmp1[, .(CUST_NO, CC_IND, FX_IND, LN_IND, WM_IND)]

write.csv(tmp2, "TBN_Y_ZERO_9568_ben_recent_wayback_custInfo.csv", row.names = F)


fls.models <- list.files(file.path(data.dir, "validation_1/models"), pattern = "9538_x_ben.recent.wayback")
for (fnm in fls.models) {
    load(file.path(data.dir, "validation_1/models", fnm), verbose = T)
    bstSparse <- ret_model[[4]][2]
    imp <- xgb.importance(model = bstSparse)
    stop()
}
