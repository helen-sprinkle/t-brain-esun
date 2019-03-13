source("dirs.R")
source(file.path(aux.dir, "mylib.R"))
mylib(c("data.table", "magrittr"))

load(file.path(data.dir, "data.RData"))
load(file.path(data.dir, "date.holiday.RData"))
target.date <- 9478

# (A) most recent date (before 120 days of training period)
cc.past<- TBN_RECENT_DT[!is.na(CC_RECENT_DT), .(CUST_NO, CC_RECENT_DT)]
cc.past[, N:=0]
setnames(cc.past, "CC_RECENT_DT", "TXN_DT")
# (B) transactions within 120 (training period)
dt.cc <- unique(TBN_CC_APPLY)
setorder(dt.cc, CUST_NO, TXN_DT)
dt.cc[, N:=.N, by = CUST_NO]
# concate (A) + (B)
cc.past.all <- rbindlist(l = list(cc.past, dt.cc), use.names = T)
cc.past.all[, n:=.N, by=.(CUST_NO)]
setorder(cc.past.all, CUST_NO, TXN_DT)
cc.past.all[, `:=` (last=shift(TXN_DT),
                    nth=1:.N), by = CUST_NO]
cc.past.all[, interval:=TXN_DT-last]
cc.past.all <- date.holiday[cc.past.all, on = "TXN_DT"]

# (A) most recent date (before 120 days of training period)
fx.past <- TBN_RECENT_DT[!is.na(FX_RECENT_DT), .(CUST_NO, FX_RECENT_DT)][
    , `:=` (FX_TXN_AMT=NA, N=0)]
setnames(fx.past, "FX_RECENT_DT", "TXN_DT")
# (B) transactions within 120 (training period)
dt.fx <- TBN_FX_TXN[, .(FX_TXN_AMT=log10(sum(FX_TXN_AMT, na.rm = T))), by=.(CUST_NO, TXN_DT)]
dt.fx[, N:=.N, by = CUST_NO]
# concate (A) + (B)
fx.past.all <- rbindlist(l = list(fx.past, dt.fx), use.names = T)
setorder(fx.past.all, CUST_NO, TXN_DT)
fx.past.all[, `:=` (last=shift(TXN_DT),
                    nth=1:.N), by = CUST_NO]
fx.past.all[, interval:=TXN_DT-last]
fx.past.all <- date.holiday[fx.past.all, on = "TXN_DT"]

# (A) most recent date (before 120 days of training period)
ln.past <- TBN_RECENT_DT[!is.na(LN_RECENT_DT), .(CUST_NO, LN_RECENT_DT)]
ln.past[, N:=0]
setnames(ln.past, "LN_RECENT_DT", "TXN_DT")
# (B) transactions within 120 (training period)
dt.ln <- TBN_LN_APPLY[, .(LN_AMT=log10(sum(LN_AMT, na.rm = T))), by=.(CUST_NO, TXN_DT, LN_USE)]
dt.ln[, LN_USE:=paste("LN", LN_USE, sep = "_")]
# add categorical class
ln.use <- dcast(dt.ln, CUST_NO+TXN_DT~LN_USE, fun.aggregate = sum, value.var = "LN_AMT", fill = 0)
setorder(ln.use, CUST_NO, TXN_DT)
# concate (A) + (B)
dt.ln1 <- dt.ln[, N:=.N, by = CUST_NO][, .(CUST_NO, TXN_DT, N)]
ln.past.all <- rbindlist(l = list(ln.past, dt.ln1), use.names = T)
setorder(ln.past.all, CUST_NO, TXN_DT)
ln.past.all[, `:=` (last=shift(TXN_DT),
                    nth=1:.N), by = CUST_NO][, ln.before.interval:=TXN_DT-last]
ln.past.all <- date.holiday[ln.past.all, on = "TXN_DT"]

# (A) most recent date (before 120 days of training period)
wm.past <- TBN_RECENT_DT[!is.na(WM_RECENT_DT), .(CUST_NO, WM_RECENT_DT)][, N:=0]
setnames(wm.past, "WM_RECENT_DT", "TXN_DT")
# (B) transactions within 120 (training period)
dt.wm <- TBN_WM_TXN[, .(WM_TXN_AMT=log10(sum(WM_TXN_AMT))), 
                    by=.(CUST_NO, TXN_DT, CUST_RISK_CODE, INVEST_TYPE_CODE)][
                        , `:=` (CUST_RISK_CODE=paste("RISK", CUST_RISK_CODE, sep = "_"),
                                INVEST_TYPE_CODE=paste("USE", INVEST_TYPE_CODE, sep = "_"))]
setorder(dt.wm, CUST_NO, TXN_DT)
# add categorical class
wm.use <- dcast(dt.wm, CUST_NO+TXN_DT~INVEST_TYPE_CODE+CUST_RISK_CODE, 
                fun.aggregate = sum, value.var = "WM_TXN_AMT", fill = 0,
                drop = F, na.rm=T)
setorder(wm.use, CUST_NO, TXN_DT)
dt.wm1 <- dt.wm[, N:=.N, by = CUST_NO][, .(CUST_NO, TXN_DT, N)]
# concate (A) + (B)
wm.past.all <- rbindlist(l = list(wm.past, dt.wm1), use.names = T)
setorder(wm.past.all, CUST_NO, TXN_DT)
wm.past.all[, `:=` (last=shift(TXN_DT),
                    nth=1:.N), by = CUST_NO][, wm.before.interval:=TXN_DT-last]
wm.past.all <- date.holiday[wm.past.all, on = "TXN_DT"]

for (target.date in c(9478, 9508, 9538, 9568)) {
    cat(target.date, "\n")
        
    cc.past.x.date <- cc.past.all[TXN_DT<target.date][
        , .SD[nth==.N], by=CUST_NO, 
        .SDcols=c("TXN_DT", "N", "interval", "is.holiday")][
            , .(HasCC.N=1, cc.last.interval=target.date-TXN_DT,
                cc.before.interval=interval, cc.is.holiday=is.holiday), by=.(CUST_NO)]
    
    
    
    fx.past.x.date <- fx.past.all[TXN_DT<target.date][
        , n:=.N, by=.(CUST_NO)][
            , fx.amt.ttl:=sum(FX_TXN_AMT, na.rm = T), by=CUST_NO][
                , .SD[nth==.N], by=CUST_NO, 
                .SDcols=c("TXN_DT", "N", "interval", "is.holiday", 
                          "fx.amt.ttl", "FX_TXN_AMT")][
                              , .(HasFX.N=1, fx.last.interval=target.date-TXN_DT,
                                  fx.before.interval=interval, fx.is.holiday=is.holiday,
                                  fx.amt.ttl=fx.amt.ttl, fx.amt.last=FX_TXN_AMT), by=.(CUST_NO)][
                                      is.na(fx.amt.last), fx.amt.ttl:=NA]

    
    
    ln.use.x.date <- ln.use[TXN_DT<target.date]
    n <- ncol(ln.use.x.date)
    cols <- colnames(ln.use.x.date)[3:n]
    ln.use.x.date <- ln.use.x.date[, lapply(.SD, sum, na.rm=T), by=CUST_NO, .SDcols=cols]
    
    ln.past.x.date <- ln.past.all[TXN_DT<target.date][
        , n:=.N, by=.(CUST_NO)][
            , .SD[nth==.N], by=CUST_NO, 
            .SDcols=c("TXN_DT", "ln.before.interval", "is.holiday")]
    ln.past.x.date <- ln.use.x.date[ln.past.x.date, on = "CUST_NO"][
        , `:=` (HasLN.N=1, ln.last.interval=target.date-TXN_DT)][
            , -c("TXN_DT")]
    setnames(ln.past.x.date, "is.holiday", "ln.is.holiday")
    
    
    
    wm.use.x.date <- wm.use[TXN_DT<target.date]
    n <- ncol(wm.use.x.date)
    cols <- colnames(wm.use.x.date)[3:n]
    wm.use.x.date <- wm.use.x.date[, lapply(.SD, sum, na.rm=T), by=CUST_NO, .SDcols=cols]
    
    wm.past.x.date <- wm.past.all[TXN_DT<target.date][
        , n:=.N, by=.(CUST_NO)][
            , .SD[nth==.N], by=CUST_NO, 
            .SDcols=c("TXN_DT", "wm.before.interval", "is.holiday")]
    wm.past.x.date <- wm.use.x.date[wm.past.x.date, on ="CUST_NO"][
        , `:=` (HasWM.N=1, wm.last.interval=target.date-TXN_DT)][
            , -c("TXN_DT")]
    setnames(wm.past.x.date, "is.holiday", "wm.is.holiday")
    
    # concate four kinds of transactions
    dt <- cc.past.x.date
    dt <- merge(dt, fx.past.x.date, all=TRUE)
    dt <- merge(dt, ln.past.x.date, all=TRUE)
    dt <- merge(dt, wm.past.x.date, all=TRUE)
    dt[is.na(HasCC.N), `:=` (HasCC.N=0,
                             cc.last.interval=-1,
                             cc.before.interval=-1,
                             cc.is.holiday=-1)]
    dt[is.na(HasFX.N), `:=` (HasFX.N=0,
                             fx.last.interval=-1,
                             fx.before.interval=-1,
                             fx.is.holiday=-1,
                             fx.amt.ttl=0,
                             fx.amt.last=0)]
    dt[is.na(HasLN.N), `:=` (HasLN.N=0,
                             ln.last.interval=-1,
                             ln.before.interval=-1,
                             ln.is.holiday=-1)]
    ln.cols <- which(grepl("LN_", colnames(dt)))
    dt[HasLN.N==0, eval(ln.cols):=0]
    dt[is.na(HasWM.N), `:=` (HasWM.N=0,
                             wm.last.interval=-1,
                             wm.before.interval=-1,
                             wm.is.holiday=-1)]
    wm.cols <- which(grepl("RISK_", colnames(dt)))
    dt[HasWM.N==0, eval(wm.cols):=0]
    dt[,date:=target.date]
    save(dt, file=file.path(data.dir, "intermediates", sprintf("recent.%s.wayback.RData", target.date)))
}
slackme(msg = "recent.30.days done",st.tm)
