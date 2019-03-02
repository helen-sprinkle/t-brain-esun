source("dirs.R")
source(file.path(aux.dir, "mylib.R"))
mylib(c("data.table", "magrittr"))

# generate RData
# - raw input: data.RData
# - label data: y.train30.test30.roll30.RData / y.train60.test30.roll30.RData



## generate "t-brain/data/data.RData"----
output.fnm <- file.path(data.dir, "data.RData")
# load(output.fnm)
### read in dataset
fls <- list.files(file.path(data.dir, "raw_input"), full.names = T)
### scanning every files to check if any error
# for (i in 1:length(fls)){
#     cat(sprintf("%s\n", fls[i]))
#     dt <- fread(fls[i])
#     print(dt)
# }

# TBN_RECENT_DT (most recent transaction date)----
fnm <- fls[6]
TBN_RECENT_DT <- fread(fnm)
# TBN_CIF (customer information)----
fnm <- fls[2]
TBN_CIF <- fread(fnm)
# TBN_CIF <- na.omit(TBN_CIF)
# TBN_CUST_BEHAVIOR (customer behavior)----
fnm <- fls[3]
TBN_CUST_BEHAVIOR <- fread(fnm)
TBN_CUST_BEHAVIOR[, PAGE:=gsub("//www.esunbank.com.tw", "", PAGE)]
TBN_CUST_BEHAVIOR[, PAGE:=gsub(":", "", PAGE)]
TBN_CUST_BEHAVIOR <- TBN_CUST_BEHAVIOR[, .(n=.N), by = .(CUST_NO,VISITDATE,PAGE)]
x <- lapply(TBN_CUST_BEHAVIOR$PAGE, function(x) {
    unlist(strsplit(x, "/"))
})
tmp0 <- lapply(x, "[", 1) %>% unlist()
tmp1 <- lapply(x, "[", 2) %>% unlist()
tmp2 <- lapply(x, "[", 3) %>% unlist()
tmp3 <- lapply(x, "[", 4) %>% unlist()
tmp4 <- lapply(x, "[", 5) %>% unlist()
TBN_CUST_BEHAVIOR[, `:=` (PATH0=tmp0,
                          PATH1=tmp1,
                          PATH2=tmp2,
                          PATH3=tmp3,
                          PATH4=tmp4)]
TBN_CUST_BEHAVIOR[, PAGE.new:=gsub("https/|http.", "", PAGE)]
setorder(TBN_CUST_BEHAVIOR, CUST_NO, VISITDATE, PAGE.new)
# TBN_CC_APPLY (credit card application)----
fnm <- fls[1]
TBN_CC_APPLY <- fread(fnm)
# TBN_LN_APPLY (loan application)----
fnm <- fls[5]
TBN_LN_APPLY <- fread(fnm)
# TBN_FX_TXN (foreign exchange transcation)----
fnm <- fls[4]
TBN_FX_TXN <- fread(fnm)
# TBN_WM_TXN (wealth management transaction)----
fnm <- fls[7]
TBN_WM_TXN <- fread(fnm)

save(list = c("fls","TBN_RECENT_DT","TBN_CIF","TBN_CUST_BEHAVIOR","TBN_CC_APPLY",
              "TBN_LN_APPLY","TBN_FX_TXN","TBN_WM_TXN"),
     file = output.fnm)




load(file.path(data.dir, "data.RData"))
## generate "y.train30.test30.roll30.RData"----
output.fnm <- "y.train%s.test%s.roll%s.RData"
# load(file.path(data.dir, "y.train30.test30.roll30.RData"))
## prepare four label data [y.cc/y.fx/y.ln/y.wm/x.date.list/y.date.list]----
# x.st---train.interval---x.ed
#                         y.st---test.interval---y.ed
st.int <- 9448
# train.interval <- 30 # c(30, 60, 90)
test.interval <- 30
rolling.interval <- 30
ed.int <- st.int + 120
for (train.interval in c(30, 60, 90)) {
    ed.x.int <- ed.int - test.interval - train.interval
    
    x.st.ls <- seq(st.int, ed.x.int, by = rolling.interval)
    mid.ls <- x.st.ls + train.interval
    y.ed.ls <- mid.ls + test.interval
    
    dt <- expand.grid(st.date=x.st.ls, date=st.int:ed.int) %>% data.table()
    dt[, ed.date:=st.date+train.interval]
    x.date.list <- dt[date>=st.date&date<ed.date]
    setnames(x.date.list, c("st.date","date", "ed.date"), c("st.date", "all.date", "date"))
    dt <- expand.grid(st.date=mid.ls, date=st.int:ed.int) %>% data.table()
    dt[, ed.date:=st.date+test.interval]
    y.date.list <- dt[date>=st.date&date<ed.date]
    setnames(y.date.list, c("st.date","date", "ed.date"), c("date", "all.date", "ed.date"))
    
    # y.cc
    TBN_CC_APPLY <- TBN_CC_APPLY[,.N, by=.(CUST_NO, TXN_DT)]
    dt2 <- TBN_CC_APPLY[y.date.list, on = c("TXN_DT"="all.date")]
    dt3 <- dt2[, .(n=sum(N)), by = .(CUST_NO, date)]
    dt3[,Y:=1]
    dt4 <- dt3[date!=st.int]
    dt4 <- na.omit(dt4)
    y.cc <- dt4
    # y.fx
    TBN_FX_TXN <- TBN_FX_TXN[,.N, by=.(CUST_NO, TXN_DT)]
    dt2 <- TBN_FX_TXN[y.date.list, on = c("TXN_DT"="all.date")]
    dt3 <- dt2[, .(n=sum(N)), by = .(CUST_NO, date)]
    dt3[,Y:=1]
    dt4 <- dt3[date!=st.int]
    dt4 <- na.omit(dt4)
    y.fx <- dt4
    # y.ln
    TBN_LN_APPLY <- TBN_LN_APPLY[,.N, by=.(CUST_NO, TXN_DT)]
    dt2 <- TBN_LN_APPLY[y.date.list, on = c("TXN_DT"="all.date")]
    dt3 <- dt2[, .(n=sum(N)), by = .(CUST_NO, date)]
    dt3[,Y:=1]
    dt4 <- dt3[date!=st.int]
    dt4 <- na.omit(dt4)
    y.ln <- dt4
    # y.wm
    TBN_WM_TXN <- TBN_WM_TXN[,.N, by=.(CUST_NO, TXN_DT)]
    dt2 <- TBN_WM_TXN[y.date.list, on = c("TXN_DT"="all.date")]
    dt3 <- dt2[, .(n=sum(N)), by = .(CUST_NO, date)]
    dt3[,Y:=1]
    dt4 <- dt3[date!=st.int]
    dt4 <- na.omit(dt4)
    y.wm <- dt4
    #!!! if (train.interval==60) {
    if (!((train.interval==30&rolling.interval==30)|(train.interval==90))) {
        y.cc.raw <- y.cc
        y.fx.raw <- y.fx
        y.ln.raw <- y.ln
        y.wm.raw <- y.wm
        x.date.list.raw <- x.date.list
        y.date.list.raw <- y.date.list
        for (i.date in unique(x.date.list$date)) {
            y.cc <- y.cc.raw[date==i.date]
            y.fx <- y.fx.raw[date==i.date]
            y.ln <- y.ln.raw[date==i.date]
            y.wm <- y.wm.raw[date==i.date]
            x.date.list <- x.date.list.raw[date==i.date]
            y.date.list <- y.date.list.raw[date==i.date]
            output.fnm1 <- "y.train%s.test%s.roll%s-%s.RData"
            save(list = c("y.cc", "y.fx", "y.ln", "y.wm", "x.date.list","y.date.list"), 
                 file=file.path(data.dir, sprintf(output.fnm1, train.interval, test.interval, rolling.interval, i.date)))
        }
    } else {
        save(list = c("y.cc", "y.fx", "y.ln", "y.wm", "x.date.list","y.date.list"), 
             file=file.path(data.dir, sprintf(output.fnm, train.interval, test.interval, rolling.interval)))
    }
}
