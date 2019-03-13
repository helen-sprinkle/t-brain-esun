source("dirs.R")
source(file.path(aux.dir, "mylib.R"))
source(file.path(aux.dir, "slackme.R"))
mylib(c("data.table", "magrittr", "Matrix", "mltools", "xgboost"))
# - raw input: "fls","TBN_RECENT_DT","TBN_CIF","TBN_CUST_BEHAVIOR",
#                    "TBN_CC_APPLY","TBN_LN_APPLY","TBN_FX_TXN","TBN_WM_TXN"
load(file.path(data.dir, "data.RData"))

# sparse matrix
# https://www.rdocumentation.org/packages/mltools/versions/0.3.5/topics/sparsify
# https://www.kaggle.com/cartographic/data-table-to-sparsematrix
# https://github.com/dmlc/xgboost/blob/master/R-package/demo/create_sparse_matrix.R


fls <- list.files(data.dir, "y.*.RData")
for (in.fnm in fls) {
    # - label data: "y.cc", "y.fx", "y.ln", "y.wm", "x.date.list", "y.date.list"
    load(file.path(data.dir, in.fnm))
    for (y.tag in c("cc", "fx", "ln", "wm")) {
        # label data----
        y.date <- get(sprintf("y.%s", y.tag))
        y.date <- y.date[, .(CUST_NO, date, Y)]
        for (feature.tag in c("behavior", "custInfo", "recent")) {
            # feature data----
            feature.date.list <- x.date.list[, .(date, all.date)]
            
            if(feature.tag=="behavior") {
                ## path categories (melt to long table)
                dt.long <- TBN_CUST_BEHAVIOR[, .(CUST_NO, VISITDATE, PATH1, PATH2, PATH3, PATH4, n)] %>% 
                    melt.data.table(., id.vars = c("CUST_NO", "VISITDATE", "n"), 
                                    measure.vars = c("PATH1", "PATH2", "PATH3", "PATH4"), 
                                    variable.name = "path.level", value.name = "category")
                all.path <- unique(dt.long$category)
                dt.long.date <- dt.long[!is.na(category)&category!="", 
                                        .(CUST_NO, VISITDATE, category, n)][feature.date.list, on = c("VISITDATE"="all.date")][
                                            , .(n=sum(n)), by = .(CUST_NO, date, category)]
                ### spread to every path category
                dt.raw <- dcast(dt.long.date, CUST_NO+date~category, value.var = "n", fun.aggregate = sum)
                zero.path <- all.path[!all.path %in% colnames(dt.raw)]
                dt.raw[,eval(zero.path):=0]
                dt.raw <- y.date[dt.raw, on = c("CUST_NO"="CUST_NO", "date"="date")][is.na(Y), Y:=0]
                dt <- dt.raw[, -c("CUST_NO", "date")]
                x <- sparsify(dt[, -c("Y")], sparsifyNAs = T)
            } else if (feature.tag=="custInfo") {
                ## cust_info
                TBN_CIF[is.na(TBN_CIF)] <- -1
                TBN_CIF[GENDER_CODE=="", GENDER_CODE:="Missing"]
                dt.raw <- y.date[TBN_CIF, on = "CUST_NO"][is.na(Y), Y:=0]
                dt <- dt.raw[, -c("CUST_NO", "date")]
                x <- sparse.model.matrix(Y~.-1, data = dt)
            } else if (feature.tag=="recent") {
                # concate all action
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
                dt <- rbindlist(list(dt.cc,dt.ln,dt.fx,dt.wm,dt.recent,dt.st))
                setorder(dt, CUST_NO, TXN_DT, type)
                dt.raw <- dt[feature.date.list, on = c("TXN_DT"="all.date")][, .(n=sum(n)), by=.(CUST_NO, date, type)] %>% 
                    dcast.data.table(., CUST_NO+date~type,value.var = "n")
                dt.raw <- y.date[dt.raw, on = c("date","CUST_NO")][is.na(Y), Y:=0]
                dt <- dt.raw[, -c("CUST_NO", "date")]
                x <- sparsify(dt[, -c("Y")], sparsifyNAs = T)
            }
            out.fnm <- file.path(data.dir, "processed", sprintf("dt_x_%s_%s_%s",feature.tag, y.tag, in.fnm))
            out.fnm1 <- file.path(data.dir, "intermediates", sprintf("dt.raw_x_%s_%s_%s",feature.tag, y.tag, in.fnm))
            # output file
            y <- dt$Y
            obj <- list(x=x, y=y)
            save(obj, file=out.fnm)
            save(dt.raw, file=out.fnm1)
            cat(sprintf("feature:%s_y:%s_intvl:%s_done\n",feature.tag, y.tag, in.fnm))
        }
    }
}
slackme("training data done", st.tm)
dt.raw[1:3, 1:3]
dt.raw[, .N, by=Y]





in.fnm <- "y.train30.test30.roll30.RData"
feature.tag <- "bencode"
load(file.path(data.dir, in.fnm))
load(file.path(data.dir, "bencoder.RData"))

for (y.tag in c("cc", "fx", "ln", "wm")) {
    # label data----
    y.date <- get(sprintf("y.%s", y.tag))
    for (target.date in c(9478, 9508, 9538, 9568)) {
        dt <- bencoder[date==target.date]
        y <- y.date[date==target.date, .(CUST_NO, Y)]
        dt.raw <- y[dt, on = 'CUST_NO']
        dt.raw[, .N, by=Y]
        dt.raw[is.na(Y), Y:=0]
        dt <- dt.raw[, -c("CUST_NO", "date")]
        y <- dt$Y
        x <- sparsify(dt[, -c("Y")], sparsifyNAs = T)
        out.fnm <- file.path(data.dir, "processed", sprintf("dt_%s_x_%s_%s_%s",target.date,feature.tag, y.tag, in.fnm))
        out.fnm1 <- file.path(data.dir, "intermediates", sprintf("dt.raw_%s_x_%s_%s_%s",target.date,feature.tag, y.tag, in.fnm))
        # output file
        y <- dt$Y
        obj <- list(x=x, y=y)
        save(obj, file=out.fnm)
        save(dt.raw, file=out.fnm1)
        cat(sprintf("feature:%s_y:%s_intvl:%s_done\n",feature.tag, y.tag, in.fnm))
    }
}

in.fnm <- "y.train30.test30.roll30.RData"
load(file.path(data.dir, in.fnm))
feature.tag <- "recent.wayback"
for (target.date in c(9478, 9508, 9538, 9568)) {
    load(file.path(data.dir, sprintf("recent.%s.wayback.RData", target.date)))
    dt.recent <- dt
    for (y.tag in c("cc", "fx", "ln", "wm")) {
        # label data----
        y.date <- get(sprintf("y.%s", y.tag))
        y <- y.date[date==target.date, .(CUST_NO, Y)]
        dt.raw <- y[dt.recent, on = 'CUST_NO']
        # dt.raw[, .N, by=Y]
        dt.raw[is.na(Y), Y:=0]
        dt <- dt.raw[, -c("CUST_NO", "date")]
        y <- dt$Y
        x <- sparsify(dt[, -c("Y")], sparsifyNAs = T)
        out.fnm <- file.path(data.dir, "processed", sprintf("dt_%s_x_%s_%s_%s",target.date,feature.tag, y.tag, in.fnm))
        out.fnm1 <- file.path(data.dir, "intermediates", sprintf("dt.raw_%s_x_%s_%s_%s",target.date,feature.tag, y.tag, in.fnm))
        # output file
        y <- dt$Y
        obj <- list(x=x, y=y)
        stop()
        save(obj, file=out.fnm)
        save(dt.raw, file=out.fnm1)
        cat(sprintf("feature:%s_y:%s_intvl:%s_done\n",feature.tag, y.tag, in.fnm))
    }
}    
