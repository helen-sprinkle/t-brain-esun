source("dirs.R")
source(file.path(aux.dir, "mylib.R"))
source(file.path(aux.dir, "slackme.R"))
mylib(c("data.table", "magrittr", "Matrix", "mltools", "xgboost"))

load(file.path(data.dir, "data.RData"))
dt.long <- TBN_CUST_BEHAVIOR[, .(CUST_NO, VISITDATE, PATH1, PATH2, PATH3, PATH4, n)] %>% 
    melt.data.table(., id.vars = c("CUST_NO", "VISITDATE", "n"), 
                    measure.vars = c("PATH1", "PATH2", "PATH3", "PATH4"), 
                    variable.name = "path.level", value.name = "category")
setnames(dt.long, "VISITDATE", "date")
dt.long.date <- dt.long[!is.na(category)&category!="", 
                        .(CUST_NO, date, category, n)][
                            , .(n=sum(n)), by = .(CUST_NO, date, category)]
dt.raw <- dcast(dt.long.date, CUST_NO+date~category, value.var = "n", fun.aggregate = sum)
dim(dt.raw)
cust.list <- data.table(CUST_NO=unique(dt.raw$CUST_NO))
date.list <- unique(dt.raw$date)

save(cust.list, file=file.path(data.dir, "cust.list.RData"))

l <- lapply(date.list, function (target.date) {
    dt <- dt.raw[date==target.date, -c("date")][cust.list, on = "CUST_NO"]
    dt[is.na(dt)] <- 0
    setorder(dt, CUST_NO)
    x <- sparsify(dt, sparsifyNAs = T)
    writeMM(x, file=file.path(data.dir, "behavior", sprintf("behavior_%s.csv", target.date)))
})

setorder(dt.raw, CUST_NO, date)
setnames(dt.raw, "date", "all.date")
dt.raw.new <- x.date.list[dt.raw, on=c("all.date")]
