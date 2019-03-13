source("dirs.R")
source(file.path(aux.dir, "mylib.R"))
# source(file.path(aux.dir, "slackme.R"))
mylib(c("data.table", "magrittr", "Matrix", "mltools", "xgboost"))

in.fnm <- "y.train30.test30.roll30.RData"
feature.tag <- "ben_recent_wayback"
load(file.path(data.dir, in.fnm))
load(file.path(data.dir, "bencoder.RData"))

for (target.date in c(9478, 9508, 9538, 9568)) {
    
    load(file.path(data.dir, sprintf("recent.%s.wayback.RData", target.date)))
    dt.recent <- dt
    beh <- bencoder[date==target.date]
    cat(dim(dt.recent), "\n")
    for (y.tag in c("fx", "ln", "wm")) {
    # for (y.tag in c("cc")) {
        y.date <- get(sprintf("y.%s", y.tag))
        y.date <- y.date[date==target.date, .(CUST_NO, Y)]

        dt.raw <- merge(dt.recent, y.date, all = T, by="CUST_NO")
        dt.raw <- merge(dt.raw, beh, all=T, by=c("CUST_NO", "date"))
        # dt.raw[, .N, by=Y]
        dt.raw[is.na(Y), Y:=0]

        dt <- dt.raw[, -c("CUST_NO", "date")]
        y <- dt$Y
        x <- sparsify(dt[, -c("Y")], sparsifyNAs = T)
        out.fnm <- file.path(data.dir, "processed", sprintf("dt_%s_x_%s_%s_%s",target.date,feature.tag, y.tag, in.fnm))
        out.fnm1 <- file.path(data.dir, "intermediates", sprintf("dt.raw_%s_x_%s_%s_%s",target.date,feature.tag, y.tag, in.fnm))
        # output file
        obj <- list(x=x, y=y)
        save(obj, file=out.fnm)
        save(dt.raw, file=out.fnm1)
        cat(sprintf("feature:%s%s_y:%s_intvl:%s_done\n",feature.tag,target.date, y.tag, in.fnm))
    }
}


# 
# load(file.path(data.dir, "data.RData"))
# in.fnm <- "y.train30.test30.roll30.RData"
# feature.tag <- "custInfo_ben_recent_wayback"
# TBN_CIF[is.na(TBN_CIF)] <- -1
# TBN_CIF[GENDER_CODE=="", GENDER_CODE:="Missing"]
# tmp <- TBN_CIF
# # dt.raw <- y.date[TBN_CIF, on = "CUST_NO"][is.na(Y), Y:=0]
# # dt <- dt.raw[, -c("CUST_NO", "date")]
# x1 <- sparse.model.matrix(CUST_NO~.-1, data = tmp)
# row.names(x1) <- tmp$CUST_NO
# for (y.tag in c("cc", "fx", "ln", "wm")) {
#     # label data----
#     for (target.date in c(9478, 9508, 9538, 9568)) {
#         load(file.path(data.dir, sprintf("recent.%s.wayback.RData", target.date)))
#         dt.recent <- dt
#         
#         beh <- bencoder[date==target.date]
#         
#         y.date <- get(sprintf("y.%s", y.tag))
#         y.date <- y.date[date==target.date, .(CUST_NO, Y)]
#         
#         dt.raw <- merge(dt.recent, y.date, all = T, by="CUST_NO")
#         dt.raw <- merge(dt.raw, beh, all=T, by=c("CUST_NO", "date"))
#         # dt.raw[, .N, by=Y]
#         dt.raw[is.na(Y), Y:=0]
#         
#         dt <- dt.raw[, -c("CUST_NO", "date")]
#         y <- dt$Y
#         x <- sparsify(dt[, -c("Y")], sparsifyNAs = T)
#         out.fnm <- file.path(data.dir, "processed", sprintf("dt_%s_x_%s_%s_%s",target.date,feature.tag, y.tag, in.fnm))
#         out.fnm1 <- file.path(data.dir, "intermediates", sprintf("dt.raw_%s_x_%s_%s_%s",target.date,feature.tag, y.tag, in.fnm))
#         # output file
#         obj <- list(x=x, y=y)
#         save(obj, file=out.fnm)
#         save(dt.raw, file=out.fnm1)
#         cat(sprintf("feature:%s%s_y:%s_intvl:%s_done\n",feature.tag,target.date, y.tag, in.fnm))
#     }
# }
