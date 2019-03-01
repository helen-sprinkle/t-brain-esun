fls <- list.files(file.path(data.dir, "output"))

dt.ls <- lapply(fls, function(fnm) {
    dt <- fread(file.path(data.dir, "output",fnm), header=F)
    dt[, fnm:=fnm]
})
dt <- rbindlist(dt.ls)
setnames(dt, colnames(dt), c("learning_rate","max_depth","mds","sb_sample",
                             "spw","auc","Fmeasure","precision","recall","fnm"))
dt[, c("x", "y", "n") := tstrsplit(fnm,"_",keep=c(3,4,5))]
write.table(dt, file="summary_and_reports/results.csv",row.names = F, append = T, sep = ",")
