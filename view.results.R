source("dirs.R")
source(file.path(aux.dir, "mylib.R"))
source(file.path(aux.dir, "slackme.R"))
mylib(c("data.table"))


fls <- list.files(file.path(data.dir, "output"))

dt.ls <- lapply(fls, function(fnm) {
    dt <- fread(file.path(data.dir, "output",fnm), header=F)
    dt[, fnm:=fnm]
})
dt <- rbindlist(dt.ls)
setnames(dt, colnames(dt), c("round","learning_rate","max_depth","mds","sb_sample",
                             "spw","auc","Fmeasure","precision","recall","fnm"))
dt[, fnm:=gsub("ben_recent_wayback", "ben.recent.wayback", dt$fnm)]
dt[, c("x", "y", "n") := tstrsplit(fnm,"_",keep=c(4,5,2))]
write.table(dt, file="summary_and_reports/results.csv",row.names = F, sep = ",",
            col.names = F, append = T)
