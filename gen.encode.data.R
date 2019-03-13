source("dirs.R")
source(file.path(aux.dir, "mylib.R"))
mylib(c("data.table"))

bencoder <- fread(file.path(data.dir, "encode.csv"), skip = 1)
load(file.path(data.dir, "cust.list.RData"))
fls <- list.files(data.dir, "y.*.RData")
in.fnm <- fls[1]
load(file.path(data.dir, in.fnm))
date.list <- unique(y.date.list$date)
date.list <- c(date.list, date.list[3]+30)
setorder(cust.list, CUST_NO)
cust.list <- unlist(cust.list)
feature.tag <- "bencoder"
bencoder[ ,`:=` (CUST_NO=rep(cust.list, each=4),
                 date=rep(date.list, times=length(cust.list)))]

save(bencoder, file=file.path(data.dir, "bencoder.RData"))

# # check data
# load("/Users/helenchang/data/t-brain-esun/data/done/second_try_feature_class_missing/intermediates/dt.raw_x_behavior_cc_y.train30.test30.roll30.RData", verbose = T)
# # date.ls <- expand.grid(date=c(9478, 9508, 9538, 9568), all.date=st.int:ed.int) %>% data.table()
# cust.ls <- expand.grid(CUST_NO=cust.list, date=c(9478, 9508, 9538, 9568)) %>% data.table()
# dt.raw.new <- dt.raw[cust.ls, on=c("CUST_NO", "date")]
# setorder(dt.raw.new, CUST_NO, date)
# ii <- 100
# i <- 4*(ii-1)+1
# bencoder[seq(i, by=195000, length.out = 4), 121:130]
# bencoder[i:(i+3), 121:130]
# rowSums(dt.raw.new[CUST_NO==cust.list[ii], -c("CUST_NO", "date", "Y")])
