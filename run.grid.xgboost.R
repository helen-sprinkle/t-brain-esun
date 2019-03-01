source("dirs.R")
source(file.path(aux.dir, "mylib.R"))
source(file.path(aux.dir, "slackme.R"))
mylib(c("xgboost", "caret", "Matrix", "mltools", "ROCR"))

fls <- list.files(file.path(data.dir, "processed"))
for (in.fnm in fls) {
    load(file.path(data.dir, "processed", in.fnm))

    out.fnm <- gsub(".RData", "", basename(in.fnm))
    # ex. "dt_x_behavior_fx_y.train30.test30.roll30.RData"
    #      to (3,4,5) "behavior" "fx" "y.train30.test30.roll30"
    out.tag <- tstrsplit(out.fnm, "_", keep=3:5) %>% unlist
    # ex. "y.train30.test30.roll30"
    #      to "30" (train interval)
    out.tag[3] <- substr(out.tag[3], 8, 9)
    
    x <- obj[["x"]]
    y <- obj[["y"]]
    
    set.seed(107)
    inTrain <- createDataPartition(
        y = y,
        ## the outcome data are needed
        p = 0.75,
        ## The percentage of data in the
        ## training set
        list = T
    ) %>% unlist()
    
    training    <- x[ inTrain,]
    train.label <- y[ inTrain] 
    testing     <- x[-inTrain,]
    test.label  <- y[-inTrain]
    
    # set ranges
    ## update speed
    range_learning_rate = 1 #.1*c(1, 3, 10)
    # tree depth, increasing this value will make the
    # model more complex and more likely to overfit
    range_max_depth =  c(2, 8) # 4
    # If it is set to a positive value, it can help making
    # the update step more conservative. Usually this parameter
    # is not needed, but it might help in logistic regression when
    # class is extremely imbalanced.
    range_mds = c(0)
    # Subsample ratio of the training instances. Setting it to 0.5
    # means that XGBoost would randomly sample half of the training
    # data prior to growing trees, which could prevent overfitting
    range_sb_sample = c(0.5, 1)
    # Control the balance of positive and negative weights,
    # useful for unbalanced classes. A typical value to consider:
    # sum(negative instances) / sum(positive instances)
    range_spw = round(table(y)[1] / table(y)[2], 0)
    
    ret_grid <- expand.grid(learning_rate=range_learning_rate,
                            max_depth=range_max_depth,
                            mds=range_mds,
                            sb_sample=range_sb_sample,
                            spw=range_spw,
                            auc=0,
                            Fmeasure=0,
                            precision=0,
                            recall=0)
    tot_testing_numbers <- nrow(ret_grid)
    ret_model <- vector(mode = "list", length = tot_testing_numbers)
    
    
    # ret_model <- lapply(1:2, function(test.n){
    ret_model <- lapply(1:nrow(ret_grid), function(test.n){
        bstSparse <- xgboost(data = training, label = train.label, nthread = 4, 
                             nrounds = 2000, early_stopping_rounds = 50, 
                             print_every_n = 100L,
                             objective = "binary:logistic", eval_metric = 'aucpr',
                             max.depth = ret_grid[test.n,"max_depth"], 
                             eta = ret_grid[test.n,"learning_rate"],
                             max_delta_step = ret_grid[test.n,"mds"], 
                             subsample = ret_grid[test.n,"sb_sample"],
                             scale_pos_weight = ret_grid[test.n,"spw"]
        )
        pred <- predict(bstSparse, testing)
        
        fitted.results <- ifelse(pred > 0.5,1,0)
        misClasificError <- mean(fitted.results != test.label)
        print(paste('Accuracy',1-misClasificError))
        
        pr <- prediction(fitted.results, test.label)
        # ROC curve
        prf <- performance(pr, measure = "tpr", x.measure = "fpr")
        plot(prf)
        
        auc <- performance(pr, measure = "auc")
        auc <- auc@y.values[[1]]
        # auc
        
        # Recall-Precision curve             
        prf <- performance(pr, measure = "f")
        plot(prf)
        
        retrieved <- sum(fitted.results)
        precision <- sum(fitted.results & test.label) / retrieved
        recall <- sum(fitted.results & test.label) / sum(test.label)
        Fmeasure <- 2 * precision * recall / (precision + recall)
        # Fmeasure
        
        ret_grid[test.n, "auc"] <<- auc
        ret_grid[test.n, "Fmeasure"] <<- Fmeasure
        ret_grid[test.n, "precision"] <<- precision
        ret_grid[test.n, "recall"] <<- recall
        para <- ret_grid[test.n, c(1:2, 4:5, 7)] %>% unlist()
        # round:1
        cat(sprintf("round:%s\n", test.n))
        # behavior_cc_tr90_F0.56_lr0.3_dep2_unb0.5_nb65
        cat(sprintf("%s_%s_tr%s_F%s_lr%s_dep%s_unb%s_nb%s\n", 
                    # "behavior" "fx" "30 (train interval)"
                    out.tag[1], out.tag[2], out.tag[3], 
                    # Fmeasure, learning rate, depth, subset, unbalanced
                    round(para[5],2), para[1], para[2],para[3],para[4]))
        write.table(ret_grid[test.n,], file.path(data.dir, "output",sprintf("%s_ret_grid.csv", out.fnm)),
                  row.names = F, col.names = F, append = T, sep = ",")
        return(list(pred, bstSparse))
    })
    
    slackme(sprintf("%s_finished",out.fnm), st.tm)
    save(ret_model, file=file.path(data.dir, "models", sprintf("%s.RData", out.fnm)))
    # write.csv(ret_grid, file.path(data.dir, "output",sprintf("%s_ret_grid.csv", out.fnm)), row.names = F)
    # stop()
}
