
### Load Retention
retention <-read.csv("ret2020.csv",stringsAsFactors = F)
str(retention)
summary(retention)
head(retention)
dim(retention)
names(retention)
tail(retention)
str(retention)
str(retention)
##########################################
#
#           # Convert data types
#
#########################################

num_cols <- c(3,6,8:20,24:26) 
char_cols <- c(1,2,4,5,7,21:23)
char_retention <- retention[,char_cols]
num_retention <-retention[,num_cols]
num_retention <- lapply(num_retention, function(x) as.numeric(gsub("[$]", "", x)))
num_retention <- as.data.frame( num_retention)
num_retention <- cbind(num_retention,char_retention)
dim(num_retention)
str(num_retention)
summary(num_retention)
str(num_retention)

##########################################
#
#           # Imput External Status 
#
#########################################

  unique(num_retention$External.Status)
  summary(num_retention$External.Status)
  
# -- Convert the blank value to 'O' for open

  library(dplyr)
  retention_op <- num_retention %>% 
      mutate(External.Status = replace(External.Status, External.Status == " ", "O"))
  
# -- Verify  
  unique(retention_op$External.Status)
  #View(retention$External.Status)
  
# --------------//----------
  
  dim(retention_op)
  
  ##########################################
  #
  #           # Requirements
  #
  #########################################
  
  
  # ------------------------------
  #remove RowNum=1 with 
    # with delquency
    # not open external status
    # who maxed out
  #--------------------
  retention_op<- na.omit(retention_op )
  
      id_vec1 <-  select(retention_op,DebtDimId) %>% 
        filter(((retention_op$Row.Num==1) & (retention_op$Days.Deliq >0)))
    dim(id_vec1)
 select1 <- subset(retention_op,!(DebtDimId %in% id_vec1$DebtDimId))   
        dim(select1)
        
    id_vec2 <-  select(select1,DebtDimId) %>% 
        filter(((select1$Row.Num==1) & (select1$External.Status !="O")))
select2 <- subset(select1,!(DebtDimId %in% id_vec2$DebtDimId))   
    dim(select2)
    
    
    id_vec3 <- select(select2,DebtDimId) %>% 
      filter(((select2$Row.Num==1) & (select2$Credit.Limit < select2$Ending.Balance)))
select3 <- subset(select2,!(DebtDimId %in% id_vec3$DebtDimId))   
    dim(select3)  
    
    id_vec4 <- select(select3,DebtDimId) %>% 
      filter(((select3$Row.Num==1) & (select3$Credit.Limit < select3$Opening.Balance)))
select4 <- subset(select3,!(DebtDimId %in% id_vec4$DebtDimId))   
    dim(select4)    
    
  # -----------------//
    
    
    
    # ----- Create the 'Bad' Variable
     
    # This function is to determine if customer is bad or good based on 
    # External Status, Months on book, and Deliquency.
    #
    # ------- Beging Fun
    isBad <- function(external, delq,mnth)
    {
      if ((mnth>=7) & (external=="O" | external=="C") & (delq <=90))
        {
          return(0)
        }
      else  
        {
          return(1)
        }
    }
    # ------- End Fun
  
    
    
    # Apply the funtion
    
    ext <-select4$External.Status
    delq <- select4$Days.Deliq
    mnth <- select4$Months.On.Book
    Bad <-NULL
    n <-length(ext)
    for (i in 1:n){
      Bad[i] <- isBad(ext[i],delq[i],mnth[i])
    }
      retention_bad <- cbind(select4,Bad)
      table(retention_bad$Bad)
      dim(retention_bad)
    
      # ------------//
      
    
    # -----------------
     # Reduce the dataset into 1 row per Customer (take the last first row)
     retention_1Row <- retention_bad %>% 
       group_by(DebtDimId) %>% 
       filter(Row.Num==1)
      table(retention_1Row$Bad)
    
     dim(retention_1Row)
     
     
     
     
      # ---------------
     #   Select Predictor variables.
     
     # Scatter plots to see if whcich variables are related
    
     #######pairs(retention_1Row[,3:12])
     
     
     # Directly selected Variables
      input1 <- retention_1Row[,c("DebtDimId","Credit.Limit","Bad")] 
      
    # Calculated variables"
      utilize <-  retention_1Row$Ending.Balance /retention_1Row$Credit.Limit
      over_limt <-retention_1Row$Ending.Balance #  ifelse(retention_1Row$Over.limit.Amount >= 0 , 1,0)
      balance_grp <- cut(retention_1Row$Ending.Balance,breaks = c(0,135,215,845),labels = c("Low","Medium","High"))
      payed_min <- ifelse(retention_1Row$Actual.Min.Pay.Due<=retention_1Row$Net.Payments.During.Cycle,0,1)
      
    # merge the direct and the calculated.
      # it turned out over limit to be constant. exclude from model
     data_model <- cbind(input1,Utilize=utilize,Balance=balance_grp,Payed=payed_min)
     table(data_model$Balance,data_model$Bad)
     data_model <-na.omit(data_model)
     names(data_model)
     #table(data_model$Over.Limit)
     
     ###########################################
     #
     #    Descriptive statistics
     #
     ##########################################
     
     library(ggplot2)
     graphics.off()
    gbc <- ggplot(data_model,aes(x=as.factor(Bad),y=Credit.Limit))+
       geom_boxplot()+
       labs(x="Bad",y="Credit Limit",title = "Credit Limit Against Bad", caption =  "Box plot of Credit Limtit")+
       theme_bw()+ 
       scale_x_discrete(breaks=c("0","1"),
                        labels=c("No", "Yes"))
    gbu <-   ggplot(data_model,aes(x=as.factor(Bad),y=Utilize))+
       geom_boxplot()+
       labs(x="Bad",y="Credit Utilization",title = "Utilization Against Bad", caption =  "Box plot of Utilization")+
       theme_bw()+ 
       scale_x_discrete(breaks=c("0","1"),
                        labels=c("No", "Yes"))
     
    gbrb <- ggplot(data_model) + geom_bar(aes(x=as.factor(Bad),fill=as.factor(Balance)))+
        labs(x="Bad",y="Ending Balance",title = "Ending Balance Against Bad", caption =  "Bar plot of Ending balance")+
        theme_bw()+
       scale_x_discrete(breaks=c("0","1"),
                         labels=c("No", "Yes"))+ labs(fill="Balance") +
       scale_fill_manual("legend", values = c("Low" = "lightblue", "Medium" = "lightgreen", "High" = "red"))
     
    gbrp <-ggplot(data_model) + geom_bar(aes(x=as.factor(Bad),fill=as.factor(Payed)))+
        labs(x="Bad",y="Payed",title = "Payed Against Bad", caption =  "Bar plot of Payed")+
        theme_bw()+
       scale_x_discrete(breaks=c("0","1"), labels=c("No", "Yes"))+ labs(fill="Payed") +
        scale_fill_manual("legend", values = c("0" = "lightgreen", "1" = "red"))
    library(gridExtra)
    
    
    grid.arrange(gbc,gbu,gbrb,gbrp)
    
     names(data_model)
     summary(data_model$Payed)
     ## Indicator varable Build vs Validation
     dim(data_model)
     n <- nrow(data_model)
     set.seed(551)
     
     rows <- sample(1:n, 0.6 * n)
     build <- data_model[rows,]
     validation <- data_model[-rows,]
     # build$Indcator <-"Build"
     # validation$Indcator <-"Validation"
     # retention_indc <-rbind(build,validation)
     # retention_indc <-retention_indc[order(retention_indc$DebtDimId,decreasing = F),]
     # retention_indc <-retention_indc[,c(1,3,2,4:8)]
     ### 
    # names(retention_indc)
    
     
    
     
     
     # ################################################
     #
     #              Build a model
     #
     #################################################
     
     
     # Split data into Build and Test data
  names(build)
     train <- build[,-1]
     test <- validation[,-1]
     names(train)
     nrow(train)
     nrow(test)
     nrow(data_model)
     
     
     
     ### Logistic
     names(train)
     # build model
     model_log <- glm(Bad~.,data = train,family = binomial())
     summary(model_log)
     # Predict
     pred_log <- predict(model_log,newdata=test, type = "response")
     pred_log_bin = ifelse(pred_log > 0.5, 1, 0)
     
     names(train)
     
   ### Tree
     
     library(rpart)
     library(maptree)
     model_tree = rpart(Bad ~ ., data = train)
     summary(model_tree)
     par(mar=c(1,1,1,1))
     draw.tree (model_tree, cex=.8, 
                nodeinfo=TRUE, units="Bad",
                cases="obs",
                digits=1, print.levels=TRUE,
                new=TRUE)
     
    pred_tree <- predict(model_tree,newdata = test)
unique(pred_tree)
     pred_tree_bin <- ifelse(pred_tree>=0.5,1,0)
     conf_mat_tree <- table(pred_tree_bin, test$Bad)
     
     #### MaRs
     
     library(earth)
     library(faraway)
     library(lattice)
     cbind(names(train))
     model_mars <- earth(train[,-2],train[,2],glm = list(family=binomial),degree = 1)
     pred_mars <- predict(model_mars,type='response',newdata=test)
     pred_mars_bin <- ifelse(pred_mars >=0.5,1,0)
     graphics.off()
     plot(model_mars, which=1)
     
     # Append Probability columns to dataset
      # 
      # retention_indc$Log.Prob <- predict(model_log,newdata = rbind(train,test))
      # retention_indc$Tree.Prob <-predict(model_tree,newdata = rbind(train,test))
      # retention_indc$Mars.Prob <-predict(model_mars,newdata = rbind(train,test))
      # View(retention_indc)
      # 
     
  
     names(train)
      ######################################################################
      #
      #
      #                Model Performance
      #
      ######################################################################
     
    
     #### Classification error for cutoff=0.5
     
     ## Logistic
     
       conf_mat_log <- table(pred_log_bin, test$Bad)
       conf_mat_log
       err_log <- (conf_mat_log[1,2] + conf_mat_log[2,1]) / sum(conf_mat_log)
       err_log
     
     ## Tree
       conf_mat_tree <- table(pred_tree_bin, test$Bad)
       conf_mat_tree
       err_tree <-(conf_mat_tree[1,2] + conf_mat_tree[2,1]) / sum(conf_mat_tree)
       err_tree
     
     ## Mars
       conf_mat_mars <- table(pred_mars_bin, test$Bad)
       conf_mat_mars
       err_tree <-(conf_mat_mars[1,2] + conf_mat_mars[2,1]) / sum(conf_mat_mars)
       err_tree
       
     
     #### ROC,KS,Gain,Lift
       
       # Gains  gain and lift
       # -- Logistic
       library(gains)
       #logprobs <- ilogit(predict(modell))
       log.gains <- gains(test$Bad, pred_log, groups = 10)
       log.gains
       graphics.off()
       plot(log.gains)
       plot(log.gains$depth, log.gains$lift, col = "red", xlab = "Depth", ylab = "Lift", xlim = c(0,105), ylim = c(0, 200))
       lines(log.gains$depth, log.gains$lift, col = "red")
       points(log.gains$depth, log.gains$cume.lift, col = "blue")
       lines(log.gains$depth, log.gains$cume.lift, col = "blue")
       title("Logisitic Lift Plot")
       abline(h=100)
      
       #--Tree
       #treeprobs <- predict(newmodel)
       graphics.off()
       
       tree.gains <- gains(test$Bad,pred_tree,groups =10)
       tree.gains
       plot(tree.gains)
       plot(tree.gains$depth, tree.gains$lift, col = "red", ylab = "Lift", xlab = "Depth", xlim = c(0,105), ylim = c(0, 800))
       lines(tree.gains$depth, tree.gains$lift, col = "red")
       l
       points(tree.gains$depth,tree.gains$cume.lift, col = "blue")
       lines(tree.gains$depth,tree.gains$cume.lift, col = "blue")
       title("Decision Tree Lift Plot")
       abline(h=100)
       
       # Mars
       mars.gains <- gains(test$Bad,pred_mars,groups =10)
       mars.gains
       graphics.off()
       #par(mfrow=c(1,2))
       plot(mars.gains)
       plot(mars.gains$depth, mars.gains$lift, col = "red", ylab = "Lift", xlab = "Depth")#, xlim = c(0,105), ylim = c(0, 50))
       lines(mars.gains$depth, mars.gains$lift, col = "red")
       points(mars.gains$depth,mars.gains$cume.lift, col = "blue")
       lines(mars.gains$depth,mars.gains$cume.lift, col = "blue")
       title("Decision Tree Lift Plot")
       abline(h=100)
       
       ###############################
       #
       #    ROC and KS
       #
       ###############################
       
       n <- nrow(test)
       index <- 1:n
       
       # create datasets for each model ordered in descending order by theri respective probabilities
       
       log.roc.data <-cbind(test,prob=pred_log)
       log.roc.data <-log.roc.data[order(log.roc.data$prob,decreasing = T),]  
       
       tree.roc.data <-cbind(test,prob=pred_tree)
       tree.roc.data <-tree.roc.data[order(tree.roc.data$prob,decreasing = T),]  
       head(tree.roc.data)
       
       mars.roc.data <- cbind(test,prob=pred_mars)
       colnames(mars.roc.data) <- c("Credit.Limit","Bad" ,"Utilize","Balance", "Payed" , "prob"  )
       mars.roc.data <- mars.roc.data[order(mars.roc.data$prob,decreasing = T),]
       
       names(log.roc.data)
       names(tree.roc.data)
       names(mars.roc.data)
       head(mars.roc.data)
       # ---------
       
       ### ROC Logistic
          # Cumulative of 1's and 0's
       cumsum_1 <- cumsum(log.roc.data$Bad)
       cumsum_0 <- index - cumsum_1
       
          #  True and False  Positive Percentage
       percent_1 <- cumsum_1/cumsum_1[n]
       percent_0 <- cumsum_0/cumsum_0[n]
       
          # ks
       difference <- abs(percent_1 - percent_0)
       ks <- max(difference)
       ks
       
       percent_tot <- index / (cumsum_1[n] + cumsum_0[n])
       x <- percent_tot[which(difference==ks)]
       y1 <- percent_0[which(difference==ks)]
       y2 <- percent_1[which(difference==ks)]
      
       
          # Plot ROC Curve
       graphics.off()
       
       plot(percent_0, percent_1, main = "ROC Curve for Logistic Regression", xlab = "False Positive Rate", ylab = "True Positive Rate", type = "l", col = "blue")
       abline(0, 1, col = "red")
       legend(0.7, 0.3, c("ROC", "Baseline"), lty = c(1, 1), lwd = c(2.5, 2.5), col = c("blue", "red"))
        # kS curve
       plot(percent_tot, percent_1, main = "KS Curve for Logistic Model", xlab = "Cumulative Depth of File (Population Percentile)", ylab = "Capture Rate", type = "l", col = "blue")
       lines(percent_tot, percent_0, type = "l", col = "red")
       legend(0.6, 0.2, c("True Pos. Rate", "False Pos. Rate","KS"), lty = c(1, 1,2), lwd = c(2.5, 2.5,2), col = c("blue", "red","black"))
       segments(x,y1,x,y2,lty = 2,lwd = 2)
       
       ### ROC Tree
       # Cumulative of 1's and 0's
       #View(tree.roc.data)
       cumsum_t1 <- cumsum(tree.roc.data$Bad)
       cumsum_t0 <- index - cumsum_t1
       
       #  True and False  Positive Percentage
       percent_t1 <- cumsum_t1/cumsum_t1[n]
       percent_t0 <- cumsum_t0/cumsum_t0[n]
       #PERCENT_FIX1 <- ifelse(percent_1<percent_0,(.5-percent_1),percent_1)
       #PERCENT_FIX0 <- ifelse(percent_1<percent_0,(1-percent_0),percent_0)
       #percent_1 <- PERCENT_FIX1
       #percent_1 <- PERCENT_FIX0
       # ks
       difference_t <- abs(percent_t1 - percent_t0)
       kst <- max(difference_t)
       
       percent_tot_t <- index / (cumsum_t1[n] + cumsum_t0[n])
       xt <- percent_tot_t[which(difference_t==kst)]
       yt1 <- percent_t0[which(difference_t==kst)]
       yt2 <- percent_t1[which(difference_t==kst)]
      
     
        percent_t0
        graphics.off()
       par(mfrow=c(1,2))
       # Plot ROC Curve
       
       plot(percent_t0, percent_t1, main = "ROC Curve for Tree Regression", xlab = "False Positive Rate", ylab = "True Positive Rate", type = "l", col = "blue")
       abline(0, 1, col = "red")
       legend(0, 1, c("ROC", "Baseline"), lty = c(1, 1), lwd = c(2.5, 2.5), col = c("blue", "red"))
        
       plot(percent_tot_t, percent_t1, main = "KS Curve for Tree Model", xlab = "Cumulative Depth of File (Population Percentile)", ylab = "Capture Rate", type = "l", col = "blue")
       lines(percent_tot_t, percent_t0, type = "l", col = "red")
       legend(0, 1, c("True Pos. Rate", "False Pos. Rate","KS"), lty = c(1, 1,2), lwd = c(2.5, 2.5,2), col = c("blue", "red","black"),cex = .7)
       segments(xt,yt1,x,yt2,lty = 2,lwd = 2)
       
              
       ### ROC Mars
       # Cumulative of 1's and 0's
      
       cumsum_m1 <- cumsum(mars.roc.data$Bad)
       cumsum_m0 <- index - cumsum_m1
       cumsum_m1
       
       #  True and False  Positive Percentage
       percent_m1 <- cumsum_m1/cumsum_m1[n]
       percent_m0 <- cumsum_m0/cumsum_m0[n]
       percent_m0
       # ks
       difference_m <- abs(percent_m1 - percent_m0)
       ksm <- max(difference_m)
       
       percent_tot_m <- index / (cumsum_m1[n] + cumsum_m0[n])
       xm <- percent_tot_m[which(difference_m==ksm)]
       ym1 <- percent_m0[which(difference_m==ksm)]
       ym2 <- percent_m1[which(difference_m==ksm)]
       
       
       # Plot ROC Curve
       graphics.off()
       par(mfrow=c(1,2))
       plot(percent_m0, percent_m1, main = "ROC Curve for Mars Model", xlab = "False Positive Rate", ylab = "True Positive Rate", type = "l", col = "blue")
       abline(0, 1, col = "red")
       legend(0.4, 0.2, c("ROC", "Baseline"), lty = c(1, 1), lwd = c(2.5, 2.5), col = c("blue", "red"))
       
       plot(percent_tot_m, percent_m1, main = "KS Curve for Mars Model", xlab = "Cumulative Depth of File (Population Percentile)", ylab = "Capture Rate", type = "l", col = "blue")
       lines(percent_tot_m, percent_m0, type = "l", col = "red")
       legend(0.4, 0.15, c("True Pos. Rate", "False Pos. Rate","KS"), lty = c(1, 1,2),cex = .7, lwd = c(2.5, 2.5,2), col = c("blue", "red","black"))
       segments(xm,ym1,x,ym2,lty = 2,lwd = 2)
       
       
       
       # AUC
       
       # source :https://www.r-bloggers.com/calculating-auc-the-area-under-a-roc-curve/
       simple_auc  <- function(TPR, FPR){
         dFPR <- c(diff(FPR), 0)
         dTPR <- c(diff(TPR), 0)
         sum(TPR * dFPR) + sum(dTPR * dFPR)/2
       }
       
       # Logistic
       auc_log <- simple_auc(percent_1, percent_0)
       
       auc_log
       # Tree
       auc_tree <- simple_auc(percent_t1,percent_t0)
       auc_tree
       # Mars
       auc_mars <- simple_auc(percent_m1,percent_m0)
       auc_mars
       
       
       
       
       
       ###########################################
       
       # Create a final dataset 
       
       ###############################
       
       # For build and Valid dataset creat Indicator column
       
       build$Indicator <-"Build"
       validation$Indicator<-"Valid"
       names(build)
       names(validation)
       head(validation)
       head(build)
       
       # Merge the build and validation data
       ret_final <- rbind(build,validation)
       # sort by IDs
       ret_final <- ret_final[order(ret_final$DebtDimId,decreasing = F),]
       dim(ret_final)
       table(ret_final$Indicator)
       
       # Append probabilities
       cbind(names(ret_final))
       newdat <- ret_final[,-c(1,7)]
      
       ret_final$prob_log <- predict(model_log,newdata = newdat)
       head(ret_final,20)
       
       ret_final$prob_tree <- predict(model_tree,newdata = newdat)
       head(ret_final,20)
       
       ret_final$prob_mars <- predict(model_mars,newdata = newdat)
       head(ret_final,20)
       
       names(ret_final)
       
       # Define data.
       
      
       new_dict <-data.frame(var=names(ret_final),
                             desc= c("Customer Id",
                                     "The maximum spending allowed",
                                     "Customer Behaviour",
                                     "customer usage of the credit",
                                     "Category Low, Med, and High",
                                     "Indicator if customer pays minimum due",
                                     "build,if data was used and for model building, and valid for validation",
                                     "Estimated probabiliy by Logistic model",
                                    "Estimated probabiliy by Tree model",
                                    "Estimated probabiliy by Mars model"
                             ))
       head(new_dict)
       
       ### write the dataset and dictionary into excel
       
       # 
       
      write.csv(ret_final, file = "retention_negassi_data.csv")
       write.csv(new_dict, file = "retention_negassi_dict.csv")
       
       
       # ---------------------------------/\/\/\ END /\/\/\-------------------
       
       
       
     