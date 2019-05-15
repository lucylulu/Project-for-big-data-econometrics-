######################################################################################
#############Define machine learning method(h2o) for Double Machine learning ######### 
######################################################################################








#########Function for ATE, SE.ATE, LOGLOSS  ########################################### 

ML <- function(datause,dataout,yvar,d,xvar,xvar_d,method){ 
  form_y <- yvar 
  form_d <- d   
 
  ind_u <-as.vector(h2o.which(datause[,d] =='1'))
  ind_o <-as.vector(h2o.which(dataout[,d] == '1'))
  n_use = nrow(datause)
  n_out = nrow(dataout)
  ##convert data into 
  ###define my_d1x as g(1,x), my_d0x as g(0,x),my_x as m(X)
  my_d1x <- NULL 
  my_d0x <- NULL 
  my_x <- NULL    
  pymse<- 0  
  pdmse<- 0    
  
  # h2o.datause <- as.h2o(datause) 
  # h2o.dataout <- as.h2o(dataout)
  
  h2o.datause <-datause 
  h2o.dataout <- dataout

  
  ###Train model with different models 
  if (method == 'Random Forest'){ 
    
    fit.yd1 <-h2o.randomForest(x=xvar,y=form_y,training_frame = h2o.datause[ind_u,],ntrees = 200,max_depth = 20,mtries = -1,stopping_rounds = 5,stopping_metric = 'MSE') 
     # ###grid search for the best model
    # hyper_params = list(ntrees = c(50,100,200),max_depth= c(10,20),mtries=c(-1,-2))
    # grid_randomf <- h2o.grid('randomForest',x = xvar,y= form_y,training_frame = h2o.datause[ind_u,],hyper_params = hyper_params,stopping_rounds = 5,stopping_tolerance = 0.001)
    # grid_randomf
    # summary(grid_randomf)
    # ## ntrees = 200. max_depth = 10, mtries = -2 
    
    # ###grid search for the best model for data in year 2002
    # hyper_params = list(ntrees = c(50,100,200),max_depth= c(10,20),mtries=c(-1,-2))
    # grid_randomf <- h2o.grid('randomForest',x = xvar,y= form_y,training_frame = h2o.datause,hyper_params = hyper_params,stopping_rounds = 5,stopping_tolerance = 0.001)
    # grid_randomf
    # summary(grid_randomf)
    # ## ntrees = 200. max_depth = 10, mtries = -1
    
    # ###grid search for the best model for data in specification1
    # hyper_params = list(ntrees = c(50,100,200),max_depth= c(10,20),mtries=c(-1,-2))
    # grid_randomf <- h2o.grid('randomForest',x = xvar,y= form_y,training_frame = h2o.datause,validation_frame = h2o.dataout,hyper_params = hyper_params,stopping_rounds = 5,stopping_tolerance = 0.001)
    # grid_randomf
    # ## ntrees = 200. max_depth = 20, mtries = -1
    
    
    
  
    fit.yd0 <-h2o.randomForest(x=xvar,y=form_y,training_frame = h2o.datause[-ind_u,],ntrees = 200,max_depth = 20,mtries = -1,stopping_rounds = 5,stopping_metric = 'MSE') 
    
    # ###grid search for the best model
    # hyper_params = list(ntrees = c(100,200,1000),max_depth= c(10,20))
    # grid_randomf <- h2o.grid('randomForest',x = xvar,y= form_y,training_frame = h2o.datause[-ind_u,],hyper_params = hyper_params)
    # grid_randomf
    # summary(grid_randomf)
    # ## ntrees = 200. max_depth = 10

    
    fit.d <- h2o.randomForest(x=xvar_d,y=form_d,training_frame =h2o.datause,validation_frame = h2o.dataout,ntrees = 200,max_depth = 20,mtries = -2,stopping_rounds = 5,stopping_metric = 'misclassification')
    # ###grid search for the best model
    # hyper_params = list(ntrees = c(100,200,1000),max_depth= c(10,20),mtries=c(-1,-2))
    # grid_randomf <- h2o.grid('randomForest',x = xvar_d,y= form_d,training_frame = h2o.datause,hyper_params = hyper_params,stopping_rounds = 5,stopping_metric = 'misclassification')
    # grid_randomf
    # summary(grid_randomf)
    # ## ntrees = 200. max_depth = 20,mtries=-2 
    

    # ###grid search for the best model for year 2002
    # hyper_params = list(ntrees = c(100,200,1000),max_depth= c(10,20),mtries=c(-1,-2))
    # grid_randomf <- h2o.grid('randomForest',x = xvar_d,y= form_d,training_frame = h2o.datause,hyper_params = hyper_params,stopping_rounds = 5,stopping_metric = 'misclassification')
    # grid_randomf
    # summary(grid_randomf)
    # ## ntrees = 200. max_depth = 10,mtries=-1
    
    
  }  
  
  if (method == 'Gradient Boost'){ 
    fit.yd1 <-h2o.gbm(x=xvar,y=form_y,training_frame = h2o.datause[ind_u,],ntrees = 200,max_depth = 5,learn_rate = 0.01,stopping_rounds = 5,stopping_metric = 'MSE') 
    # ###grid search for the best model
    # hyper_params = list(ntrees = c(50,100,200),max_depth= c(5,10,20),learn_rate = c(0.01,0.05,0.1,0.2))
    # grid_bgm <- h2o.grid('gbm',x = xvar,y= form_y,training_frame = h2o.datause,validation_frame = h2o.dataout,hyper_params = hyper_params,stopping_rounds = 5,stopping_metric = 'MSE')
    # grid_bgm
    # ## ntrees = 200. max_depth = 10,learn_rate = 0.05 
    
    
    # ###grid search for the best model for year 2002
    # hyper_params = list(ntrees = c(50,100,200),max_depth= c(5,10,20),learn_rate = c(0.01,0.05,0.1,0.2))
    # grid_bgm <- h2o.grid('gbm',x = xvar,y= form_y,training_frame = h2o.datause,validation_frame = h2o.dataout,hyper_params = hyper_params,stopping_rounds = 5,stopping_metric = 'MSE')
    # grid_bgm
    # ## ntrees = 200. max_depth = 5,learn_rate = 0.01  
    
    # ###grid search for the best model (specification1 )
    # hyper_params = list(ntrees = c(50,100,200),max_depth= c(5,10,20),learn_rate = c(0.01,0.05,0.1,0.2))
    # grid_bgm <- h2o.grid('gbm',x = xvar,y= form_y,training_frame = h2o.datause,validation_frame = h2o.dataout,hyper_params = hyper_params,stopping_rounds = 5,stopping_metric = 'MSE')
    # grid_bgm
    # ## ntrees = 200. max_depth = 10,learn_rate = 0.05
     

    
    
    fit.yd0 <-h2o.gbm(x=xvar,y=form_y,training_frame = h2o.datause[-ind_u,],ntrees = 200,max_depth = 5,learn_rate = 0.01,stopping_rounds = 5,stopping_metric = 'MSE') 
    
    
    
    
    fit.d <- h2o.gbm(x=xvar_d,y=form_d,training_frame =h2o.datause,validation_frame = h2o.dataout,learn_rate_annealing = 0.1,ntrees = 200,max_depth = 20,stopping_rounds = 5,stopping_metric = 'misclassification') 
    # ###grid search for the best model
    # hyper_params = list(ntrees = c(50,100,200),max_depth= c(5,10,20),learn_rate = c(0.01,0.05,0.1))
    # grid_bgm <- h2o.grid('gbm',x = xvar,y= form_d,distribution = 'bernoulli',training_frame = h2o.datause,validation_frame = h2o.dataout,hyper_params = hyper_params,stopping_rounds = 5,stopping_metric = 'misclassification')
    # grid_bgm
    # ## ntrees = 50. max_depth = 5,learning_rate = 0.1 
    
    
    
    # ###grid search for the best model for year 2002
    # hyper_params = list(ntrees = c(50,100,200),max_depth= c(5,10,20),learn_rate = c(0.01,0.05,0.1))
    # grid_bgm <- h2o.grid('gbm',x = xvar,y= form_d,distribution = 'bernoulli',training_frame = h2o.datause,validation_frame = h2o.dataout,hyper_params = hyper_params,stopping_rounds = 5,stopping_metric = 'misclassification')
    # grid_bgm
    # ## ntrees = 200. max_depth = 20,learning_rate = 0.1
  }  
  

  if (method == 'Lasso'){ 
    fit.yd1 <-h2o.glm(x=xvar,y=form_y,training_frame = h2o.datause[ind_u,],alpha = 1,lambda_search = T,nfolds = 10)
    fit.yd0 <-h2o.glm(x=xvar,y=form_y,training_frame = h2o.datause[-ind_u,],alpha = 1,lambda_search = T,nfolds = 10)
    fit.d <- h2o.glm(x=xvar_d,y=form_d,training_frame =h2o.datause,validation_frame = h2o.dataout,alpha = 1,lambda_search = T,family = 'binomial',nfolds = 10 )
    
  }   
  
  if (method == 'Ridge'){ 
    fit.yd1 <-h2o.glm(x=xvar,y=form_y,training_frame = h2o.datause[ind_u,],alpha = 0,lambda_search = T,nfolds = 10)
    fit.yd0 <-h2o.glm(x=xvar,y=form_y,training_frame = h2o.datause[-ind_u,],alpha = 0,lambda_search = T,nfolds = 10)
    fit.d <- h2o.glm(x=xvar_d,y=form_d,training_frame =h2o.datause,validation_frame = h2o.dataout,alpha = 0,lambda_search = T,family = 'binomial',nfolds = 10 )
  }   
  
  if (method == 'Elastic Net'){ 
    fit.yd1 <-h2o.glm(x=xvar,y=form_y,training_frame = h2o.datause[ind_u,],lambda_search = T,nfolds = 10)
    fit.yd0 <-h2o.glm(x=xvar,y=form_y,training_frame = h2o.datause[-ind_u,],lambda_search = T,nfolds = 10)
    fit.d <- h2o.glm(x=xvar_d,y=form_d,training_frame =h2o.datause,validation_frame = h2o.dataout,lambda_search = T,family = 'binomial',nfolds = 10 )
    
  }   
  
  if (method == 'Neural Net'){ 
    fit.yd1 <-h2o.deeplearning(x=xvar,y=form_y,training_frame = h2o.datause[ind_u,],activation = 'Tanh',hidden = c(100,50,20),stopping_rounds = 5,stopping_metric = 'MSE') 
    # ###grid search for the best model
    # hyper_params = list(hidden=list(100,c(100,50),c(100,50,20),c(100,50,20,10)),activation=c("Tanh","Rectifier"))
    # grid_nn <- h2o.grid('deeplearning',x = xvar,y= form_y,training_frame = h2o.datause,validation_frame = h2o.dataout,hyper_params = hyper_params,stopping_rounds = 5,stopping_metric = 'MSE',epochs = 1000)
    # grid_nn
    # ##  hidden = [100,50,20],activation = Rectifier 
    
    
    # ###grid search for the best model for year 2002
    # hyper_params = list(hidden=list(100,c(100,50),c(100,50,20),c(100,50,20,10)),activation=c("Tanh","Rectifier"))
    # grid_nn <- h2o.grid('deeplearning',x = xvar,y= form_y,training_frame = h2o.datause,validation_frame = h2o.dataout,hyper_params = hyper_params,stopping_rounds = 5,stopping_metric = 'MSE',epochs = 1000)
    # grid_nn
    # ##  hidden = [100,50,20],activation = Tanh 
    
    
    
    # ###grid search for the best model( specification 1)
    # hyper_params = list(hidden=list(100,c(100,50),c(100,50,20),c(100,50,20,10)),activation=c("Tanh","Rectifier"))
    # grid_nn <- h2o.grid('deeplearning',x = xvar,y= form_y,training_frame = h2o.datause,validation_frame = h2o.dataout,hyper_params = hyper_params,stopping_rounds = 5,stopping_metric = 'MSE',epochs = 1000)
    # grid_nn
    # ##  hidden = [100,50,20],activation = Rectifier
    fit.yd0 <-h2o.deeplearning(x=xvar,y=form_y,training_frame = h2o.datause[-ind_u,],hidden = c(100,50,20),activation = 'Tanh',stopping_rounds = 5,stopping_metric = 'MSE')
    
    
  
    
    
    fit.d <- h2o.deeplearning(x=xvar_d,y=form_d,training_frame =h2o.datause,validation_frame = h2o.dataout,activation = 'Tanh',hidden = c(100,50,20),stopping_rounds = 5,stopping_metric = 'misclassification')
  # ###grid search for the best model
  #   hyper_params = list(hidden=list(100,c(100,50),c(100,50,20),c(100,50,20,10)),activation=c("Tanh","Rectifier"))
  #   grid_nn <- h2o.grid('deeplearning',x = xvar_d,y= form_d,training_frame = h2o.datause,validation_frame = h2o.dataout,hyper_params = hyper_params,stopping_rounds = 5,stopping_metric = 'misclassification',epochs = 1000)
  #   grid_nn  
  #   ## hidden = [100,50], activation = Tanh 
    
    
    # ###grid search for the best model for year 2002
    #   hyper_params = list(hidden=list(100,c(100,50),c(100,50,20),c(100,50,20,10)),activation=c("Tanh","Rectifier"))
    #   grid_nn <- h2o.grid('deeplearning',x = xvar_d,y= form_d,training_frame = h2o.datause,validation_frame = h2o.dataout,hyper_params = hyper_params,stopping_rounds = 5,stopping_metric = 'misclassification',epochs = 1000)
    #   grid_nn
    #   ## hidden = [100,50,20], activation = Tanh
    
  }   
  
  ###using the model to compute outcome we need: my_d1x,my_d0x,my_x 
  
  my_d1x <- h2o.predict(fit.yd1,newdata = h2o.dataout)  
  
  my_d0x <- predict(fit.yd0,newdata = h2o.dataout)   
  pymse <- (sum((h2o.dataout[ind_o,form_y]-my_d1x[ind_o])^2)+sum((h2o.dataout[-ind_o,form_y]- my_d0x[-ind_o])^2))/n_out
  
  my_x <-h2o.predict(fit.d,newdata = h2o.dataout,type = 'prob')[,2]   
  
  logloss_d <- h2o.logloss(fit.d,valid = T) 
  
  return(list(my_d1x =as.matrix( my_d1x),my_d0x = as.matrix(my_d0x),my_x =as.matrix( my_x),pymse=pymse,logloss_d = logloss_d))

}

####The followings are the respective machine learning method
# 
# cond_rf<-function(datause,dataout,yvar,d,xx,xvar){  
#   form_y <- yvar 
#   form_d <- d 
#   form_x <- xx 
#   ind_u <- which(datause[,d]==1)
#   ind_o <- which(dataout[,d]==1)    
#   n_use = nrow(datause)
#   n_out = nrow(dataout)
#   ##convert data into 
#   ###define my_d1x as g(1,x), my_d0x as g(0,x),my_x as m(X)
#   my_d1x <- NULL 
#   my_d0x <- NULL 
#   my_x <- NULL    
#   pymse<- 0  
#   pdmse<- 0    
#   
#   #change data into h2o form  
#   h2o.datause <- as.h2o(datause) 
#   h2o.dataout <- as.h2o(dataout)
#   
#   
#   ###using h2o to run random forset  (using the default) 
#   # fit.yd1 <- h2o.randomForest(x=form_x,y= form_y,h2o.datause1) 
#   # my_d1x <- h2o.predict(fit.yd1,newdata = h2o.dataout)   
#   fit.yd1 <-h2o.randomForest(x=xvar,y=form_y,training_frame = h2o.datause[ind_u,])
#   my_d1x <- h2o.predict(fit.yd1,newdata = h2o.dataout)  
#   pymse <- pymse + sum((h2o.dataout[ind_o,form_y]-my_d1x[ind_o])^2)
#   
#   
#   
#   fit.yd0 <-h2o.randomForest(x=xvar,y=form_y,training_frame = h2o.datause[-ind_u,])
#   my_d0x <- predict(fit.yd0,newdata = h2o.dataout)   
#   
#   pymse <- pymse+sum((h2o.dataout[-ind_o,form_y]- my_d0x[-ind_o])^2)
#   ###pymse 
#   pymse <- pymse/n_out
#   
#   fit.d <- h2o.randomForest(x=xvar,y=form_d,training_frame =h2o.datause,validation_frame = h2o.dataout )
#   my_x <-h2o.predict(fit.d,newdata = h2o.dataout,type = 'prob')[,2] 
#   my_x<- as.matrix(my_x)
#   dout<- as.numeric(dataout[,form_d]$ALC) -1  
#   logloss_d <- Logloss(dout,my_x)/n_out
#   
#   return(list(my_d1x =as.matrix( my_d1x),my_d0x = as.matrix(my_d0x),my_x = my_x,pymse=pymse,logloss_d = logloss_d))
# }
# 
# #try h2o for machine learning 
# cond_bs<-function(datause,dataout,yvar,d,xx,xvar){  
#   form_y <- yvar 
#   form_d <- d 
#   form_x <- xx 
#   ind_u <- which(datause[,d]==1)
#   ind_o <- which(dataout[,d]==1)    
#   n_use = nrow(datause)
#   n_out = nrow(dataout)
#   ##convert data into 
#   ###define my_d1x as g(1,x), my_d0x as g(0,x),my_x as m(X)
#   my_d1x <- NULL 
#   my_d0x <- NULL 
#   my_x <- NULL    
#   pymse<- 0  
#   pdmse<- 0    
#   
#   #change data into h2o form  
#   h2o.datause <- as.h2o(datause) 
#   h2o.dataout <- as.h2o(dataout)
#   
#   
#   ###using h2o to run random forset  (using the default) 
#   # fit.yd1 <- h2o.randomForest(x=form_x,y= form_y,h2o.datause1) 
#   # my_d1x <- h2o.predict(fit.yd1,newdata = h2o.dataout)   
#   fit.yd1 <-h2o.gbm(x=xvar,y=form_y,training_frame = h2o.datause[ind_u,])
#   my_d1x <- h2o.predict(fit.yd1,newdata = h2o.dataout)  
#   my_d1x[ind_o] 
#   pymse <- pymse + sum((h2o.dataout[ind_o,form_y]-my_d1x[ind_o])^2)
#   
#   
#   
#   fit.yd0 <-h2o.gbm(x=xvar,y=form_y,training_frame = h2o.datause[-ind_u,])
#   my_d0x <- predict(fit.yd0,newdata = h2o.dataout)   
#   
#   pymse <- pymse+sum((h2o.dataout[-ind_o,form_y]- my_d0x[-ind_o])^2)
#   ###pymse 
#   pymse <- pymse/n_out
#   
#   fit.d <- h2o.gbm(x=xvar,y=form_d,training_frame =h2o.datause,validation_frame = h2o.dataout )
#   my_x <-h2o.predict(fit.d,newdata = h2o.dataout,type = 'prob')[,2] 
#   my_x<- as.matrix(my_x)
#   dout<- as.numeric(dataout[,form_d]$ALC) -1  
#   logloss_d <- Logloss(dout,my_x)/n_out
#   
#   return(list(my_d1x =as.matrix( my_d1x),my_d0x = as.matrix(my_d0x),my_x = my_x,pymse=pymse,logloss_d = logloss_d))
# }
# 
# 
# ####consider regression-base : LASSO,ELASTIC NET,NEURAL NETWORK  
# 
# ##neural network
# cond_nt<-function(datause,dataout,yvar,d,xx,xvar){  
#   form_y <- yvar 
#   form_d <- d 
#   form_x <- xx 
#   ind_u <- which(datause[,d]==1)
#   ind_o <- which(dataout[,d]==1)    
#   n_use = nrow(datause)
#   n_out = nrow(dataout)
#   ##convert data into 
#   ###define my_d1x as g(1,x), my_d0x as g(0,x),my_x as m(X)
#   my_d1x <- NULL 
#   my_d0x <- NULL 
#   my_x <- NULL    
#   pymse<- 0  
#   pdmse<- 0    
#   
#   #change data into h2o form  
#   h2o.datause <- as.h2o(datause) 
#   h2o.dataout <- as.h2o(dataout)
#   
#   
#   ###using h2o to run random forset  (using the default) 
#   # fit.yd1 <- h2o.randomForest(x=form_x,y= form_y,h2o.datause1) 
#   # my_d1x <- h2o.predict(fit.yd1,newdata = h2o.dataout)   
#   fit.yd1 <-h2o.deeplearning(x=xvar,y=form_y,training_frame = h2o.datause[ind_u,])
#   my_d1x <- h2o.predict(fit.yd1,newdata = h2o.dataout)  
#   my_d1x[ind_o] 
#   pymse <- pymse + sum((h2o.dataout[ind_o,form_y]-my_d1x[ind_o])^2)
#   
#   
#   
#   fit.yd0 <-h2o.deeplearning(x=xvar,y=form_y,training_frame = h2o.datause[-ind_u,])
#   my_d0x <- predict(fit.yd0,newdata = h2o.dataout)   
#   
#   pymse <- pymse+sum((h2o.dataout[-ind_o,form_y]- my_d0x[-ind_o])^2)
#   ###pymse 
#   pymse <- pymse/n_out
#   
#   fit.d <- h2o.deeplearning(x=xvar,y=form_d,training_frame =h2o.datause,validation_frame = h2o.dataout )
#   my_x <-h2o.predict(fit.d,newdata = h2o.dataout,type = 'prob')[,2] 
#   my_x<- as.matrix(my_x)
#   dout<- as.numeric(dataout[,form_d]$ALC) -1  
#   logloss_d <- Logloss(dout,my_x)/n_out
#   
#   return(list(my_d1x =as.matrix( my_d1x),my_d0x = as.matrix(my_d0x),my_x = my_x,pymse=pymse,logloss_d = logloss_d))
# }
# 
# ##lasso 
# cond_lasso<-function(datause,dataout,yvar,d,xx,xvar){  
#   form_y <- yvar 
#   form_d <- d 
#   form_x <- xx 
#   ind_u <- which(datause[,d]==1)
#   ind_o <- which(dataout[,d]==1)    
#   n_use = nrow(datause)
#   n_out = nrow(dataout)
#   ##convert data into 
#   ###define my_d1x as g(1,x), my_d0x as g(0,x),my_x as m(X)
#   my_d1x <- NULL 
#   my_d0x <- NULL 
#   my_x <- NULL    
#   pymse<- 0  
#   pdmse<- 0    
#   
#   #change data into h2o form  
#   h2o.datause <- as.h2o(datause) 
#   h2o.dataout <- as.h2o(dataout)
#   
#   
#   ###using h2o to run random forset  (using the default) 
#   # fit.yd1 <- h2o.randomForest(x=form_x,y= form_y,h2o.datause1) 
#   # my_d1x <- h2o.predict(fit.yd1,newdata = h2o.dataout)   
#   fit.yd1 <-h2o.glm(x=xvar,y=form_y,training_frame = h2o.datause[ind_u,],alpha = 1,lambda_search = T)
#   my_d1x <- h2o.predict(fit.yd1,newdata = h2o.dataout)  
#   pymse <- pymse + sum((h2o.dataout[ind_o,form_y]-my_d1x[ind_o])^2)
#   
#   
#   
#   fit.yd0 <-h2o.glm(x=xvar,y=form_y,training_frame = h2o.datause[-ind_u,],alpha = 1,lambda_search = T)
#   my_d0x <- predict(fit.yd0,newdata = h2o.dataout)   
#   
#   pymse <- pymse+sum((h2o.dataout[-ind_o,form_y]- my_d0x[-ind_o])^2)
#   ###pymse 
#   pymse <- pymse/n_out
#   
#   fit.d <- h2o.glm(x=xvar,y=form_d,training_frame =h2o.datause,validation_frame = h2o.dataout,alpha = 1,lambda_search = T,family = 'binomial' )
#   my_x <-h2o.predict(fit.d,newdata = h2o.dataout,type = 'prob')[,2] 
#   my_x<- as.matrix(my_x)
#   dout<- as.numeric(dataout[,form_d]$ALC) -1  
#   logloss_d <- Logloss(dout,my_x)/n_out
#   
#   return(list(my_d1x =as.matrix( my_d1x),my_d0x = as.matrix(my_d0x),my_x = my_x,pymse=pymse,logloss_d = logloss_d))
# }
# ##ridge 
# cond_ridge<-function(datause,dataout,yvar,d,xx,xvar){  
#   form_y <- yvar 
#   form_d <- d 
#   form_x <- xx 
#   ind_u <- which(datause[,d]==1)
#   ind_o <- which(dataout[,d]==1)    
#   n_use = nrow(datause)
#   n_out = nrow(dataout)
#   ##convert data into 
#   ###define my_d1x as g(1,x), my_d0x as g(0,x),my_x as m(X)
#   my_d1x <- NULL 
#   my_d0x <- NULL 
#   my_x <- NULL    
#   pymse<- 0  
#   pdmse<- 0    
#   
#   #change data into h2o form  
#   h2o.datause <- as.h2o(datause) 
#   h2o.dataout <- as.h2o(dataout)
#   
#   
#   ###using h2o to run random forset  (using the default) 
#   # fit.yd1 <- h2o.randomForest(x=form_x,y= form_y,h2o.datause1) 
#   # my_d1x <- h2o.predict(fit.yd1,newdata = h2o.dataout)   
#   fit.yd1 <-h2o.glm(x=xvar,y=form_y,training_frame = h2o.datause[ind_u,],alpha = 0,lambda_search = T)
#   my_d1x <- h2o.predict(fit.yd1,newdata = h2o.dataout)  
#   pymse <- pymse + sum((h2o.dataout[ind_o,form_y]-my_d1x[ind_o])^2)
#   
#   
#   
#   fit.yd0 <-h2o.glm(x=xvar,y=form_y,training_frame = h2o.datause[-ind_u,],alpha = 0,lambda_search = T)
#   my_d0x <- predict(fit.yd0,newdata = h2o.dataout)   
#   
#   pymse <- pymse+sum((h2o.dataout[-ind_o,form_y]- my_d0x[-ind_o])^2)
#   ###pymse 
#   pymse <- pymse/n_out
#   
#   fit.d <- h2o.glm(x=xvar,y=form_d,training_frame =h2o.datause,validation_frame = h2o.dataout,alpha = 0,lambda_search = T,family = 'binomial' )
#   my_x <-h2o.predict(fit.d,newdata = h2o.dataout,type = 'prob')[,2] 
#   my_x<- as.matrix(my_x)
#   dout<- as.numeric(dataout[,form_d]$ALC) -1  
#   logloss_d <- Logloss(dout,my_x)/n_out
#   
#   return(list(my_d1x =as.matrix( my_d1x),my_d0x = as.matrix(my_d0x),my_x = my_x,pymse=pymse,logloss_d = logloss_d))
# }
# ###elastic net 
# cond_en<-function(datause,dataout,yvar,d,xx,xvar){  
#   form_y <- yvar 
#   form_d <- d 
#   form_x <- xx 
#   ind_u <- which(datause[,d]==1)
#   ind_o <- which(dataout[,d]==1)    
#   n_use = nrow(datause)
#   n_out = nrow(dataout)
#   ##convert data into 
#   ###define my_d1x as g(1,x), my_d0x as g(0,x),my_x as m(X)
#   my_d1x <- NULL 
#   my_d0x <- NULL 
#   my_x <- NULL    
#   pymse<- 0  
#   pdmse<- 0    
#   
#   #change data into h2o form  
#   h2o.datause <- as.h2o(datause) 
#   h2o.dataout <- as.h2o(dataout)
#   
#   
#   ###using h2o to run random forset  (using the default) 
#   # fit.yd1 <- h2o.randomForest(x=form_x,y= form_y,h2o.datause1) 
#   # my_d1x <- h2o.predict(fit.yd1,newdata = h2o.dataout)   
#   fit.yd1 <-h2o.glm(x=xvar,y=form_y,training_frame = h2o.datause[ind_u,],lambda_search = T)
#   my_d1x <- h2o.predict(fit.yd1,newdata = h2o.dataout)  
#   pymse <- pymse + sum((h2o.dataout[ind_o,form_y]-my_d1x[ind_o])^2)
#   
#   
#   
#   fit.yd0 <-h2o.glm(x=xvar,y=form_y,training_frame = h2o.datause[-ind_u,],lambda_search = T)
#   my_d0x <- predict(fit.yd0,newdata = h2o.dataout)   
#   
#   pymse <- pymse+sum((h2o.dataout[-ind_o,form_y]- my_d0x[-ind_o])^2)
#   ###pymse 
#   pymse <- pymse/n_out
#   
#   fit.d <- h2o.glm(x=xvar,y=form_d,training_frame =h2o.datause,validation_frame = h2o.dataout,lambda_search = T,family = 'binomial' )
#   my_x <-h2o.predict(fit.d,newdata = h2o.dataout,type = 'prob')[,2] 
#   my_x<- as.matrix(my_x)
#   dout<- as.numeric(dataout[,form_d]$ALC) -1  
#   logloss_d <- Logloss(dout,my_x)/n_out
#   
#   return(list(my_d1x =as.matrix( my_d1x),my_d0x = as.matrix(my_d0x),my_x = my_x,pymse=pymse,logloss_d = logloss_d))
# }
# 
# 




############Function for ATE#############
ATE <- function(y, d, my_d1x, my_d0x, md_x){
  return( mean( (d * (y - my_d1x) / md_x) -  ((1 - d) * (y - my_d0x) / (1 - md_x))  + my_d1x - my_d0x ) );
} 

############Function for se.ATE ###########
SE.ATE <- function(y, d, my_d1x, my_d0x, md_x)
{
  return( sd( (d * (y - my_d1x) / md_x) -  ((1 - d) * (y - my_d0x) / (1 - md_x)) + my_d1x - my_d0x )/sqrt(length(y)) );
}

# ########Function for logloss #################
# Logloss <- function(true_lable,predict_lable) { 
#   logloss<- sum(-(true_lable*log(predict_lable)+(1-true_lable)*log(1-predict_lable)),na.rm = T)
#   return(logloss)
# }

##########Functions for getting the final result table (with and without common support)###############
getresult<-function(data,K,yvar,xvar,xvar_d,d,Methods,M){  
  
  split <- runif(nrow(data))
  cvgroup<-as.numeric(cut(split,breaks = quantile(split,probs = seq(0,1,1/K)),include.lowest = T))
  
  nmethod <- length(Methods) 
  ###create empty tables for results
  ate.table <- matrix(0,1,nmethod)
  se.ate.table <- matrix(0,1,nmethod) 
  mse.out.table <- matrix(0,1,nmethod) 
  logloss.out.table <- matrix(0,1,nmethod)
  
  
  for ( m in (1:M)){
    ####for all machine learning methods in Methods 
    for (i in (1:nmethod)){   
      method <- Methods[i]
      
      ate<- 0
      se.ate <- 0   
      mse.out <- 0 
      logloss.out <- 0 
      
      for ( j in 1:K){
        ii <- cvgroup == j
        nii <- cvgroup != j 
        datause = data[nii,]
        dataout = data[ii,] 
        calxx<- ML(datause = datause, dataout = dataout,yvar= yvar,d=d,method = method,xvar = xvar,xvar_d = xvar_d)
        yout = as.matrix(dataout[,yvar] )
        dout =as.matrix(as.numeric(dataout[,d]$ALC)-1)
        atedata <- cbind(yout,dout,calxx$my_d1x,calxx$my_d0x,calxx$my_x)  
        if(length( which(c(atedata[,5]==0 |atedata[,5]== 1 ))) != 0){ 
          atedata <- atedata[-which(c(atedata[,5]==0 |atedata[,5]== 1 )),]
        }
        ate <- ate + ATE(atedata[,1],atedata[,2],atedata[,3],atedata[,4],atedata[,5]) 
        se.ate <-se.ate + (SE.ATE(atedata[,1],atedata[,2],atedata[,3],atedata[,4],atedata[,5]))^2 
        mse.out <- mse.out+ calxx$pymse
        logloss.out <- logloss.out + calxx$logloss_d 
      }   
      
      ate.table[,i] <-ate.table[,i]+ ate/K 
      se.ate.table[,i] <- se.ate.table[,i]+sqrt(se.ate/(K^2))
      mse.out.table[,i] <-mse.out.table[,i] + sqrt(mse.out/K )
      logloss.out.table[,i] <-logloss.out.table[,i]+ logloss.out/K
      
    } 
  } 
  ate.table <- ate.table/M 
  se.ate.table <- se.ate.table/M 
  mse.out.table <- mse.out.table/M 
  logloss.out.table <- logloss.out.table/M
  
  return(list(ate.table = ate.table,se.ate.table = se.ate.table,mse.out.table = mse.out.table,logloss.out.table = logloss.out.table))
}

getresult_commonsupport<-function(data,K,yvar,xvar,xvar_d,d,Methods,M){  
  nmethod <- length(Methods) 
  ###create empty tables for results
  ate.table <- matrix(0,1,nmethod)
  se.ate.table <- matrix(0,1,nmethod) 
  mse.out.table <- matrix(0,1,nmethod) 
  logloss.out.table <- matrix(0,1,nmethod)
  h2o.data <- as.h2o(data)
  ####for all machine learning methods in Methods   
  for ( m in (1:M)){ 
    split <- runif(nrow(data))
    cvgroup<-as.numeric(cut(split,breaks = quantile(split,probs = seq(0,1,1/K)),include.lowest = T))
    
    ####for all machine learning methods in Methods 
    for (i in (1:nmethod)){   
      method <- Methods[i]
      
      ate<- 0
      se.ate <- 0   
      mse.out <- 0 
      logloss.out <- 0 
      
      for ( j in 1:K){
        ii <-which(cvgroup == j)
        nii <-which (cvgroup != j )
        datause = h2o.data[nii,]
        dataout = h2o.data[ii,] 
        calxx<- ML(datause = datause, dataout = dataout,yvar= yvar,d=d,method = method,xvar = xvar,xvar_d = xvar_d)
        yout = as.matrix(dataout[,yvar] )
        dout =as.matrix(as.numeric(dataout[,d]$ALC)-1)
        atedata <- cbind(yout,dout,calxx$my_d1x,calxx$my_d0x,calxx$my_x)  
        
        ###common support : keep only observations with (0.1<p<0.9) 
        atedata <- atedata[ which(atedata[,5]>0.1 &atedata[,5]<0.9), ]
        
        
        
        
        ate <- ate + ATE(atedata[,1],atedata[,2],atedata[,3],atedata[,4],atedata[,5]) 
        se.ate <-se.ate + (SE.ATE(atedata[,1],atedata[,2],atedata[,3],atedata[,4],atedata[,5]))^2 
        mse.out <- mse.out+ calxx$pymse
        logloss.out <- logloss.out + calxx$logloss_d 
      }   
      
      ate.table[,i] <-ate.table[,i]+ ate/K 
      se.ate.table[,i] <- se.ate.table[,i]+sqrt(se.ate/(K^2))
      mse.out.table[,i] <- mse.out.table[,i]+ sqrt(mse.out/K )
      logloss.out.table[,i] <-logloss.out.table[,i]+logloss.out/K
      
    } 
  } 
  ate.table <- ate.table/M 
  se.ate.table <- se.ate.table/M 
  mse.out.table <- mse.out.table/M 
  logloss.out.table <- logloss.out.table/M
  
  return(list(ate.table = ate.table,se.ate.table = se.ate.table,mse.out.table = mse.out.table,logloss.out.table = logloss.out.table))
}





gettable <- function(data,K,yvar,xvar,xvar_d,d,Methods,M){
  
  
  all <- getresult(data,K,yvar = yvar,xvar = xvar,d= d,xvar_d = xvar_d,Methods = Methods, M=M)
  
  ####using xtable to compute output table 
  
  result <- matrix(0,length(all),length(Methods)) 
  colnames(result) <- Methods 
  rownames(result) <- c('ATE','se(ATE)','mse.v','logloss.v')
  for ( i in (1:nrow(result))){
    result[i,]<- all[[i]]
  }  
  result <- round(result,digits = 3)
  
  for ( i in 1:ncol(result)){
    for(j in 2:nrow(result)){
      result[j,i]<-paste("(", result[j,i], ")", sep="")
    }
  }
  
  
  
  resulttable <-xtable(result, digits=3)
  
  return(list(model = all,resulttable = resulttable))
  
}

gettable_commonsupport <- function(data,K,yvar,xvar,xvar_d,d,Methods,M){
  
  
  all <- getresult_commonsupport(data,K,yvar = yvar,xvar = xvar,d= d,xvar_d = xvar_d,Methods = Methods,M)
  
  ####using xtable to compute output table 
  
  result <- matrix(0,length(all),length(Methods)) 
  colnames(result) <- Methods 
  rownames(result) <- c('ATE','se(ATE)','mse.v','logloss.v')
  for ( i in (1:nrow(result))){
    result[i,]<- all[[i]]
  }  
  result <- round(result,digits = 3)
  
  for ( i in 1:ncol(result)){
    for(j in 2:nrow(result)){
      result[j,i]<-paste("(", result[j,i], ")", sep="")
    }
  }
  
  
  
  resulttable <-xtable(result, digits=3)
  
  return(list(model = all,resulttable = resulttable))
  
}

