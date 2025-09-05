#'@title netdata
#'@name  netdata
#'
#'@param fit Fill in the model you want to analyze. Support survival analysis and logistic regression.
#'@param newdata If the decision curve of the validation set is to be analysed. Fill in the validation set data here.
#'@param timepoint If it is a survival analysis, fill in the point in time you need to study. The default is the median time.
#'@param cmprsk If it is a competitive risk model, select TRUE here.
#'@param modelnames Defines the name of the generated image model.
#'@param y.min The maximum value of the negative part of the picture. Generally defaults to positive values multiplied by 0.4.
#'@param xstop The maximum value of the X-axis of the picture.
#'@param y.max The maximum value of the Y-axis. The default value is the maximum net benefit.
#'@param pyh The height at which the bars are plotted cannot exceed y.min.
#'@param relcol The colour of the relevant part of the bar. The default is red.
#'@param irrelcol The colour of the irrelevant part of the bar. The default is blue.
#'@param relabel Relevance Tags.
#'@param irrellabel No relevant tags.
#'@param text.size Font size.
#'@param text.col The colour of the font.
#'@param colbar The default is true, and if false is selected, bar plotting is cancelled.
#'@param merge If true is selected it will merge the two long zones.
#'@param threshold.text The default is FALSE, if TRUE is selected, a text message for the threshold will be added.
#'@param threshold.line The default is FALSE, and if TRUE is selected, lines for the threshold will be added.
#'@param nudge_x Used to adjust the x-axis position of the point where the threshold is located.
#'@param nudge_y Used to adjust the y-axis position of the point where the threshold is located.
#'@param threshold.linetype The line shape of the threshold line.
#'@param threshold.linewidth The line width of the threshold line.
#'@param threshold.linecol The colour of the threshold line.
#'@param po.text.size The size of the threshold point text.
#'@param po.text.col The colour of the threshold point text.
#'@param po.text.fill The background of the threshold point text.
#'@param liftpec Threshold point left displacement.
#'@param rightpec Threshold point right displacement.
#'@param legend.position Set the position of the legend.
#'@importFrom "stats" "median"
#'@importFrom "stats" "predict"
#'@importFrom "stats" "model.matrix"
#'@importFrom "xgboost" "xgb.DMatrix"
#'@import "ggplot2"
#'@import "reshape2"
#'@import "survival"
#'@import "xgboost"
#'
#'
#'@return A data used for plotting.
#'

netdata<-function(fit,newdata=NULL,timepoint='median',cmprsk=FALSE,modelnames=NULL,merge=FALSE,y.min=NULL,xstop=NULL,y.max=NULL,
                  pyh=NULL,relcol="#c01e35",irrelcol="#0151a2",relabel="Nomogram relevant",
                  irrellabel="Nomogram irrelevant",text.size=4.5,text.col="green",colbar=TRUE,
                  threshold.text=FALSE,threshold.line=FALSE,nudge_x = 0,nudge_y = 0,
                  threshold.linetype=2,threshold.linewidth = 1.2,threshold.linecol="black",
                  po.text.size=4,po.text.col="black",po.text.fill="white",liftpec=NULL,rightpec=NULL,
                  legend.position = c(0.85,0.75)) {
  UseMethod('netdata')
}


#'@title netdata.coxph
#'@name  netdata.coxph
#'@param fit Fill in the model you want to analyze. Support survival analysis and logistic regression.
#'@param newdata If the decision curve of the validation set is to be analysed. Fill in the validation set data here.
#'@param timepoint If it is a survival analysis, fill in the point in time you need to study. The default is the median time.
#'@param cmprsk If it is a competitive risk model, select TRUE here.
#'@param modelnames Defines the name of the generated image model.
#'@param y.min The maximum value of the negative part of the picture. Generally defaults to positive values multiplied by 0.4.
#'@param xstop The maximum value of the X-axis of the picture.
#'@param y.max The maximum value of the Y-axis. The default value is the maximum net benefit.
#'@param pyh The height at which the bars are plotted cannot exceed y.min.
#'@param relcol The colour of the relevant part of the bar. The default is red.
#'@param irrelcol The colour of the irrelevant part of the bar. The default is blue.
#'@param relabel Relevance Tags.
#'@param irrellabel No relevant tags.
#'@param text.size Font size.
#'@param text.col The colour of the font.
#'@param colbar The default is true, and if false is selected, bar plotting is cancelled.
#'@param merge If true is selected it will merge the two long zones.
#'@param threshold.text The default is FALSE, if TRUE is selected, a text message for the threshold will be added.
#'@param threshold.line The default is FALSE, and if TRUE is selected, lines for the threshold will be added.
#'@param nudge_x Used to adjust the x-axis position of the point where the threshold is located.
#'@param nudge_y Used to adjust the y-axis position of the point where the threshold is located.
#'@param threshold.linetype The line shape of the threshold line.
#'@param threshold.linewidth The line width of the threshold line.
#'@param threshold.linecol The colour of the threshold line.
#'@param po.text.size The size of the threshold point text.
#'@param po.text.col The colour of the threshold point text.
#'@param po.text.fill The background of the threshold point text.
#'@param liftpec Threshold point left displacement.
#'@param rightpec Threshold point right displacement.
#'@param legend.position Set the position of the legend.
#'@export

netdata.coxph<-function(fit,newdata=NULL,timepoint='median',cmprsk=FALSE,modelnames=NULL,merge=FALSE,y.min=NULL,xstop=NULL,y.max=NULL,
                        pyh=NULL,relcol="#c01e35",irrelcol="#0151a2",relabel="Nomogram relevant",
                        irrellabel="Nomogram irrelevant",text.size=4.5,text.col="green",colbar=TRUE,
                        threshold.text=FALSE,threshold.line=FALSE,nudge_x = 0,nudge_y = 0,
                        threshold.linetype=2,threshold.linewidth = 1.2,threshold.linecol="black",
                        po.text.size=4,po.text.col="black",po.text.fill="white",liftpec=NULL,rightpec=NULL,
                        legend.position = c(0.85,0.75)) {
  if (missing(fit)) stop("fit is missing .")
  fit<-fit;
  if (is.null(modelnames)) {modelnames<-"model"
  } else {
    modelnames<-modelnames
  }
  all.var<-all.vars(fit$terms)
  modely<-model.y(fit)
  modelx<-model.x(fit)
  data<-modeldata(fit)
  if (timepoint=='median') {
    timepo1<-median(data[,modely[1]])
  } else {timepo1<-timepoint}
  if (!is.null(newdata)) {
    newdata<-newdata
  }
  if (!is.null(newdata)) {
    newdata$prob1 <- c(1-(summary(survfit(fit, newdata=newdata), times=timepo1)$surv))
  } else {
    data$prob1 = c(1- (summary(survfit(fit, newdata=data), times=timepo1)$surv))
  }
  if (!is.null(newdata)) {
    net<-stdca(data=newdata, outcome=modely[2], ttoutcome=modely[1], timepoint=timepo1, predictors="prob1", probability=FALSE,
               graph=F)
  } else {
    net<-stdca(data=data, outcome=modely[2], ttoutcome=modely[1], timepoint=timepo1, predictors="prob1", probability=FALSE,
               graph=F)
  }
  net
}


#'@title netdata.crr
#'@name  netdata.crr
#'@param fit Fill in the model you want to analyze. Support survival analysis and logistic regression.
#'@param newdata If the decision curve of the validation set is to be analysed. Fill in the validation set data here.
#'@param timepoint If it is a survival analysis, fill in the point in time you need to study. The default is the median time.
#'@param cmprsk If it is a competitive risk model, select TRUE here.
#'@param modelnames Defines the name of the generated image model.
#'@param y.min The maximum value of the negative part of the picture. Generally defaults to positive values multiplied by 0.4.
#'@param xstop The maximum value of the X-axis of the picture.
#'@param y.max The maximum value of the Y-axis. The default value is the maximum net benefit.
#'@param pyh The height at which the bars are plotted cannot exceed y.min.
#'@param relcol The colour of the relevant part of the bar. The default is red.
#'@param irrelcol The colour of the irrelevant part of the bar. The default is blue.
#'@param relabel Relevance Tags.
#'@param irrellabel No relevant tags.
#'@param text.size Font size.
#'@param text.col The colour of the font.
#'@param colbar The default is true, and if false is selected, bar plotting is cancelled.
#'@param merge If true is selected it will merge the two long zones.
#'@param threshold.text The default is FALSE, if TRUE is selected, a text message for the threshold will be added.
#'@param threshold.line The default is FALSE, and if TRUE is selected, lines for the threshold will be added.
#'@param nudge_x Used to adjust the x-axis position of the point where the threshold is located.
#'@param nudge_y Used to adjust the y-axis position of the point where the threshold is located.
#'@param threshold.linetype The line shape of the threshold line.
#'@param threshold.linewidth The line width of the threshold line.
#'@param threshold.linecol The colour of the threshold line.
#'@param po.text.size The size of the threshold point text.
#'@param po.text.col The colour of the threshold point text.
#'@param po.text.fill The background of the threshold point text.
#'@param liftpec Threshold point left displacement.
#'@param rightpec Threshold point right displacement.
#'@param legend.position Set the position of the legend.
#'@export

netdata.crr<-function(fit,newdata=NULL,timepoint='median',cmprsk=FALSE,modelnames=NULL,merge=FALSE,y.min=NULL,xstop=NULL,y.max=NULL,
                     pyh=NULL,relcol="#c01e35",irrelcol="#0151a2",relabel="Nomogram relevant",
                     irrellabel="Nomogram irrelevant",text.size=4.5,text.col="green",colbar=TRUE,
                     threshold.text=FALSE,threshold.line=FALSE,nudge_x = 0,nudge_y = 0,
                     threshold.linetype=2,threshold.linewidth = 1.2,threshold.linecol="black",
                     po.text.size=4,po.text.col="black",po.text.fill="white",liftpec=NULL,rightpec=NULL,
                     legend.position = c(0.85,0.75)) {
  fit1<-fit[["mod"]];cencode<-fit[[2]];failcode<-fit[[3]]
  if (missing(fit1)) stop("fit is missing .")
  if (is.null(modelnames)) {modelnames<-"model"
  } else {
    modelnames<-modelnames
  }
  all.var<-all.vars(fit1$terms)
  modely<-model.y(fit1)
  modelx<-model.x(fit1)
  data<-modeldata(fit1,crrmol=TRUE)
  if (timepoint=='median') {
    timepo1<-median(data[,modely[1]])
  } else {timepo1<-timepoint}
  if (!is.null(newdata)) {
    newdata<-newdata
  }
  if (!is.null(newdata)) {
    newdata$prob1 <- c(1-(summary(survfit(fit1, newdata=newdata), times=timepo1)$surv))
  } else {
    data$prob1 = c(1- (summary(survfit(fit1, newdata=data), times=timepo1)$surv))
  }
  if (!is.null(newdata)) {
    net<-cmprskstdca(data=newdata, outcome=modely[2], ttoutcome=modely[1], timepoint=timepo1, predictors="prob1", probability=FALSE,
                     graph=F,cmprsk=TRUE)
  } else {
    net<-cmprskstdca(data=data, outcome=modely[2], ttoutcome=modely[1], timepoint=timepo1, predictors="prob1", probability=FALSE,
                     graph=F,cmprsk=TRUE)
  }
  net
}

#'@title netdata.glm
#'@name  netdata.glm
#'@param fit Fill in the model you want to analyze. Support survival analysis and logistic regression.
#'@param newdata If the decision curve of the validation set is to be analysed. Fill in the validation set data here.
#'@param timepoint If it is a survival analysis, fill in the point in time you need to study. The default is the median time.
#'@param cmprsk If it is a competitive risk model, select TRUE here.
#'@param modelnames Defines the name of the generated image model.
#'@param y.min The maximum value of the negative part of the picture. Generally defaults to positive values multiplied by 0.4.
#'@param xstop The maximum value of the X-axis of the picture.
#'@param y.max The maximum value of the Y-axis. The default value is the maximum net benefit.
#'@param pyh The height at which the bars are plotted cannot exceed y.min.
#'@param relcol The colour of the relevant part of the bar. The default is red.
#'@param irrelcol The colour of the irrelevant part of the bar. The default is blue.
#'@param relabel Relevance Tags.
#'@param irrellabel No relevant tags.
#'@param text.size Font size.
#'@param text.col The colour of the font.
#'@param colbar The default is true, and if false is selected, bar plotting is cancelled.
#'@param merge If true is selected it will merge the two long zones.
#'@param threshold.text The default is FALSE, if TRUE is selected, a text message for the threshold will be added.
#'@param threshold.line The default is FALSE, and if TRUE is selected, lines for the threshold will be added.
#'@param nudge_x Used to adjust the x-axis position of the point where the threshold is located.
#'@param nudge_y Used to adjust the y-axis position of the point where the threshold is located.
#'@param threshold.linetype The line shape of the threshold line.
#'@param threshold.linewidth The line width of the threshold line.
#'@param threshold.linecol The colour of the threshold line.
#'@param po.text.size The size of the threshold point text.
#'@param po.text.col The colour of the threshold point text.
#'@param po.text.fill The background of the threshold point text.
#'@param liftpec Threshold point left displacement.
#'@param rightpec Threshold point right displacement.
#'@param legend.position Set the position of the legend.
#'@export


netdata.glm<-function(fit,newdata=NULL,timepoint='median',cmprsk=FALSE,modelnames=NULL,merge=FALSE,y.min=NULL,xstop=NULL,y.max=NULL,
                     pyh=NULL,relcol="#c01e35",irrelcol="#0151a2",relabel="Nomogram relevant",
                     irrellabel="Nomogram irrelevant",text.size=4.5,text.col="green",colbar=TRUE,
                     threshold.text=FALSE,threshold.line=FALSE,nudge_x = 0,nudge_y = 0,
                     threshold.linetype=2,threshold.linewidth = 1.2,threshold.linecol="black",
                     po.text.size=4,po.text.col="black",po.text.fill="white",liftpec=NULL,rightpec=NULL,
                     legend.position = c(0.85,0.75)) {
  if (missing(fit)) stop("fit is missing .")
  fit<-fit;
  if (is.null(modelnames)) {modelnames<-"model"
  } else {
    modelnames<-modelnames
  }
  all.var<-all.vars(fit$terms)
  modely<-model.y(fit)
  modelx<-model.x(fit)
  data<-fit[["data"]]
  if (!is.null(newdata)) {
    newdata<-newdata
  }
  if (!is.null(newdata)) {
    newdata$prob1 <- predict(fit,newdata=newdata,type="response")
  } else {
    data$prob1<-predict(fit,newdata=data,type="response")
  }
  if (!is.null(newdata)) {
    if (is.factor(newdata[,modely[1]])) {
      newdata[,modely[1]]<-as.numeric(newdata[,modely[1]])-1
    }
    net<-dca(data = newdata, outcome = modely[1], predictors = c("prob1"),xstart = 0,
             xstop = 1,graph=F)
  } else {
    net<-dca(data = data, outcome = modely[1], predictors = c("prob1"),xstart = 0,
             xstop = 1,graph=F)
  }
  ########
  net
}


#'@title netdata.randomFores
#'@name  netdata.randomFores
#'@param fit Fill in the model you want to analyze. Support survival analysis and logistic regression.
#'@param newdata If the decision curve of the validation set is to be analysed. Fill in the validation set data here.
#'@param timepoint If it is a survival analysis, fill in the point in time you need to study. The default is the median time.
#'@param cmprsk If it is a competitive risk model, select TRUE here.
#'@param modelnames Defines the name of the generated image model.
#'@param y.min The maximum value of the negative part of the picture. Generally defaults to positive values multiplied by 0.4.
#'@param xstop The maximum value of the X-axis of the picture.
#'@param y.max The maximum value of the Y-axis. The default value is the maximum net benefit.
#'@param pyh The height at which the bars are plotted cannot exceed y.min.
#'@param relcol The colour of the relevant part of the bar. The default is red.
#'@param irrelcol The colour of the irrelevant part of the bar. The default is blue.
#'@param relabel Relevance Tags.
#'@param irrellabel No relevant tags.
#'@param text.size Font size.
#'@param text.col The colour of the font.
#'@param colbar The default is true, and if false is selected, bar plotting is cancelled.
#'@param merge If true is selected it will merge the two long zones.
#'@param threshold.text The default is FALSE, if TRUE is selected, a text message for the threshold will be added.
#'@param threshold.line The default is FALSE, and if TRUE is selected, lines for the threshold will be added.
#'@param nudge_x Used to adjust the x-axis position of the point where the threshold is located.
#'@param nudge_y Used to adjust the y-axis position of the point where the threshold is located.
#'@param threshold.linetype The line shape of the threshold line.
#'@param threshold.linewidth The line width of the threshold line.
#'@param threshold.linecol The colour of the threshold line.
#'@param po.text.size The size of the threshold point text.
#'@param po.text.col The colour of the threshold point text.
#'@param po.text.fill The background of the threshold point text.
#'@param liftpec Threshold point left displacement.
#'@param rightpec Threshold point right displacement.
#'@param legend.position Set the position of the legend.
#'@export

netdata.randomForest<-function(fit,newdata=NULL,timepoint='median',cmprsk=FALSE,modelnames=NULL,merge=FALSE,y.min=NULL,xstop=NULL,y.max=NULL,
                              pyh=NULL,relcol="#c01e35",irrelcol="#0151a2",relabel="Nomogram relevant",
                              irrellabel="Nomogram irrelevant",text.size=4.5,text.col="green",colbar=TRUE,
                              threshold.text=FALSE,threshold.line=FALSE,nudge_x = 0,nudge_y = 0,
                              threshold.linetype=2,threshold.linewidth = 1.2,threshold.linecol="black",
                              po.text.size=4,po.text.col="black",po.text.fill="white",liftpec=NULL,rightpec=NULL,
                              legend.position = c(0.85,0.75)) {
  if (missing(fit)) stop("fit is missing .")
  fit<-fit;
  if (is.null(modelnames)) {modelnames<-"model"
  } else {
    modelnames<-modelnames
  }
  all.var<-all.vars(fit$terms)
  modely<-model.y(fit)
  modelx<-model.x(fit)
  if (is.null(newdata)) {stop("The newdata parameter cannot be a null value in a random forest model.")}
  newdata<-newdata
  def_pred<-predict(fit, newdata=newdata,type = "prob")
  def_pred<-as.data.frame(def_pred)
  newdata$prob1<-def_pred[,2]
  if (is.character(newdata[,modely])) {newdata[,modely]<-factor(newdata[,modely])}
  if (is.factor(newdata[,modely])) {newdata[,modely]<-as.numeric(newdata[,modely])-1 }
  net<-dca(data = newdata, outcome = modely[1], predictors = c("prob1"),xstart = 0,
           xstop = 1,graph=F)
  ########
  net
}

#'@title netdata.svm
#'@name  netdata.svm
#'@param fit Fill in the model you want to analyze. Support survival analysis and logistic regression.
#'@param newdata If the decision curve of the validation set is to be analysed. Fill in the validation set data here.
#'@param timepoint If it is a survival analysis, fill in the point in time you need to study. The default is the median time.
#'@param cmprsk If it is a competitive risk model, select TRUE here.
#'@param modelnames Defines the name of the generated image model.
#'@param y.min The maximum value of the negative part of the picture. Generally defaults to positive values multiplied by 0.4.
#'@param xstop The maximum value of the X-axis of the picture.
#'@param y.max The maximum value of the Y-axis. The default value is the maximum net benefit.
#'@param pyh The height at which the bars are plotted cannot exceed y.min.
#'@param relcol The colour of the relevant part of the bar. The default is red.
#'@param irrelcol The colour of the irrelevant part of the bar. The default is blue.
#'@param relabel Relevance Tags.
#'@param irrellabel No relevant tags.
#'@param text.size Font size.
#'@param text.col The colour of the font.
#'@param colbar The default is true, and if false is selected, bar plotting is cancelled.
#'@param merge If true is selected it will merge the two long zones.
#'@param threshold.text The default is FALSE, if TRUE is selected, a text message for the threshold will be added.
#'@param threshold.line The default is FALSE, and if TRUE is selected, lines for the threshold will be added.
#'@param nudge_x Used to adjust the x-axis position of the point where the threshold is located.
#'@param nudge_y Used to adjust the y-axis position of the point where the threshold is located.
#'@param threshold.linetype The line shape of the threshold line.
#'@param threshold.linewidth The line width of the threshold line.
#'@param threshold.linecol The colour of the threshold line.
#'@param po.text.size The size of the threshold point text.
#'@param po.text.col The colour of the threshold point text.
#'@param po.text.fill The background of the threshold point text.
#'@param liftpec Threshold point left displacement.
#'@param rightpec Threshold point right displacement.
#'@param legend.position Set the position of the legend.
#'@export


netdata.svm<-function(fit,newdata=NULL,timepoint='median',cmprsk=FALSE,modelnames=NULL,merge=FALSE,y.min=NULL,xstop=NULL,y.max=NULL,
                     pyh=NULL,relcol="#c01e35",irrelcol="#0151a2",relabel="Nomogram relevant",
                     irrellabel="Nomogram irrelevant",text.size=4.5,text.col="green",colbar=TRUE,
                     threshold.text=FALSE,threshold.line=FALSE,nudge_x = 0,nudge_y = 0,
                     threshold.linetype=2,threshold.linewidth = 1.2,threshold.linecol="black",
                     po.text.size=4,po.text.col="black",po.text.fill="white",liftpec=NULL,rightpec=NULL,
                     legend.position = c(0.85,0.75)) {
  if (missing(fit)) stop("fit is missing .")
  fit<-fit;
  if (is.null(modelnames)) {modelnames<-"model"
  } else {
    modelnames<-modelnames
  }
  all.var<-all.vars(fit$terms)
  modely<-model.y(fit)
  modelx<-model.x(fit)
  if (is.null(newdata)) {stop("In the support vector machine model, the newdata parameter cannot be a null value.")}
  newdata<-newdata
  newdata$def_pred<-predict(fit, newdata=newdata,probability = TRUE)
  Pred_Prob <- attr(newdata$def_pred, "probabilities")
  def_pred<-as.data.frame(Pred_Prob)
  newdata$prob1<-def_pred[,2]
  if (is.character(newdata[,modely])) {newdata[,modely]<-factor(newdata[,modely])}
  if (is.factor(newdata[,modely])) {newdata[,modely]<-as.numeric(newdata[,modely])-1 }
  net<-dca(data = newdata, outcome = modely[1], predictors = c("prob1"),xstart = 0,
           xstop = 1,graph=F)
  ########
  net
}

#'@title netdata.scixgboot
#'@name  netdata.scixgboot
#'
#'@param fit Fill in the model you want to analyze. Support survival analysis and logistic regression.
#'@param newdata If the decision curve of the validation set is to be analysed. Fill in the validation set data here.
#'@param timepoint If it is a survival analysis, fill in the point in time you need to study. The default is the median time.
#'@param cmprsk If it is a competitive risk model, select TRUE here.
#'@param modelnames Defines the name of the generated image model.
#'@param y.min The maximum value of the negative part of the picture. Generally defaults to positive values multiplied by 0.4.
#'@param xstop The maximum value of the X-axis of the picture.
#'@param y.max The maximum value of the Y-axis. The default value is the maximum net benefit.
#'@param pyh The height at which the bars are plotted cannot exceed y.min.
#'@param relcol The colour of the relevant part of the bar. The default is red.
#'@param irrelcol The colour of the irrelevant part of the bar. The default is blue.
#'@param relabel Relevance Tags.
#'@param irrellabel No relevant tags.
#'@param text.size Font size.
#'@param text.col The colour of the font.
#'@param colbar The default is true, and if false is selected, bar plotting is cancelled.
#'@param merge If true is selected it will merge the two long zones.
#'@param threshold.text The default is FALSE, if TRUE is selected, a text message for the threshold will be added.
#'@param threshold.line The default is FALSE, and if TRUE is selected, lines for the threshold will be added.
#'@param nudge_x Used to adjust the x-axis position of the point where the threshold is located.
#'@param nudge_y Used to adjust the y-axis position of the point where the threshold is located.
#'@param threshold.linetype The line shape of the threshold line.
#'@param threshold.linewidth The line width of the threshold line.
#'@param threshold.linecol The colour of the threshold line.
#'@param po.text.size The size of the threshold point text.
#'@param po.text.col The colour of the threshold point text.
#'@param po.text.fill The background of the threshold point text.
#'@param liftpec Threshold point left displacement.
#'@param rightpec Threshold point right displacement.
#'@param legend.position Set the position of the legend.
#'@export


netdata.scixgboot<-function(fit,newdata=NULL,timepoint='median',cmprsk=FALSE,modelnames=NULL,merge=FALSE,y.min=NULL,xstop=NULL,y.max=NULL,
                      pyh=NULL,relcol="#c01e35",irrelcol="#0151a2",relabel="Nomogram relevant",
                      irrellabel="Nomogram irrelevant",text.size=4.5,text.col="green",colbar=TRUE,
                      threshold.text=FALSE,threshold.line=FALSE,nudge_x = 0,nudge_y = 0,
                      threshold.linetype=2,threshold.linewidth = 1.2,threshold.linecol="black",
                      po.text.size=4,po.text.col="black",po.text.fill="white",liftpec=NULL,rightpec=NULL,
                      legend.position = c(0.85,0.75)) {
  if (missing(fit)) stop("fit is missing .")
  fit<-fit;
  if (is.null(modelnames)) {modelnames<-"model"
  } else {
    modelnames<-modelnames
  }
  yvname<-modely<-model.y(fit)
  modelx<-model.x(fit)
  call1<-fit[["call1"]]
  fitxgboot<-fit[["fit"]]
  if (!is.null(newdata)) {
    newdata<-newdata
  } else {newdata<-fit[["data"]]}
  ###
  x = model.matrix(as.formula(call1),data=newdata)[,-1]
  if (is.factor(newdata[,yvname])) {
    ylabel <-as.numeric(as.character(newdata[,yvname]))
  } else {ylabel<-as.numeric(newdata[,yvname])}
  data_DMatrix<-xgb.DMatrix(x , label =ylabel)
  newdata$prob1 = predict(fitxgboot,newdata = data_DMatrix)
  ####
  if (is.factor(newdata[,modely[1]])) {
    newdata[,modely[1]]<-as.numeric(as.character(newdata[,modely[1]]))
  }
  net<-dca(data = newdata, outcome = modely[1], predictors = c("prob1"),xstart = 0,
           xstop = 1,graph=F)
  ########
  net
}
