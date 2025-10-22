#'@title  scidca.scisvycoxph
#'@name  scidca.scisvycoxph
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
#'@param lincol Defines the drawing line color.
#'@importFrom "stats" "median"
#'
#'
#'@export
#'
#'@return A picture.
#'





scidca.scisvycoxph<-function(fit,newdata=NULL,timepoint='median',cmprsk=FALSE,modelnames=NULL,merge=FALSE,y.min=NULL,xstop=NULL,y.max=NULL,
                       pyh=NULL,relcol="#c01e35",irrelcol="#0151a2",relabel="Nomogram relevant",
                       irrellabel="Nomogram irrelevant",text.size=4.5,text.col="green",colbar=TRUE,
                       threshold.text=FALSE,threshold.line=FALSE,nudge_x = 0,nudge_y = 0,
                       threshold.linetype=2,threshold.linewidth = 1.2,threshold.linecol="black",
                       po.text.size=4,po.text.col="black",po.text.fill="white",liftpec=NULL,rightpec=NULL,
                       legend.position = c(0.85,0.75),lincol=c("#377EB8", "#4DAF4A", "#FF7F00")) {
  if (missing(fit)) stop("fit is missing .")
  fit<-fit;
  if (is.null(modelnames)) {modelnames<-"model"
  } else {
    modelnames<-modelnames
  }
  all.var<-all.vars(fit$terms)
  modely<-fit[["yv"]]
  timevar<-fit[["timevar"]]
  data<-fit[["data"]]
  if (timepoint=='median') {
    timepo1<-stats::median(data[,modely[1]])
  } else {timepo1<-timepoint}
  if (!is.null(newdata)) {
    newdata<-newdata
  }
  data$prob1<-fit[["prob1"]]
  if (!is.null(newdata)) {
    if (is.factor(newdata[,modely[2]])) {
      newdata[,modely[2]]<-as.numeric(as.character(newdata[,modely[2]]))
    }
    net<-stdca(data=newdata, outcome=modely[2], ttoutcome=modely[1], timepoint=timepo1, predictors="prob1", probability=FALSE,
               graph=F)
  } else {
    if (is.factor(data[,modely[2]])) {
      data[,modely[2]]<-as.numeric(as.character(data[,modely[2]]))
    }
    net<-stdca(data=data, outcome=modely[2], ttoutcome=modely[1], timepoint=timepo1, predictors="prob1", probability=FALSE,
               graph=F)
  }
  ########
  p<-getplot(net,pyh=pyh,relcol=relcol,irrelcol=irrelcol,relabel=relabel,modelnames=modelnames,merge=merge,y.min=y.min,xstop=xstop,y.max=y.max,
             irrellabel=irrellabel,text.size=text.size,text.col=text.col,colbar=colbar,
             threshold.text=threshold.text,threshold.line=threshold.line,nudge_x = nudge_x,nudge_y = nudge_y,
             threshold.linetype=threshold.linetype,threshold.linewidth = threshold.linewidth,threshold.linecol=threshold.linecol,
             po.text.size=po.text.size,po.text.col=po.text.col,po.text.fill=po.text.fill,liftpec=liftpec,rightpec=rightpec,
             legend.position,lincol=lincol)
  p
}
