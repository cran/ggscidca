#'@title  scidca.glm
#'@name  scidca.glm
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
#'@importFrom "stats" "predict"
#'
#'
#'
#'@export
#'
#'@return A picture.
#'






scidca.glm<-function(fit,newdata=NULL,timepoint='median',cmprsk=FALSE,modelnames=NULL,merge=FALSE,y.min=NULL,xstop=NULL,y.max=NULL,
                     pyh=NULL,relcol="#c01e35",irrelcol="#0151a2",relabel="Nomogram relevant",
                     irrellabel="Nomogram irrelevant",text.size=4.5,text.col="green",colbar=TRUE) {
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
    newdata$prob1 <- stats::predict(fit,newdata=newdata,type="response")
  } else {
    data$prob1<-stats::predict(fit,newdata=data,type="response")
  }
  if (!is.null(newdata)) {
    net<-dca(data = newdata, outcome = modely[1], predictors = c("prob1"),xstart = 0,
             xstop = 1,graph=F)
  } else {
    net<-dca(data = data, outcome = modely[1], predictors = c("prob1"),xstart = 0,
             xstop = 1,graph=F)
  }
  ########
  p<-getplot(net,pyh,relcol=relcol,irrelcol=irrelcol,relabel=relabel,merge=merge,modelnames=modelnames,y.min=y.min,xstop=xstop,y.max=y.max,
             irrellabel=irrellabel,text.size=text.size,text.col=text.col,colbar=colbar)
  p
}
