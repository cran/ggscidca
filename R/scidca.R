#'@title  scidca
#'@name  scidca
#'@description  You can use it to generate a decision curve with coloured bars.
#'
#'@details  Table 1 represents the relationship between the baseline values of the data.
#'          This function can be easily done.Creates 'Table 1', i.e., description of baseline patient
#'          characteristics, which is essential in every medical research.
#'          Supports both continuous and categorical variables, as well as
#'          p-values and standardized mean differences.
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
#'
#'
#'@import "ggplot2"
#'@import "reshape2"
#'@import "survival"
#'
#'@return A picture.
#'
#'@export
#'@examples
#'library(survival)
#'library(reshape2)
#'library(ggplot2)
#'##Import the internal data of the R package
#'bc<-Breastcancer
#'##Categorical variables converted to factors
#'bc$histgrad<-as.factor(bc$histgrad)
#'bc$er<-as.factor(bc$er)
#'bc$pr<-as.factor(bc$pr)
#'bc$ln_yesno<-as.factor(bc$ln_yesno)
#'##Generate Survival Analysis Model
#'f1<-coxph(Surv(time,status)~er+histgrad+pr+age+ln_yesno,bc)
#'##Draw decision curve
#'scidca(f1)
#'scidca(f1,threshold.line = TRUE,threshold.text = TRUE)
#'##logistic regression model
#'fit<-glm(status~er+histgrad+pr+age+ln_yesno,family = binomial(link = "logit"),data=bc)
#'##Draw decision curve
#'scidca(f1)
#'scidca(f1,threshold.line = TRUE,threshold.text = TRUE)
#'##random forest model
#'library(randomForest)
#'LIRI<-LIRI
#'set.seed(1)
#'index <- sample(2,nrow(LIRI),replace = TRUE,prob=c(0.7,0.3))
#'traindata <- LIRI[index==1,]
#'testdata <- LIRI[index==2,]
#'traindata$status<-as.factor(traindata$status)
#'#Modelling random forests
#'fit<-randomForest(status ~ANLN+CENPA+GPR182+BCO2 ,data=traindata,ntree=500,
#'important=TRUE,proximity=TRUE)
#'scidca(fit,newdata = traindata)
#'scidca(fit,newdata = testdata )
#'scidca(fit,newdata = testdata ,threshold.line = TRUE,threshold.text = TRUE)




scidca <- function(fit,newdata=NULL,timepoint='median',cmprsk=FALSE,modelnames=NULL,merge=FALSE,y.min=NULL,xstop=NULL,y.max=NULL,
                   pyh=NULL,relcol="#c01e35",irrelcol="#0151a2",relabel="Nomogram relevant",
                   irrellabel="Nomogram irrelevant",text.size=4.5,text.col="green",colbar=TRUE,
                   threshold.text=FALSE,threshold.line=FALSE,nudge_x = 0,nudge_y = 0,
                   threshold.linetype=2,threshold.linewidth = 1.2,threshold.linecol="black",
                   po.text.size=4,po.text.col="black",po.text.fill="white",liftpec=NULL,rightpec=NULL,
                   legend.position = c(0.85,0.75)) {
  UseMethod('scidca')
}







