#'@title  cox.tcdca
#'@name  cox.tcdca
#'@description  You can use it to plot decision curves for multiple generative analysis or competitive risk models.
#'
#'
#'@param ... Fill in multiple survival analysis or competitive risk models. You cannot mix and match.
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
#'@param Splitface Name the faceted image.
#'
#'
#'
#'@import "ggplot2"
#'@import "reshape2"
#'@import "survival"
#'
#'@return A picture.
#'
#'@export

utils::globalVariables(c(
  'get_names'
))



cox.tcdca<-function(...,newdata=NULL,timepoint='median',cmprsk=FALSE,modelnames=NULL,merge=FALSE,y.min=NULL,xstop=NULL,y.max=NULL,
                    pyh=NULL,relcol="#c01e35",irrelcol="#0151a2",relabel="Nomogram relevant",
                    irrellabel="Nomogram irrelevant",text.size=4.5,text.col="green",colbar=TRUE,
                    threshold.text=FALSE,threshold.line=FALSE,nudge_x = 0,nudge_y = 0,
                    threshold.linetype=2,threshold.linewidth = 1.2,threshold.linecol="black",
                    po.text.size=4,po.text.col="black",po.text.fill="white",liftpec=NULL,rightpec=NULL,
                    legend.position = c(0.85,0.75),Splitface=NULL) {
  if (is.null(newdata)) {stop("Newdata cannot be missing.")}
  if (!is.list(newdata)) {stop("Newdata must be a list.")}
  fit.list<-list(...)
  fn<-length(fit.list);dn<-length(newdata)
  if (fn != dn) {stop("The number of models must be equal to the number of data.")}
  if (is.null(modelnames)) {
    modelnames=get_names(...)
  } else {modelnames<-modelnames}
  mn<-length(modelnames)
  if (fn != mn) {stop("The number of models and the number of model names must be equal.")}
  if (length(timepoint)==1) {
    nt<-list()
    for (i in 1:fn) {
      fit<-fit.list[[i]];dat<-as.data.frame(newdata[[i]])
      net<-netdata(fit,newdata = dat,timepoint=timepoint)
      nbdat<-net$net.benefit
      names(nbdat)<-c("threshold","all","none","net.benefit")
      nt[[i]]<-nbdat
    }
    dt1<-nt[[1]]
    name0<-names(dt1);name.b<-name0[1:3];name1<-name0
    for (i in 2:fn) {
      net.benefit.name<-paste0("net.benefit",i)
      name1<-c(name1,net.benefit.name)
      dt1<-cbind(dt1,net.benefit.name=nt[[i]]$net.benefit)
      names(dt1)<-name1
    }
    name2<-setdiff(names(dt1), name.b)
    name3<-c(name.b,modelnames)
    names(dt1)<-name3
    plotdat<- melt(dt1,id="threshold",measure=c(modelnames,"all","none"))
    x.max<-max(nbdat$threshold,na.rm = T)
    if (!is.null(xstop)) {
      x.max<-xstop
    }
    y.max<-max(plotdat$value,na.rm = T)
    if (is.null(y.max)) {
      y.max<-max(plotdat$value,na.rm = T)
    } else {y.max<-y.max}
    if (is.null(y.min)) {
      y.min<-y.max*0.4
    } else {y.min<-y.min}
    p<-ggplot2::ggplot(plotdat)+
      geom_line(aes(x=threshold,y=value,color=variable),linewidth=1.2)+
      coord_cartesian(xlim=c(0,x.max), ylim=c(-y.min,y.max))+
      labs(x="Threshold probability (%)")+labs(y="Net benefit")+
      scale_color_discrete(name="Model",labels=c(modelnames,"all","none"))+
      theme_bw(base_size = 14)+
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            legend.title=element_blank(),
            legend.position= legend.position
      )
  }
  if (length(timepoint)>1) {
    if (fn != length(timepoint)) {stop("The number of models must be equal to the number of times.")}
    nt<-list()
    for (i in 1:fn) {
      fit<-fit.list[[i]];dat<-as.data.frame(newdata[[i]])
      net<-netdata(fit,newdata = dat,timepoint=timepoint[i])
      nbdat<-net$net.benefit
      names(nbdat)<-c("threshold","all","none","net.benefit")
      nt[[i]]<-nbdat
    }
    plotdat<-NULL
    if (is.null(Splitface)) {
      Splitface<-timepoint
    } else {Splitface<-Splitface}
    for (i in 1:fn) {
      dat<-nt[[i]]
      dat<-melt(dat,id="threshold",measure=c("net.benefit","all","none"))
      dat$type<-rep(Splitface[i],nrow(dat))
      plotdat<-rbind(plotdat,dat)
    }
    x.max<-max(nbdat$threshold,na.rm = T)
    if (!is.null(xstop)) {
      x.max<-xstop
    }
    y.max<-max(plotdat$value,na.rm = T)
    if (is.null(y.max)) {
      y.max<-max(plotdat$value,na.rm = T)
    } else {y.max<-y.max}
    if (is.null(y.min)) {
      y.min<-y.max*0.4
    } else {y.min<-y.min}
    p<-ggplot2::ggplot(plotdat)+
      geom_line(aes(x=threshold,y=value,color=variable),linewidth=1.2)+
      facet_wrap(~type)+
      coord_cartesian(xlim=c(0,0.99), ylim=c(-y.min,y.max))+
      labs(x="Threshold probability (%)")+labs(y="Net benefit")+
      theme_bw(base_size = 14)+
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            legend.title=element_blank(),
            strip.background = element_rect(fill = "white")#分面背景
      )
  }
  p
}
