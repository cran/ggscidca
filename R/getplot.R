utils::globalVariables(c('complete.cases',
                         'melt',
                         'ggplot',
                         'geom_line',
                         'aes',
                         'threshold',
                         'value',
                         'variable',
                         'coord_cartesian',
                         'labs',
                         'scale_color_discrete',
                         'theme_bw',
                         'theme',
                         'element_blank',
                         'geom_rect',
                         'xmin',
                         'xmax',
                         'ymin',
                         'ymax',
                         'annotate'
))



getplot<-function(data,pyh=NULL,relcol="#c01e35",irrelcol="#0151a2",relabel="Nomogram relevant",y.min=NULL,xstop=NULL,y.max=NULL,
                  irrellabel="Nomogram irrelevant",text.size=4.5,text.col="green",modelnames=NULL,
                  colbar=TRUE,merge=FALSE) {
  dt<-data
  nbdat<-dt$net.benefit
  names(nbdat)<-c("threshold","all","none","net.benefit")
  nbdat$id<-c(1:nrow(nbdat))
  ####
  l1<-nbdat$net.benefit-nbdat$all>0
  which(l1 == T)
  l11<-which(l1 == TRUE)[1]
  if (any(nbdat$net.benefit<0,na.rm = T)==TRUE) {
    l2<-nbdat$net.benefit<0
    l21<-which(l2 == TRUE)[1]-1
  } else if (any(nbdat$net.benefit==0,na.rm = T)==TRUE) {
    l21<-which(nbdat$net.benefit==0)[1]
    l22<-length(nbdat$net.benefit)
  } else if (!any(nbdat$net.benefit<=0,na.rm = T)==TRUE) {
    l21<-length(nbdat$net.benefit[complete.cases(nbdat$net.benefit)])
    l22<-length(nbdat$net.benefit)
  }
  ###########
  t1<-nbdat$threshold[l11]
  t2<-nbdat$threshold[l21]
  if (merge==TRUE) {
    t2<-nbdat$threshold[l22]
  }
  ##
  plotdat <- melt(nbdat,id="threshold",measure=c("net.benefit","all","none"))
  x.max<-max(nbdat$threshold,na.rm = T)
  if (!is.null(xstop)) {
    x.max<-xstop
  }
  if (is.null(y.max)) {
    y.max<-max(plotdat$value,na.rm = T)
  } else {y.max<-y.max}
  if (is.null(y.min)) {
    y.min<-y.max*0.4
  } else {y.min<-y.min}
  p.max<-y.min
  not1<-paste(round(p.max,4),"is the range of drawing height for a square chart.",collapse = "")
  not2<-paste("The current height of the square bar is",round(p.max/2,4),collapse = "")
  message(not1);message(not2);
  if (is.null(pyh)) {
    pyh<-p.max/2} else {
      pyh<-pyh
    }
  if (pyh>p.max) stop("Outside the drawing area.")
  x1.text<-(t2-t1)/2+t1
  y1.text<-(p.max-pyh)/2+pyh
  x2.text<-(x.max-t2)/2+t2
  y2.text<-(p.max-pyh)/2+pyh
  p3<-ggplot(plotdat)+
    geom_line(aes(x=threshold,y=value,color=variable),linewidth=1.2)+
    coord_cartesian(xlim=c(0,x.max), ylim=c(-y.min,y.max))+
    labs(x="Threshold probability (%)")+labs(y="Net benefit")+
    scale_color_discrete(name="Model",labels=c(modelnames,"all","none"))+
    theme_bw(base_size = 14)+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          legend.title=element_blank(),
          legend.position = c(0.85,0.75)
    )
  ###############
  rect_df1 <- data.frame(xmin = 0,
                         xmax = t1,
                         ymin = -p.max,
                         ymax = -pyh
  )
  rect_df2 <- data.frame(xmin = t1,
                         xmax = t2,
                         ymin = -p.max,
                         ymax = -pyh
  )
  rect_df3 <- data.frame(xmin = t2,
                         xmax = x.max,
                         ymin = -p.max,
                         ymax = -pyh
  )
  p4 <- p3+
    geom_rect(data = rect_df1,
              mapping = aes(xmin = xmin, xmax=xmax,ymin=ymin,ymax=ymax),
              fill = irrelcol)+
    geom_rect(data = rect_df2,
              mapping = aes(xmin = xmin, xmax=xmax,ymin=ymin,ymax=ymax),
              fill = relcol)+
    annotate(geom = "text", label = relabel,
             x = x1.text, y = -y1.text,
             size = text.size, color = text.col)
  if (merge==FALSE) {
    p4<-p4+geom_rect(data = rect_df3,
                     mapping = aes(xmin = xmin, xmax=xmax,ymin=ymin,ymax=ymax),
                     fill = irrelcol)+annotate(geom = "text", label = irrellabel,
                                               x = x2.text, y = -y2.text,
                                               size = text.size, color = text.col)
  }
  if (colbar==TRUE) {
    p<-p4} else {p<-p3}
  p
}


