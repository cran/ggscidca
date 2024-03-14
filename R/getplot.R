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
                         'annotate',
                         'x',
                         'y',
                         'label'
))



getplot<-function(data,pyh=NULL,relcol="#c01e35",irrelcol="#0151a2",relabel="Nomogram relevant",y.min=NULL,xstop=NULL,y.max=NULL,
                  irrellabel="Nomogram irrelevant",text.size=4.5,text.col="green",modelnames=NULL,
                  colbar=TRUE,merge=FALSE,threshold.text=FALSE,threshold.line=FALSE,nudge_x = 0,nudge_y = 0,
                  threshold.linetype=2,threshold.linewidth = 1.2,threshold.linecol="black",
                  po.text.size=4,po.text.col="black",po.text.fill="white",liftpec=NULL,rightpec=NULL) {
  dt<-data
  nbdat<-dt$net.benefit
  names(nbdat)<-c("threshold","all","none","net.benefit")
  nbdat$id<-c(1:nrow(nbdat))
  ####
  l1<-nbdat$net.benefit-nbdat$all>0
  which(l1 == TRUE)
  if (!is.null(liftpec)) {l11<-which(l1 == TRUE)[1]+liftpec
  } else {
    l11<-which(l1 == TRUE)[1]
  }
  if (any(nbdat$net.benefit<0,na.rm = TRUE)==TRUE) {
    l2<-nbdat$net.benefit<0
    l21<-which(l2 == TRUE)[1]-1
  } else if (any(nbdat$net.benefit==0,na.rm = TRUE)==TRUE) {
    l21<-which(nbdat$net.benefit==0)[1]
    l22<-length(nbdat$net.benefit)
  } else if (!any(nbdat$net.benefit<=0,na.rm = TRUE)==TRUE) {
    l21<-length(nbdat$net.benefit[complete.cases(nbdat$net.benefit)])
    l22<-length(nbdat$net.benefit)
  }
  if (!is.null(rightpec)) {l21<-which(l2 == TRUE)[1]-1+rightpec}
  ###########
  t1<-nbdat$threshold[l11]
  t2<-nbdat$threshold[l21]
  if (merge==TRUE) {
    t2<-nbdat$threshold[l22]
  }
  y.benefit<-nbdat$net.benefit[l11]
  text.t1<-round(t1*100,2)
  text.t1<-paste0(text.t1,"%")
  text.t2<-round(t2*100,2)
  text.t2<-paste0(text.t2,"%")
  ##
  plotdat <- melt(nbdat,id="threshold",measure=c("net.benefit","all","none"))
  x.max<-max(nbdat$threshold,na.rm = TRUE)
  if (!is.null(xstop)) {
    x.max<-xstop
  }
  if (is.null(y.max)) {
    y.max<-max(plotdat$value,na.rm = TRUE)
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
  line_df <- data.frame(x = c(t1,t2),
                        ymin = c(-pyh,-pyh),
                        ymax = c(y.benefit,0)
  )
  line_df1 <- line_df[1,]
  point_df<-data.frame(x=c(t1,t2),
                       y=c(y.benefit,0),
                       label=c(text.t1,text.t2))
  point_df1<-point_df[1,]
  ###
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
  if (threshold.line==TRUE) {
    if (merge==TRUE) {p4<-p4+geom_linerange(data = line_df1, mapping = aes(x=x,ymin=ymin,ymax=ymax),
                                            linetype = threshold.linetype, linewidth = threshold.linewidth,col=threshold.linecol)
    } else {
      p4<-p4+geom_linerange(data = line_df, mapping = aes(x=x,ymin=ymin,ymax=ymax),
                            linetype = threshold.linetype, linewidth = threshold.linewidth,col=threshold.linecol)
    }
  }
  if (threshold.text==TRUE) {
    if (merge==TRUE) {
      p4<-p4+geom_label(data=point_df,
                        aes(x=x,y=y,label=label),nudge_x = nudge_x, nudge_y = nudge_y,size=po.text.size,
                        fill = po.text.fill,col=po.text.col)
    } else {
      p4<-p4+geom_label(data=point_df,
                        aes(x=x,y=y,label=label),nudge_x = nudge_x, nudge_y = nudge_y,size=po.text.size,
                        fill = po.text.fill,col=po.text.col)
    }
  }
  if (colbar==TRUE) {
    p<-p4
  } else {
    if (threshold.line==TRUE) {
      p3<-p3+geom_vline(aes(xintercept=t1), linetype=threshold.linetype,
                        linewidth = threshold.linewidth,colour=threshold.linecol)+
        geom_vline(aes(xintercept=t2),linetype=threshold.linetype,
                   linewidth = threshold.linewidth,colour=threshold.linecol)
    }
    if (threshold.text==TRUE) {
      p3<-p3+geom_label(data=point_df,
                        aes(x=x,y=y,label=label),nudge_x = nudge_x, nudge_y = nudge_y,size=po.text.size,
                        fill = po.text.fill,col=po.text.col)
      p<-p3
    }
  }
  p
}


