#'@title  newcrr
#'@name  newcrr
#'@description  Types of transformation of survival analysis models into competitive risk models.
#'@param fit Modelling for Survival Analysis.
#'@param cencode Censor status, default is 0.
#'@param failcode Events of interest, default is 1.
#'
#'
#'@return A list of competing risk model formats.
#'@export
#'


newcrr<-function(fit,cencode = 0, failcode = 1) {
  mod<-fit
  cencode<-cencode
  failcode<-failcode
  out<-list(mod=mod,failcode<-failcode,cencode<-cencode)
  class(out)<-"crr"
  out
}
