pr.fac<-function(model,plotfactor, factorname="", modeltype="lm")
{
  if(modeltype %in% c("linear","lm","poisson","p","quasipossion","qp"))
  {
    skiprows<-unique(summary(model)$na.action)
  }
  if(modeltype %in% c("negativebinomial","nb"))
  {
    skiprows<-unique(summary(model)[21])
  }
  if(length(skiprows)>0) {Factor<-plotfactor[-skiprows]}
  if(length(skiprows)==0) {Factor<-plotfactor}
  plot1<-data.frame(PR=resid(model, type="pearson"),Factor)
  if(is.factor(Factor)==FALSE)
  {
    PR.plot1<<-ggplot(plot1)+
      geom_point(aes(y=PR, x=Factor))+theme_bw(22)+
      geom_hline(yintercept=0, linetype='dashed', col='red')+
      ylab("Residuals")+xlab(factorname)
  }
  if(is.factor(Factor)==TRUE)
  {
    PR.plot1<<-ggplot(plot1)+
      geom_boxplot(aes(y=PR, x=Factor))+theme_bw(22)+
      geom_hline(yintercept=0, linetype='dashed', col='red')+
      ylab("Residuals")+xlab(factorname)
  }
  return(PR.plot1)
}


dispersion<-function(model,modeltype='gaussian')
{
  A<-sum(resid(model,type="pearson")^2)
  if(modeltype %in% c("g","p","qp","gaussian","poisson","quasipoisson"))
  {
    B<-length(resid(model))-length(coef(model))
  }
  if(modeltype %in% c("nb","negativebinomial"))
  {
    B<-length(resid(model))-(length(coef(model))+1)
  }
  DISP<<-A/B
  return(DISP)
}