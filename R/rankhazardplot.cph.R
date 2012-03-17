`rankhazardplot.cph` <-
function(cphobj,refvalues=NULL,x=NULL,legendtext=NULL,axistext=NULL,plottype="hazard",...)
{
    if (is.null(cphobj$x) & is.null(x)) stop("Covariate data need to provided either as argument x or as cphobj$x.")
    if (is.null(x)) x<-as.data.frame(cphobj$x)[,cphobj$Design$name];
    if (is.null(legendtext) & !is.null(axistext)) legendtext<-axistext;
    if (is.null(legendtext) & is.null(axistext))  legendtext<-colnames(x);
    if (is.null(axistext)) axistext<-legendtext;
    xp<-predict(cphobj,newdata=x,type="terms");
    refvalues<-predict(cphobj,newdata=apply(x,2,median,na.rm=TRUE),type="terms");
    rankhazardplot.default(x=x,xp=xp,refvalues=refvalues,legendtext=legendtext,axistext=axistext,plottype=plottype,...)
}

