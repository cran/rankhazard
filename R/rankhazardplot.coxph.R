`rankhazardplot.coxph` <-
function(coxphobj,refvalues=NULL,x=NULL,legendtext=NULL,axistext=NULL,plottype="hazard",...)
{
    if (is.null(coxphobj$x) & is.null(x)) stop("Covariate data need to provided either as argument x or as coxphobj$x.")
    if (is.null(x)) 
    {
        if (!is.null(coxphobj$pterms)) stop("A model with pspline() requires the original data to be given as argument x.")
        x<-as.data.frame(coxphobj$x);
    }
    if (is.null(legendtext) & !is.null(axistext)) legendtext<-axistext;
    if (is.null(legendtext) & is.null(axistext))  legendtext<-colnames(x);
    if (is.null(axistext)) axistext<-legendtext;
    medians<-as.data.frame(rbind(apply(x,2,median,na.rm=TRUE)));
    newdata<-rbind(x,medians)
    newdata$temp1<-0
    newdata$temp2<-0
    survterms<-strsplit(gsub(" ","",sub(")","",sub("Surv(","",as.character(coxphobj$formula)[2],fixed=TRUE),fixed=TRUE),fixed=TRUE),",",fixed=TRUE)
    colnames(newdata)<-c(colnames(x),survterms[[1]][1],survterms[[1]][2])
    xp<-predict(coxphobj,newdata=newdata,type="terms");
    refvalues<-xp[dim(xp)[1],];
    rankhazardplot.default(x=x,xp=xp[1:(dim(xp)[1]-1),],refvalues=refvalues,legendtext=legendtext,axistext=axistext,plottype=plottype,...)
}

