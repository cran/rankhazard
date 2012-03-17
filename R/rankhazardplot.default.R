`rankhazardplot.default` <-
function(x,coefs=NULL,xp=NULL,refvalues=NULL,legendtext=NULL,axistext=NULL,plottype="hazard",col=NULL,pch=NULL,lwd=1,...)
{
    n<-dim(x)[1];
    m<-dim(x)[2];
    ones<-matrix(1,nrow=n,ncol=1);
    if (is.null(xp) & is.null(coefs)) stop("Either coefs or xp must be provided.")
    if (!is.null(xp)) coefs<-rep(1,m);
    if (is.null(refvalues) & is.null(xp)) refvalues<-apply(x,2,median,na.rm=TRUE);
    if (is.null(refvalues) & !is.null(xp)) stop("When xp is given, also refvalues are required.");
    if (is.null(xp)) xp<-x;
    if (is.null(pch)) pch<-seq(0,m-1);
    if (is.null(col)) col<-1:m;
    if (isTRUE(all.equal(length(lwd),1))) lwd<-rep(lwd,m);
    if (is.null(axistext)) axistext<-legendtext;
    if (is.null(legendtext)) legendtext<-axistext;
    quantiles<-c(0,0.25,0.5,0.75,1);
    axistextpos<--0.1;
    logvar="";
    if (plottype=="hazard")
    {
        ylab<-"relative hazard";
        yfunc<-function(x,coefs,refvalues)
        {
            y<-exp( (ones %*% coefs) * (x-(ones %*% refvalues)) );
            return(y)
        }
        logvar="y";
    }
    if (plottype=="loghazard")
    {
        ylab<-"logarithm of relative hazard";
        yfunc<-function(x,coefs,refvalues)
        {
            y<- (ones %*% coefs) * (x-(ones %*% refvalues)) ;
            return(y)
        }
        logvar="";
    }
    y<-yfunc(xp,coefs,refvalues);
    maxy<-max(y,na.rm=TRUE);
    miny<-min(y,na.rm=TRUE)
    print(c(miny,maxy))
    ind<-apply(x,2,order,na.last = TRUE); 
    nasum<-colSums(is.na(x))
    for (j in 1:m)
    {
        nj<-n-nasum[j];
        ranks<-seq(0,1,length=nj);
        if (j==1) 
        {
            plot(ranks,y[ind[1:nj,j],j],ylim=c(miny,maxy),xlab="",ylab=ylab,type="l",col=col[j],pch=pch[j],lwd=lwd[j],axes=FALSE,log=logvar,...);
            points(quantiles,y[ind[1:nj,j],j][quantile(1:nj,probs=quantiles)],col=col[j],pch=pch[j]);
            if (plottype=="hazard") yat<-c(pretty(c(miny,1)),pretty(c(1,maxy)),signif(miny/2,2),signif(2*maxy,2));
            if (plottype=="loghazard") yat<-c(pretty(c(miny,0)),pretty(c(0,maxy)),signif(2*miny,2),signif(2*maxy,2));
            yat2<-c(min(yat),max(yat),labels=c("",""));
            axis(1,at=c(-10,quantiles,2),labels=FALSE)
            axis(2,at=yat,labels=as.character(yat));
            axis(3,at=c(-10,2))
            axis(4,at=yat2)
        } else {
            lines(ranks,y[ind[1:nj,j],j],col=col[j],lwd=lwd[j],...);
            points(quantiles,y[ind[1:nj,j],j][quantile(1:nj,probs=quantiles)],col=col[j],pch=pch[j]);
        }
        mtext(side=1,at=c(axistextpos,quantiles),adj=c(1,rep(0.5,length(quantiles))),
            text=c(axistext[j],signif(quantile(x[,j],probs=quantiles,na.rm=TRUE),3)),line=j);
    }
    legend("top",legend=legendtext,col=col,lwd=lwd,pch=pch,bty="n")
}

