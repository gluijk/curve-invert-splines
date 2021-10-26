# Curve inversion
invert.curve=function(x, nsample=256) {
    MAXDOTS=20  # max points to plot
    
    x=as.matrix(x)  # in case x is a vector
    ROWS=dim(x)[1]
    if (dim(x)[2]==1) x=cbind(seq(0,1,length.out=ROWS), x)  # x is vector
    
    par(mfrow=c(1,2))
    plot(x, main=paste0("Original curve (", ROWS," points)"),
         type='l', col='red', xlab='in', ylab='out')
    if (ROWS<=MAXDOTS) points(x)
    
    x=x[,2:1]  # swap in/out
    
    if (nsample>=2) {  # resample curve
        spl=spline(x, n=nsample, method="natural")
        m=cbind(spl$x, spl$y)
        plot(m, main=paste0("Inverted curve (", nsample," interp. points)"),
             type='l', col='red', xlab='in', ylab='out')
        if (nsample<=MAXDOTS) points(m)
        return(m)       
    } else {  # just transpose curve
        plot(x, main=paste0("Inverted curve (", ROWS," transp. points)"),
             type='l', col='red', xlab='in', ylab='out')
        if (ROWS<=MAXDOTS) points(x)
        return(x)
    }
}

x=as.matrix(read.csv("ARRI_AntiLog_curve.txt", header=FALSE, sep=' '))
m=invert.curve(x, nsample=18)
write.table(m, "ARRI_AntiLog_curve_INVERTED.txt", sep=' ',
            row.names=FALSE, col.names=FALSE)
