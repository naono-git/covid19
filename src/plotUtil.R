ccc7 <- c(1:6, 8)
resetX11 <- function(...){
    graphics.off()
    display <- readline("display no?:")
    x11(display=paste("localhost:", display, sep=""), ...)
}

par.margin <- function(type="smart"){
    if(type=="smart"){
        par(cex=0.8, mgp=c(2, 1, 0), mar=c(3, 3, 2, 1)+0.1)
    }
    if(type=="narrow"){
        par(cex=0.8, mgp=c(1, 1, 0), mar=c(1, 1, 1, 1)+0.1)
    }

    if(type=="none"){
        par(mgp=c(0, 0, 0), mar=c(0, 0, 0, 0))
    }
}

axis.log10 <- function(side=2, range=NULL, tickrange=NULL){
    if(is.null(range)){
        if(side==1 || side==3){
            range <- as.integer(par("usr")[1]):as.integer(par("usr")[2])
        }
        if(side==2 || side==4){
            range <- as.integer(par("usr")[3]):as.integer(par("usr")[4])
        }
    } else {
        range <- as.integer(range[1]):as.integer(range[2])
    }
    nn <- length(range)
    if(is.null(tickrange)){
        tickrange <- range
    }

    ticklabels <- 10^range

    axis(side, at=range, labels=ticklabels)
    for(ii in range[-1]){
        axis(side, at=log10(2:9*10^(ii-1)), labels=rep("", 8), tcl=-0.3)
    }
}

plotFigure <- function(plotfunction,  margin="smart",
                       np=1, px=NULL, py=NULL,
                       rr=1, rx=NULL, ry=NULL,
                       directory="fig", filename="neko", dev="x11",
                       silent=FALSE,
                       bg = "white",
                       useJapanese=FALSE, ...){

    s0 <- 3.34
    if(is.null(px) || is.null(py)){
        py <- as.integer(sqrt(np))
        px <- ceiling(np/py)
    }

    if(is.null(rx)){
        w1 <- rr*px
    }else{
        w1 <- rx
    }
    if(is.null(ry)){
        h1 <- rr*py
    } else {
        h1 <- ry
    }

    if(useJapanese){
        fontFamily <- "HiraKakuProN-W3"
    } else {
        fontFamily <- "Helvetica"
    }

    for(dev1 in dev){
        if(dev1=="pdf_quartz"){
            filename=paste(filename, ".pdf", sep="")
            if(!silent){
                cat(filename, "\n")
            }
            quartz(type="pdf", file=file.path(directory, filename),
                   bg=bg, width=s0*w1, height=s0*h1,
                   family=fontFamily)
        }
        if(dev1=="pdf"){
            filename=paste(filename, ".pdf", sep="")
            if(!silent){
                cat(filename, "\n")
            }
            pdf(file=file.path(directory, filename),
                bg=bg, width=s0*w1, height=s0*h1,
                family="Japan1GothicBBB")
        }
        if(dev1=="eps"){
            filename=paste(filename, ".eps", sep="")
            if(!silent){
                cat(filename, "\n")
            }
        }
        if(dev1 %in% c("jpeg", "jpg")){
            filename=paste(filename, ".jpg", sep="")
            if(!silent){
                cat(filename, "\n")
            }
            jpeg(file=file.path(directory, filename), pointsize=24, width=460*w1, height=460*h1)
            par(family=fontFamily)
        }
        if(dev1=="png"){
            filename=paste(filename, ".png", sep="")
            if(!silent){
                cat(filename, "\n")
            }
            png(file=file.path(directory, filename), width=230*w1, height=230*h1)
            par(family=fontFamily)
        }
        if(dev1=="x11"){
            par(family=fontFamily)
        }
        if(dev1=="jupyter"){
            par(family=fontFamily)
        }

        if(np > 1){
            par(mfrow=c(py, px))
        }

        par.margin(margin)

        plotfunction(...)

        if(dev1=="eps"){
            dev1.copy2eps(file=file.path(directory, filename), bg=bg, width=s0*w1, height=s0*h1)
        } else if(dev1!="x11" & dev1!="pdf*"){
            dev.off()
        }

        if(np > 1){
            par(mfrow=c(1, 1))
        }
    }
}

plotLegend <- function(...){
    plot(1:3, 1:3, type="n", axes=FALSE, frame=FALSE, xlab="", ylab="", main="")
    legend("topleft", ...)
}

## Table of plotting symbols
pchShow <- function(extras = c("*", ".",  "o", "O", "0", "+", "-", "|", "%", "#"),
                    cex = 3,  ## good for both .Device=="postscript" and "x11"
                    col = "red3",  bg = "gold",  coltext = "brown",  cextext = 1.2,
                    main = paste("plot symbols :  points (...  pch = *,  cex =",
                    cex, ")"))
{
    nex <- length(extras)
    np  <- 26 + nex
    ipch <- 0:(np-1)
    k <- floor(sqrt(np))
    dd <- c(-1, 1)/2
    rx <- dd + range(ix <- ipch %/% k)
    ry <- dd + range(iy <- 3 + (k-1)- ipch %% k)
    pch <- as.list(ipch) # list with integers & strings
    if(nex > 0) pch[26+ 1:nex] <- as.list(extras)
    plot(rx, ry, type="n", axes = FALSE, xlab = "", ylab = "",
         main = main)
    abline(v = ix, h = iy, col = "lightgray", lty = "dotted")
    for(i in 1:np) {
        pc <- pch[[i]]
        ## 'col' symbols with a 'bg'-colored interior (where available) :
        points(ix[i], iy[i], pch = pc, col = col, bg = bg, cex = cex)
        if(cextext > 0)
            text(ix[i] - 0.3, iy[i], pc, col = coltext, cex = cextext)
    }
}

plotMatrix <- function(xxx, grid="none", interpolate=FALSE, scale=TRUE, rr=1){
    nr <- dim(xxx)[1]
    nc <- dim(xxx)[2]
    if(scale){
        xxx <- (xxx-min(xxx))/(max(xxx)-min(xxx))
    }
    options(repr.plot.width=nc*rr, repr.plot.height=nr*rr)
    par.margin("narrow")
    plot(0,1,xlim=c(0,nc),ylim=c(0,nr),type="n",xlab=NA, ylab=NA,xaxs="i", yaxs="i", axes=FALSE, frame=TRUE)
    rasterImage(xxx,0,0,nc,nr, interpolate=interpolate)
    if(grid=="gray"){
        segments(x0=0,x1=nc,y0=0:nr,y1=0:nr,lty=3,col="#80808080")
        segments(x0=0:nc,x1=0:nc,y0=0,y1=nr,lty=3,col="#80808080")
    }
}
