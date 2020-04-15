library(rjson)

pref47_ja <- fromJSON(file="data/jisx0401-ja.json")
pid <- 1:47
names(pid) <- unlist(pref47_ja)
pref47 <- fromJSON(file="data/jisx0401-en.json")


fidtable <- read.table("data/fid.txt")

xxx.tested <- matrix(0,nrow=0,ncol=50)
xxx.infected <- matrix(0,nrow=0,ncol=50)
xxx.recovered <- matrix(0,nrow=0,ncol=50)
xxx.dead <- matrix(0,nrow=0,ncol=50)

xxx.tmp <- integer(48)
names(xxx.tmp) <- c(names(pid),"総計")

for(aa in 1:nrow(fidtable)){
    mm <- fidtable[aa,1]
    dd <- fidtable[aa,2]
    fid <- substring(fidtable[aa,3],2)
    cat(mm,dd,"\n")
    file_src <- paste("tabula-",fid,".csv",sep="")
    if(file.exists(file.path("/Users/nono/Downloads", file_src))){
        system(paste("mv /Users/nono/Downloads/tabula-",fid,".csv data",sep=""))
    }
    tmp <- read.delim(file.path("data", file_src), sep=",",header=TRUE)
    tmp <- tmp[tmp[,1]!="",]
    iii <- which(tmp[,1] %in% names(pid))
    pii <- pid[tmp[iii,1]]
    nii <- which(tmp[,1]=="総計")

    j_recovered <- 7 # which(colnames(tmp)=="うち退院")
    j_dead <- 9 # which(colnames(tmp)=="うち死亡")

    xxx.tmp[] <- 0
    xxx.tmp[pii] <- tmp[iii,2]
    xxx.tmp[48] <- tmp[nii,2]
    if(any(is.na(xxx.tmp))){
        print(xxx.tmp)
    }
    xxx.infected <- rbind(xxx.infected, c(mm,dd,xxx.tmp))

    xxx.tmp[] <- 0
    xxx.tmp[pii] <- tmp[iii,j_recovered]
    xxx.tmp[48] <- tmp[nii,j_recovered]
    if(any(is.na(xxx.tmp))){
        print(xxx.tmp)
    }
    xxx.recovered <- rbind(xxx.recovered, c(mm,dd,xxx.tmp))

    xxx.tmp[] <- 0
    xxx.tmp[pii] <- tmp[iii,j_dead]
    xxx.tmp[48] <- tmp[nii,j_dead]
    if(any(is.na(xxx.tmp))){
        print(xxx.tmp)
    }
    xxx.dead <- rbind(xxx.dead, c(mm,dd,xxx.tmp))
}
