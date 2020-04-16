library(rjson)

pref47_ja <- fromJSON(file="data/jisx0401-ja.json")
pid <- 1:47
names(pid) <- unlist(pref47_ja)
pref47 <- fromJSON(file="data/jisx0401-en.json")


fidtable <- read.table("data/fid.txt")

pref47.tested <- matrix(0,nrow=0,ncol=50)
pref47.infected <- matrix(0,nrow=0,ncol=50)
pref47.recovered <- matrix(0,nrow=0,ncol=50)
pref47.dead <- matrix(0,nrow=0,ncol=50)

colnames(pref47.tested) <- c("Month","Day",pref47,"Total")
colnames(pref47.infected) <- c("Month","Day",pref47,"Total")
colnames(pref47.recovered) <- c("Month","Day",pref47,"Total")
colnames(pref47.dead) <- c("Month","Day",pref47,"Total")

pref47.tmp <- integer(48)
names(pref47.tmp) <- c(names(pid),"総計")

for(aa in 1:nrow(fidtable)){
    mm <- fidtable[aa,1]
    dd <- fidtable[aa,2]
    fid <- substring(fidtable[aa,3],2)
    cat(mm,dd,"\n")
    file_src <- paste("tabula-",fid,".csv",sep="")
    if(file.exists(file.path("~/Downloads", file_src))){
        system(paste("mv ~/Downloads/tabula-",fid,".csv data",sep=""))
    }
    tmp <- read.delim(file.path("data", file_src), sep=",",header=TRUE)
    tmp <- tmp[tmp[,1]!="",]
    iii <- which(tmp[,1] %in% names(pid))
    pii <- pid[tmp[iii,1]]
    nii <- which(tmp[,1]=="総計")

    if(mm==3 && dd<=31){
        j_recovered <- 6
        j_dead <- 8
    } else {
        j_recovered <- 7 # which(colnames(tmp)=="うち退院")
        j_dead <- 9 # which(colnames(tmp)=="うち死亡")
    }

    pref47.tmp[] <- 0
    pref47.tmp[pii] <- tmp[iii,2]
    pref47.tmp[48] <- tmp[nii,2]
    if(any(is.na(pref47.tmp))){
        print(pref47.tmp)
    }
    pref47.infected <- rbind(pref47.infected, c(mm,dd,pref47.tmp))

    pref47.tmp[] <- 0
    pref47.tmp[pii] <- tmp[iii,j_recovered]
    pref47.tmp[48] <- tmp[nii,j_recovered]
    if(any(is.na(pref47.tmp))){
        print(pref47.tmp)
    }
    pref47.recovered <- rbind(pref47.recovered, c(mm,dd,pref47.tmp))

    pref47.tmp[] <- 0
    pref47.tmp[pii] <- tmp[iii,j_dead]
    pref47.tmp[48] <- tmp[nii,j_dead]
    if(any(is.na(pref47.tmp))){
        print(pref47.tmp)
    }
    pref47.dead <- rbind(pref47.dead, c(mm,dd,pref47.tmp))
}

writeMatrix(pref47.infected,dir="data", col=TRUE, force=TRUE)
writeMatrix(pref47.recovered,dir="data", col=TRUE, force=TRUE)
writeMatrix(pref47.dead,dir="data", col=TRUE, force=TRUE)

pjj_kinki4 <- pid[c("京都府","大阪府","兵庫県","奈良県")]
matplot(pref47.infected[,pjj_kinki4+2],type="b", pch=1:7,col=1:8,log="y")
