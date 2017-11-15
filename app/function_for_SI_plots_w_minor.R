check=""
plot_chromo <- function(chr,don,data){
    calls  = readRDS("candidates_ind_w_inter_info_processed.rds")
    cand = chr
    chr=paste("chr",cand,sep="")

    common_ggplot2 <- theme_bw() + theme(axis.text.x=element_text(size=20,angle=0),
                                         axis.text.y=element_text(size=20),
                                         axis.title.y=element_text(size=20),
                                         axis.title.x=element_blank(),
                                         legend.title = element_blank(),
                                         legend.text = element_text(size=20),
                                         legend.position = "right",
                                         legend.key = element_blank(),
                                         plot.margin=unit(c(0.1,0.1,0,0.1),"cm"),
                                         plot.title=element_blank()) +
theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), legend.position="bottom",legend.title=element_blank()) +
theme(plot.background = element_blank(),
      axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
      axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))  
#-----------------------------------------------------------------------------------------------------------------------------------

svs = read.table(paste0("bedpes/",data$SV_sample[1]),sep="\t",header=T,stringsAsFactors=F)
#svs = svs[, c(1,2,4,5,11)]

#CNVs  = read.table(paste0("CNVs/301d6ce3-4099-4c1d-8e50-c04b7ce91450.consensus.20170119.somatic.cna.annotated.txt") , header=T,sep="\t")
CNVs  = read.table(data$CNV[1] , header=T,sep="\t")
CNVs=CNVs[,(1:4)]
names(CNVs) = c("chrom","start","end", "log2copyratio")


#summary = chromthripsis_ICGC@chromSummary
#candidate_chrs <- chromthripsis_ICGC@chromSummary$chrom 
#cluster_sizes <- sapply(chromthripsis_ICGC@detail$connComp,length)
## get the SVs for this chromosome that are clustered together
#cand_clust_size <- chromthripsis_ICGC@chromSummary$clusterSize[ chromthripsis_ICGC@chromSummary$chrom == cand]
#idx = which(cluster_sizes == cand_clust_size) 
#SVsnow <- chromthripsis_ICGC@detail$SV[ as.numeric(unlist(chromthripsis_ICGC@detail$connComp[idx])) ,]

SVsnow = svs[which(svs$chrom1 == cand & svs$chrom2 == cand),][, c(1,2,4,5,11)]
names(SVsnow) = c("chrom1","pos1" ,"chrom2","pos2" ,"SVtype") #,"chromothEvent")
CNVsnow <- CNVs[which(CNVs$chrom == cand),]  ##chromthripsis_ICGC@detail$CNV

SVsnow <- unique(SVsnow[SVsnow$chrom1 == cand, ]) # remove if there are more
CNVsnow <- CNVsnow[CNVsnow$chrom == cand, ] # remove if there are more
df = SVsnow

# old only for cluster
#min_coord = min(as.numeric(c(df$pos1, df$pos2)),na.rm=T)
#max_coord = max(as.numeric(c(df$pos1, df$pos2)),na.rm=T)
# new entire chr
min_coord = min(as.numeric(c(CNVsnow$start, CNVsnow$end)),na.rm=T)
max_coord = max(as.numeric(c(CNVsnow$start, CNVsnow$end)),na.rm=T)


# no SVs in sample
if (nrow(df) != 0){


id = which(svs$chrom1 != svs$chrom2)

if (length(id)>0){
    inter= svs[id,]
    inter$type_SV = rep("",nrow(inter))
    inter$type_SV[which(inter$strand1 == "-" & inter$strand2 == "-")] = "t2tINV"
    inter$type_SV[which(inter$strand1 == "-" & inter$strand2 == "+")] = "DUP"
    inter$type_SV[which(inter$strand1 == "+" & inter$strand2 == "-")] = "DEL"
    inter$type_SV[which(inter$strand1 == "+" & inter$strand2 == "+")] = "h2hINV"

    inter2=c()
    check=""
    a = which(inter$chrom1 == cand)
    pepe = inter[a,c(1:3,13)]
    names(pepe) = c("chrom","pos","end","type_SV")
    if (length(a)>0){inter2 = rbind(inter2,pepe); check="a"} #colnames(inter2) = c("chrom","pos","end","type_SV") }
    b = which(inter$chrom2 == cand)
    pepe=inter[b,c(4:6,13)]
    names(pepe) = c("chrom","pos","end","type_SV")
    if (length(b)>0){inter2 = rbind(inter2,pepe); check="a"} # colnames(inter2) = c("chrom","pos","end","type_SV") }
    if(check=="a"){

        names(inter2)[1] = "chrom"
        names(inter2)[2] = "pos"

        idx = which(inter2$pos >= min_coord | inter2$pos <= max_coord)
        if (length(idx)>0){
            inter2=inter2[idx,]

            inter2$y = rep(0,nrow(inter2))
            inter2$y[which(inter2$type_SV %in% c("DUP","DEL"))] = 4
            inter2$y[!(inter2$type_SV %in% c("DUP","DEL"))] = 12
            inter2$colour = rep("",nrow(inter2))
            inter2$colour[which(inter2$type_SV == "DUP")] = "blue1"
            inter2$colour[which(inter2$type_SV == "DEL")] = "darkorange1"
            inter2$colour[which(inter2$type_SV == "h2hINV")] = "black"
            inter2$colour[which(inter2$type_SV == "t2tINV")] = "forestgreen"
        }else{check="sdfds"}
    }
}

idx1=which(CNVsnow$start >= min_coord)
#idx2=which(CNVsnow$end = max_coord)    ##changes ad hoc
idx=idx1#intersect(idx1,idx2)
CNVsnow = CNVsnow#[idx,]


df$SVtype <- gsub("SVCLASS=","",df$SVtype)
y1=4
y2=12
df$y1 = rep(y1,nrow(df))
df$y2 = rep(y2,nrow(df))
df$diff = abs(df$pos1 - df$pos2)
df$curv = 1 -(df$diff / max(df$diff))
max_diff = max(df$diff)
# poner la curvatura en funcion del rango de coordenadas.. no del ratio
df$curv[(df$diff / max_diff) > 0.2] <- .15
df$curv[(df$diff / max_diff) > 0.8] <- .08
df$curv[(df$diff / max_diff) < 0.2] <- 1
d = data.frame(x=c(min_coord),y=c(1),leg=c("DEL","DUP","t2tINV","h2hINV"))
idx = c()

breakpoints = ggplot(d,aes(x=x,y=y)) +
    geom_point(colour="white") + ylim(0,y2+5) + common_ggplot2 

breakpoints = breakpoints + geom_hline(yintercept=y1,size=1) + 
    geom_hline(yintercept=y2,size=1) + theme(axis.ticks.x=element_blank(),panel.border = element_blank(),
                                               axis.title.y=element_text(colour="white"),
                                               axis.text.y=element_text(colour="white"),
                                               axis.ticks.y=element_line(colour="white")) + 
scale_x_continuous(expand = c(0.01,0.01), limits=c(min_coord,max_coord))

now = df[df$SVtype == "DUP",]
if (nrow(now) > 0){
    for (i in 1:nrow(now)){
        breakpoints = breakpoints +
            geom_curve( size=.7,data = now[i,] , aes(x = pos1, y = y1, xend = pos2, yend = y1), curvature = now$curv[i],colour="blue1",ncp=8)
    }
    idx= c(idx,1)
}
breakpoints = breakpoints + geom_point(data=now,size=3,aes(x=pos1,y=y1)) + geom_point(data=now,size=3,aes(x=pos2,y=y1))


now = df[df$SVtype == "DEL",]
if (nrow(now) > 0){
    for (i in 1:nrow(now)){
        breakpoints = breakpoints +
            geom_curve( size=.7,data = now[i,] , aes(x = pos1, y = y1, xend = pos2, yend = y1), curvature = -1*now$curv[i],colour="darkorange1") 
    }
    idx= c(idx,2)
}
breakpoints = breakpoints + geom_point(data=now,size=3,aes(x=pos1,y=y1)) + geom_point(data=now,size=3,aes(x=pos2,y=y1))

now = df[df$SVtype == "t2tINV",]
if (nrow(now) > 0){
    for (i in 1:nrow(now)){
        breakpoints = breakpoints +
            geom_curve( size=.7,data = now[i,] , aes(x = pos1, y = y2, xend = pos2, yend = y2), curvature = now$curv[i],colour="forestgreen") 
    }
    idx= c(idx,3)
}
breakpoints = breakpoints + geom_point(data=now,size=3,aes(x=pos1,y=y2)) + geom_point(data=now,size=3,aes(x=pos2,y=y2))



now = df[df$SVtype == "h2hINV",]
if (nrow(now) > 0){
    for (i in 1:nrow(now)){
        breakpoints = breakpoints +
            geom_curve( size=.7,data = now[i,] , aes(x = pos1, y = y2, xend = pos2, yend = y2), curvature = -1*now$curv[i],colour="black") 
    }
    idx= c(idx,4)
}
breakpoints = breakpoints + geom_point(data=now,size=3,aes(x=pos1,y=y2)) + geom_point(data=now,size=3,aes(x=pos2,y=y2))

# inter
if (check=="a"){
    inter2$type_SV = factor(inter2$type_SV,levels=c("DEL","DUP","h2hINV","t2tINV"))
    breakpoints = breakpoints + geom_point(data=inter2,size=4,shape=18,alpha=1,aes(x=pos,y=as.numeric(y),colour=type_SV)) #+
}


idx = c(1,2,3,4)
vals = c('DEL'='darkorange1','DUP'='blue1',"t2tINV"="forestgreen","h2hINV"="black")
labs = c('DEL','DUP',"t2tINV","h2hINV")

breakpoints = breakpoints +
    geom_line(data=rbind(d,d),aes(x=x,y=y,colour=leg)) +
    scale_colour_manual(name = 'SV type', 
                        values =c('darkorange1','blue1',"forestgreen","black"),
                        labels = labs[idx])  +
theme(legend.position="none")


} else {
d = data.frame(x=c(min_coord),y=c(1),leg=c("DEL","DUP","t2tINV","h2hINV"))
	y1=4
	y2=12

breakpoints = ggplot(d,aes(x=x,y=y)) +
    geom_point(colour="white") + ylim(0,y2+5) + common_ggplot2 

breakpoints = breakpoints + geom_hline(yintercept=y1,size=1) + 
    geom_hline(yintercept=y2,size=1) + theme(axis.ticks.x=element_blank(),panel.border = element_blank(),
                                               axis.title.y=element_text(colour="white"),
                                               axis.text.y=element_text(colour="white"),
                                               axis.ticks.y=element_line(colour="white")) + 
scale_x_continuous(expand = c(0.01,0.01), limits=c(min_coord,max_coord))

# no breakpoints

}


##################################################################################################
if (max(CNVsnow$log2copyratio,na.rm=T) <=10){
    CNV_plot = ggplot() +
        geom_segment(data=CNVsnow, aes(x = start, y =log2copyratio , xend = end, yend = log2copyratio),size=5) + 
        common_ggplot2 + scale_color_manual(values=c("forestgreen","red1","blue","brown")) +
            ylab("Absolute Copy Number (CN)")+ xlab(NULL) + 
            scale_x_continuous(expand = c(0.01,0.01),labels = function(x){paste(x/1000000,"MB")})
            CNV_plot = CNV_plot + scale_y_continuous(minor_breaks = NULL,breaks=c(0,sort(unique(CNVsnow$log2copyratio))),
                                                     limits=c(min(CNVsnow$log2copyratio) -0.35,max(CNVsnow$log2copyratio,na.rm=T)+.35)) + 
coord_cartesian(xlim=c(min_coord,max_coord),expand = c(0.01,0.01))
}

if (max(CNVsnow$log2copyratio,na.rm=T) <=100 & max(CNVsnow$log2copyratio,na.rm=T) >10){
    idx = which(CNVsnow$log2copyratio ==0);if(length(idx)>0){CNVsnow$log2copyratio[which(CNVsnow$log2copyratio ==0)]=0.99}
    mm2 = max(CNVsnow$log2copyratio,na.rm=T)
    mmin = min(CNVsnow$log2copyratio,na.rm=T)
    vals = sort( unique(CNVsnow$log2copyratio) )
    mm=vals
    vals=vals[1:(length(vals)-1)]
    if(length(vals)>5){vals=vals[seq(1,length(vals),2)]}
    if(mm2 <=20){mm = c(0,1,2,4,10,mm2)} #sort( unique(CNVsnow$log2copyratio) )} #c(0,1,2,4,10,mm2)}
    if(mm2 <=40 & mmin >20){if(length(vals)>6){vals=vals[seq(1,length(vals),2)]};mm = c(vals,mm2)} #sort( unique(CNVsnow$log2copyratio) )} #mm = c(0,1,2,4,10,20,mm2)}
    if(mm2 <=40 & mmin <20){if(length(vals)>6){vals=vals[seq(1,length(vals),2)]};mm = c(vals,mm2)} #sort( unique(CNVsnow$log2copyratio) )} #mm = c(0,1,2,4,10,20,mm2)}
    if(mm2 <=100 & mmin >40){if(length(vals)>6){vals=vals[seq(1,length(vals),2)]};mm = c(vals,mm2)} #sort( unique(CNVsnow$log2copyratio) )} #mm = c(0,1,2,4,10,20,mm2)}
    if( mm2 >= 40 & mmin <20){if(length(vals)>6){vals=vals[seq(1,length(vals),2)]};mm = c(vals,mm2)} #sort( unique(CNVsnow$log2copyratio) )} #mm = c(0,1,2,4,10,20,mm2)}

    CNV_plot = ggplot() +
        geom_segment(data=CNVsnow, aes(x = start, y =log(log2copyratio,base=2) , xend = end, yend = log(log2copyratio,base=2)),size=5) + 
        common_ggplot2 + scale_color_manual(values=c("forestgreen","red1","blue","brown")) +
            ylab("Absolute Copy Number (CN)")+ xlab(NULL)+ scale_x_continuous(expand = c(0.01,0.01),labels = function(x){paste(x/1000000,"MB")})#,limits=c(min_coord,max_coord))

        CNV_plot = CNV_plot + scale_y_continuous(minor_breaks = NULL,breaks=log(mm,base=2), 
                                                 #limits=c(0, max(CNVsnow$log2copyratio)), 
                                                 limits=c( -.01 + log(min(CNVsnow$log2copyratio),base=2), 
                                                          log(max(CNVsnow$log2copyratio,na.rm=T) , base=2)+.01 ), 
                                                 labels=as.character(mm)) +
coord_cartesian(xlim=c(min_coord,max_coord),expand = c(0.01,0.01))
}

if (max(CNVsnow$log2copyratio,na.rm=T) >100){
    idx = which(CNVsnow$log2copyratio ==0);if(length(idx)>0){CNVsnow$log2copyratio[which(CNVsnow$log2copyratio ==0)]=0.99}
    CNV_plot = ggplot() +
        geom_segment(data=CNVsnow, aes(x = start, y =log(log2copyratio,base=10) , xend = end, yend = log(log2copyratio,base=10)),size=5) + 
        common_ggplot2 + scale_color_manual(values=c("forestgreen","red1","blue","brown")) +
            ylab("Absolute Copy Number (CN)")+ xlab(NULL)+ scale_x_continuous(expand = c(0.01,0.01),labels = function(x){paste(x/1000000,"MB")})#,limits=c(min_coord,max_coord))
        mm2 = max(CNVsnow$log2copyratio,na.rm=T)
        mm = c(2,4,20,50,100,mm2)
        CNV_plot = CNV_plot + scale_y_continuous(minor_breaks = NULL,breaks=log(mm,base=10), 
                                                 limits=c( log(min(CNVsnow$log2copyratio),base=10), 
                                                          log(max(CNVsnow$log2copyratio,na.rm=T) , base=10)), 
                                                 labels=as.character(mm)) +
coord_cartesian(xlim=c(min_coord,max_coord),expand = c(0.01,0.01))
}

#################################
# minor CN 
#################################
# get minor CN
#minor  = read.table(paste0("CNVs/",data$CNV[1]) , header=T,sep="\t")
#minor  = read.table(paste0("CNVs/301d6ce3-4099-4c1d-8e50-c04b7ce91450.consensus.20170119.somatic.cna.annotated.txt") , header=T,sep="\t")
minor  = read.table(data$CNV[1] , header=T,sep="\t")
minor = minor[which(minor$chromosome == cand),]
CNVsnow2 =CNVsnow
CNVsnow2$minor = minor$minor_cn[match(CNVsnow$start,minor$start)]
CNVsnow2$log2copyratio = CNVsnow2$minor

if (max(CNVsnow2$log2copyratio,na.rm=T) <=10){
    CNV_plot_minor = ggplot() +
        geom_segment(data=CNVsnow2, aes(x = start, y =log2copyratio , xend = end, yend = log2copyratio),size=5) + 
        #coord_cartesian(xlim=c(min_coord,max_coord),expand = c(0.01,0.01),labels = function(x){paste(x/1000000,"MB")}) +
        common_ggplot2 + scale_color_manual(values=c("forestgreen","red1","blue","brown")) +
            ylab("Minor CN")+ xlab(NULL) + 
            scale_x_continuous(expand = c(0.01,0.01),labels = function(x){paste(x/1000000,"MB")}) #X,limits=c(min_coord,max_coord))
            CNV_plot_minor = CNV_plot_minor + scale_y_continuous(minor_breaks = NULL,breaks=c(0,sort(unique(CNVsnow2$log2copyratio))),
                                                                 limits=c(min(CNVsnow2$log2copyratio) -0.35,max(CNVsnow2$log2copyratio,na.rm=T)+.35)) + 
coord_cartesian(xlim=c(min_coord,max_coord),expand = c(0.01,0.01))
}

if (max(CNVsnow2$log2copyratio,na.rm=T) <=100 & max(CNVsnow2$log2copyratio,na.rm=T) >10){
    idx = which(CNVsnow2$log2copyratio ==0);if(length(idx)>0){CNVsnow2$log2copyratio[which(CNVsnow2$log2copyratio ==0)]=0.99}
    mm2 = max(CNVsnow2$log2copyratio,na.rm=T)
    mmin = min(CNVsnow2$log2copyratio)
    vals = sort( unique(CNVsnow2$log2copyratio) )
    mm=vals
    vals=vals[1:(length(vals)-1)]
    if(length(vals)>5){vals=vals[seq(1,length(vals),2)]}
    if(mm2 <=20){mm = c(0,1,2,4,10,mm2)} #sort( unique(CNVsnow2$log2copyratio) )} #c(0,1,2,4,10,mm2)}
    if(mm2 <=40 & mmin >20){if(length(vals)>6){vals=vals[seq(1,length(vals),2)]};mm = c(vals,mm2)} #sort( unique(CNVsnow2$log2copyratio) )} #mm = c(0,1,2,4,10,20,mm2)}
    if(mm2 <=100 & mmin >40){if(length(vals)>6){vals=vals[seq(1,length(vals),2)]};mm = c(vals,mm2)} #sort( unique(CNVsnow2$log2copyratio) )} #mm = c(0,1,2,4,10,20,mm2)}
    if( mm2 >= 40 & mmin <20){if(length(vals)>6){vals=vals[seq(1,length(vals),2)]};mm = c(vals,mm2)} #sort( unique(CNVsnow2$log2copyratio) )} #mm = c(0,1,2,4,10,20,mm2)}
    #if(mm2 <=100 & mmin <40){mm = c(vals,mm2)} #sort( unique(CNVsnow2$log2copyratio) )} #mm = c(0,1,2,4,10,20,mm2)}

    CNV_plot_minor = ggplot() +
        geom_segment(data=CNVsnow2, aes(x = start, y =log(log2copyratio,base=2) , xend = end, yend = log(log2copyratio,base=2)),size=5) + 
        common_ggplot2 + scale_color_manual(values=c("forestgreen","red1","blue","brown")) +
            ylab("Minor CN")+ xlab(NULL)+ scale_x_continuous(expand = c(0.01,0.01),labels = function(x){paste(x/1000000,"MB")})#,limits=c(min_coord,max_coord))

        CNV_plot_minor = CNV_plot_minor + scale_y_continuous(minor_breaks = NULL,breaks=log(mm,base=2), 
                                                             #limits=c(0, max(CNVsnow2$log2copyratio)), 
                                                             limits=c( -.01 + log(min(CNVsnow2$log2copyratio),base=2), 
                                                                      log(max(CNVsnow2$log2copyratio,na.rm=T) , base=2)+.01 ), 
                                                             labels=as.character(mm)) +
coord_cartesian(xlim=c(min_coord,max_coord),expand = c(0.01,0.01))
}

if (max(CNVsnow2$log2copyratio,na.rm=T) >100){
    idx = which(CNVsnow2$log2copyratio ==0);if(length(idx)>0){CNVsnow2$log2copyratio[which(CNVsnow2$log2copyratio ==0)]=0.99}
    CNV_plot_minor = ggplot() +
        geom_segment(data=CNVsnow2, aes(x = start, y =log(log2copyratio,base=10) , xend = end, yend = log(log2copyratio,base=10)),size=5) + 
        common_ggplot2 + scale_color_manual(values=c("forestgreen","red1","blue","brown")) +
            ylab("Minor CN")+ xlab(NULL)+ scale_x_continuous(expand = c(0.01,0.01),labels = function(x){paste(x/1000000,"MB")})#,limits=c(min_coord,max_coord))
        mm2 = max(CNVsnow2$log2copyratio,na.rm=T)
        mm = c(2,4,20,50,100,mm2)
        CNV_plot_minor = CNV_plot_minor + scale_y_continuous(minor_breaks = NULL,breaks=log(mm,base=10), 
                                                             #limits=c(-.35 ,log(max(CNVsnow2$log2copyratio) ,base=10)+.35), 
                                                             limits=c( log(min(CNVsnow2$log2copyratio),base=10), 
                                                                      log(max(CNVsnow2$log2copyratio,na.rm=T) , base=10)), 
                                                             labels=as.character(mm)) +
coord_cartesian(xlim=c(min_coord,max_coord),expand = c(0.01,0.01))
}
# expre plot
#  
#  expre_plot = ggplot() +
##    geom_segment(data=genes, aes(x = start, y =log(expre_cancer/expre_normal,base=2) , 
##                                 xend = end, yend = log(expre_cancer/expre_normal,base=2),colour=type),size=5) + 
#    common_ggplot2 + 
#    # add genes
#    #geom_text(data=genes,aes(x=(start+end)/2,y=log(expre_cancer/expre_normal,base=2) + 0.43,label=name),size=5,angle = 0,fontface=3) +
#    geom_text_repel(min.segment.length = unit(0.05, "lines"),
#                    point.padding = unit(1.6, 'lines'),arrow = arrow(length = unit(0.01, 'npc')),
#                    segment.alpha = .5,segment.size=.15,
#                    data=genes,aes(x=(start+end)/2,y=log(expre_cancer/expre_normal,base=2) + 0.43,
#                                                                       label=name,color=type),
#                    size=2.3,fontface="italic") +
#    scale_color_manual(values=c("forestgreen","red1","black","brown")) +

#if (max(CNVsnow$log2copyratio) <=100 & max(CNVsnow$log2copyratio) >10){
#idx = which(CNVsnow$log2copyratio ==0);if(length(idx)>0){CNVsnow$log2copyratio[which(CNVsnow$log2copyratio ==0)]=0.99}
#mm2 = max(CNVsnow$log2copyratio)
#mmin = min(CNVsnow$log2copyratio)
#print(mm2)
#vals = sort( unique(CNVsnow$log2copyratio) )
#mm=vals
#vals=vals[1:(length(vals)-1)]
#if(length(vals)>5){vals=vals[seq(1,length(vals),2)]}
#if(mm2 <=20){mm = c(0,1,2,4,10,mm2)} #sort( unique(CNVsnow$log2copyratio) )} #c(0,1,2,4,10,mm2)}
#if(mm2 <=40 & mmin >20){mm = c(vals,mm2)} #sort( unique(CNVsnow$log2copyratio) )} #mm = c(0,1,2,4,10,20,mm2)}
#if(mm2 <=100 & mmin >40){mm = c(vals,mm2)} #sort( unique(CNVsnow$log2copyratio) )} #mm = c(0,1,2,4,10,20,mm2)}
#if( mm2 >= 40 & mmin <20){mm = c(vals,mm2)} #sort( unique(CNVsnow$log2copyratio) )} #mm = c(0,1,2,4,10,20,mm2)}
##if(mm2 <=100 & mmin <40){mm = c(vals,mm2)} #sort( unique(CNVsnow$log2copyratio) )} #mm = c(0,1,2,4,10,20,mm2)}
#
#CNV_plot = ggplot() +
#geom_segment(data=CNVsnow, aes(x = start, y =log(log2copyratio,base=2) , xend = end, yend = log(log2copyratio,base=2)),size=5) + 
#common_ggplot2 + scale_color_manual(values=c("forestgreen","red1","blue","brown")) +
#ylab("Minor CN")+ xlab(NULL)+ scale_x_continuous(expand = c(0.01,0.01),labels = function(x){paste(x/1000000,"MB")})#,limits=c(min_coord,max_coord))
#
#CNV_plot = CNV_plot + scale_y_continuous(minor_breaks = NULL,breaks=log(mm,base=2), 
# #limits=c(0, max(CNVsnow$log2copyratio)), 
# limits=c( -.01 + log(min(CNVsnow$log2copyratio),base=2), 
#  log(max(CNVsnow$log2copyratio) , base=2)+.01 ), 
# labels=as.character(mm)) +
#coord_cartesian(xlim=c(min_coord,max_coord))
#}
#
#if (max(CNVsnow$log2copyratio) >100){
#idx = which(CNVsnow$log2copyratio ==0);if(length(idx)>0){CNVsnow$log2copyratio[which(CNVsnow$log2copyratio ==0)]=0.99}
#CNV_plot = ggplot() +
#geom_segment(data=CNVsnow, aes(x = start, y =log(log2copyratio,base=10) , xend = end, yend = log(log2copyratio,base=10)),size=5) + 
#common_ggplot2 + scale_color_manual(values=c("forestgreen","red1","blue","brown")) +
#ylab("Minor CN")+ xlab(NULL)+ scale_x_continuous(expand = c(0.01,0.01),labels = function(x){paste(x/1000000,"MB")})#,limits=c(min_coord,max_coord))
#mm2 = max(CNVsnow$log2copyratio)
#mm = c(2,4,20,50,100,mm2)
#CNV_plot = CNV_plot + scale_y_continuous(minor_breaks = NULL,breaks=log(mm,base=10), 
# #limits=c(-.35 ,log(max(CNVsnow$log2copyratio) ,base=10)+.35), 
# limits=c( log(min(CNVsnow$log2copyratio),base=10), 
#  log(max(CNVsnow$log2copyratio) , base=10)), 
# labels=as.character(mm)) +
#coord_cartesian(xlim=c(min_coord,max_coord))
#}


#########################################
#print(CNVsnow$log2copyratio)
common_ggplot2_chrom =  theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.border = element_blank(),
                              plot.background = element_blank(),
                              axis.text=element_blank(),
                              axis.title=element_blank(),
                              plot.title=element_blank(), 
                              axis.ticks=element_blank())


chr_info = readRDS("chr_info.rds")
chr_info$color[chr_info$gieStain == "gneg"] = "white"
chr_info$color[chr_info$gieStain == "gpos25"] = "grey75"
chr_info$color[chr_info$gieStain == "gpos50"] = "grey50"
chr_info$color[chr_info$gieStain == "gpos75"] = "grey25"
chr_info$color[chr_info$gieStain == "gpos100"] = "grey0"
chr_info$color[chr_info$gieStain == "acen"] = "red"

chr_info = chr_info[chr_info$seqnames==chr,]

chr_info$y = rep(1,nrow(chr_info))
#print(chr_info)

if (nrow(chr_info) >6){
    chr_info_annot=chr_info[seq(1,(nrow(chr_info)-2),4),]} else{
        chr_info_annot=chr_info
}

p =ggplot(data=chr_info,aes(x=y,y=y)) +geom_point(colour="white")
p = p +  
    geom_rect(data=chr_info,mapping = aes(xmin = chr_info$start, xmax = chr_info$end,   ymin = 0, ymax = 1),fill = chr_info$color,color="black",size=.1) 

p = p +ylim(0,4)
p = p + annotate(geom = "text",x = (chr_info_annot$start+chr_info_annot$end)/2,y=chr_info_annot$y + 1.3,
                 label=chr_info_annot$name, vjust=.5, angle=90,size=5,hjust=.5)
p = p + theme_bw() + common_ggplot2_chrom +
	#xlim(min_coord,max_coord) + 
  scale_x_continuous(#limits = c(min_coord,max_coord),
					 expand = c(0.01,0.01)) +
coord_cartesian(xlim=c(min_coord, max_coord),expand = c(0.01,0.01))
#scale_x_continuous(expand = c(0.01,0.01), limits=c(min_coord,max_coord))


p$layout$clip[p$layout$name=="panel"] <- "off"


##########################################
return(k=list(p,breakpoints,CNV_plot,CNV_plot_minor))




}
