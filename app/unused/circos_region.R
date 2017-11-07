#----------------------------------------------------------------------------------------------------
library(circlize)
# change this function to increase label sizes
circos.genomicInitialize <- function (data, sector.names = NULL, major.by = NULL, plotType = c("axis", "labels"), tickLabelsStartFromZero = TRUE, track.height = 0.05,  ...) 
{
  if (is.factor(data[[1]])) {
    fa = levels(data[[1]])
  }
  else {
    fa = unique(data[[1]])
  }
  if (!is.null(sector.names)) {
    if (length(sector.names) != length(fa)) {
      stop("length of `sector.names` and length of sectors differ.\n")
    }
  }
  else {
    sector.names = fa
  }
  names(sector.names) = fa
  x1 = tapply(data[[2]], data[[1]], min)[fa]
  x2 = tapply(data[[3]], data[[1]], max)[fa]
  op = circos.par("cell.padding")
  ow = circos.par("points.overflow.warning")
  circos.par(cell.padding = c(0, 0, 0, 0), points.overflow.warning = FALSE)
  circos.initialize(factor(fa, levels = fa), xlim = cbind(x1, 
                                                          x2), ...)
  if (any(plotType %in% c("axis", "labels"))) {
    circos.genomicTrackPlotRegion(data, ylim = c(0, 1), bg.border = NA, 
                                  track.height = track.height, panel.fun = function(region, 
                                                                                    value, ...) {
                                    sector.index = get.cell.meta.data("sector.index")
                                    xlim = get.cell.meta.data("xlim")
                                    if (tickLabelsStartFromZero) {
                                      offset = xlim[1]
                                      if (is.null(major.by)) {
                                        xlim = get.cell.meta.data("xlim")
                                        major.by = .default.major.by()
                                      }
                                      major.at = seq(xlim[1], xlim[2], by = major.by)
                                      major.at = c(major.at, major.at[length(major.at)] + 
                                                     major.by)
                                      if (major.by > 1e+06) {
                                        major.tick.labels = paste((major.at - offset)/1e+06, 
                                                                  "MB", sep = "")
                                      }
                                      else if (major.by > 1000) {
                                        major.tick.labels = paste((major.at - offset)/1000000,  #XX
                                                                  "MB", sep = "")
                                      }
                                      else {
                                        major.tick.labels = paste((major.at - offset), 
                                                                  "bp", sep = "")
                                      }
                                    }
                                    else {
                                      if (is.null(major.by)) {
                                        xlim = get.cell.meta.data("xlim")
                                        major.by = .default.major.by()
                                      }
                                      major.at = seq(floor(xlim[1]/major.by) * major.by, 
                                                     xlim[2], by = major.by)
                                      major.at = c(major.at, major.at[length(major.at)] + 
                                                     major.by)
                                      if (major.by > 1e+06) {
                                        major.tick.labels = paste(major.at/1e+06, 
                                                                  "MB", sep = "")
                                      }
                                      else if (major.by > 1000) {
                                        major.tick.labels = paste(major.at/1000000,  #XX
                                                                  "MB", sep = "")
                                      }
                                      else {
                                        major.tick.labels = paste(major.at, "bp", 
                                                                  sep = "")
                                      }
                                    }
                                    if (all(c("axis", "labels") %in% plotType)) {
                                      circos.axis(h = 0, major.at = major.at, labels = major.tick.labels, 
                                                  labels.cex = 0.8 * par("cex"), labels.facing = "clockwise", 
                                                  major.tick.percentage = 0.2)
                                      circos.text(mean(xlim)+12, 1.2, labels = sector.names[sector.index], 
                                                  cex = par("cex")*.8, adj = c(1.8, -3.5), niceFacing = TRUE)
                                    }
                                    else if ("labels" %in% plotType) {
                                      circos.text(mean(xlim), 0, labels = sector.names[sector.index], 
                                                  cex = par("cex"), adj = c(1, 0), niceFacing = TRUE)
                                    }
                                    else if ("axis" %in% plotType) {
                                      circos.axis(h = 0, major.at = major.at, labels = major.tick.labels, 
                                                  labels.cex = 0.3 * par("cex"), labels.facing = "clockwise", 
                                                  major.tick.percentage = 0.2)
                                    }
                                  })
  }
  circos.par(cell.padding = op, points.overflow.warning = ow)
  return(invisible(NULL))
}
#--------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------


# circos plor for chromothripsis
#setwd("/Users/icortes/Dropbox/Park_lab/paper_chromothripsis")


genes= readRDS("/Users/icortes/Dropbox/Park_lab/paper_chromothripsis/genes_hg19_processed.rds")


circos_plot = function(bedpe, CNV){

load("/Users/icortes/Dropbox/Park_lab/paper_chromothripsis/40e17bf8-6c19-4e5e-b60d-ad54f504a970.RData")


chrs= c("chr9","chr22")
chrs_only= c("9","22")

SV_sample="/Users/icortes/Dropbox/Park_lab/paper_chromothripsis/88f2c496-3eb4-4c03-b3a5-8d5a55803aaf.pcawg_consensus_1.6.161022.somatic.sv.bedpe"
sv = read.table(SV_sample,header=T)
sv$svclass[which(sv$svclass == "TRA" & sv$strand1 == "+" & sv$strand2 == "-")] = "DEL"
sv$svclass[which(sv$svclass == "TRA" & sv$strand1 == "+" & sv$strand2 == "+")] = "h2hINV"
sv$svclass[which(sv$svclass == "TRA" & sv$strand1 == "-" & sv$strand2 == "-")] = "t2tINV"
sv$svclass[which(sv$svclass == "TRA" & sv$strand1 == "-" & sv$strand2 == "+")] = "DUP"
sv$svclass = factor(sv$svclass)
colors.all =c("darkorange1","blue1","forestgreen","blue")

segSample = read.csv("/Users/icortes/Dropbox/Park_lab/paper_chromothripsis/88f2c496-3eb4-4c03-b3a5-8d5a55803aaf.consensus.20170119.somatic.cna.annotated.txt",header=T,sep="\t",stringsAsFactors=F)

segSample$chromosome <- gsub("chr","",segSample$chromosome)
segSample <- segSample[which(segSample$chromosome != "Y"),]

#segSample$total_cn[segSample$total_cn >=5] = 5
########
dd <- segSample[,c(1,2,3,4)]
dd$total_cn[dd$total_cn == 0] <- 15000
dd$total_cn[is.na(dd$total_cn)] <- 0
library(GenomicRanges)
dd <- as(dd,"GRanges")
cov <- coverage(dd,weight = dd$total_cn)
dd1 <- as(cov,"GRanges")
dd1 <- as.data.frame(dd1)
dd1 <- dd1[dd1$score !=0,]
head(dd1)
dd1 = dd1[,c(1,2,3,6)]
names(dd1) <- names(segSample)[1:4]
dd1$total_cn[dd1$total_cn == 15000] <- 0
segSample= dd1


### process SVs
a = sv[,1:3]
names(a)=c("chr","start","end")
a$chr = paste("chr",a$chr,sep="")
b = sv[,4:6]
names(b)=c("chr","start","end")
b$chr = paste("chr",b$chr,sep="")
bed1 = generateRandomBed(nr = 100)

colors.all =c("darkorange1","blue1","forestgreen","blue")
names(colors.all) = c("DEL","DUP","h2hINV","t2tINV")

idx = which(a$chr %in% chrs & b$chr %in% chrs)
a1=a[idx,]
b1=b[idx,]
class_now=sv$svclass[idx]
######################

pepe=segSample[,1:4]
names(pepe) = names(bed1)
pepe$chr = paste("chr",pepe$chr,sep="")
pepe=pepe[which(pepe$chr %in% chrs),]
names(pepe)[4] ="value"


cytoband = read.cytoband()
df = cytoband$df
head(df)
chromosome = c("chr9","chr22") #cytoband$chromosome
df = df[df$V1 %in% chromosome,]
# copy regions for the two zoomed chromosomes
zoom_df = df[df[[1]] %in% chromosome[1:2], ] 
zoom_df[[1]] = paste0("zoom_", zoom_df[[1]]) 

# 36 - 46
zoom_df3 = zoom_df[which(zoom_df$V1 == "zoom_chr9" & zoom_df$V3 > 32139979 & zoom_df$V3 <= 140579391 ),]  #23960144
zoom_df3$V2 = 32139979
zoom_df3$V3 = 140579391
#-----------
zoom_df8 = zoom_df[which(zoom_df$V1 == "zoom_chr22"  &  zoom_df$V2 >= 29683567 & zoom_df$V3 <= 49998524),]  # 21 28
zoom_df8$V2 = 29683567
zoom_df8$V3 = 49998524
zoom_df8b = zoom_df[which(zoom_df$V1 == "zoom_chr22"  &  zoom_df$V2 >= 29683567 & zoom_df$V3 <= 49998524),]  #133851895
zoom_df8b$V1  = paste0(zoom_df8b$V1,"_b")
#zoom_df8b$V2 = 128400000
#zoom_df8b$V3 = 128800000

df2 = rbind(zoom_df3, zoom_df8,zoom_df8b)


cnv = chromthripsis_ICGC@detail$CNV
cnv = cnv[cnv$chrom %in% c(9,22),]
#cnv = cnv[cnv$log2copyratio ==49,]

###


pepe2=pepe
pepe2$chr = paste0("zoom_",pepe2$chr)

# 
# 22:29683567−49998524
# 9:32139979−140579391

pepe_df3 = pepe2[which(pepe2$chr == "zoom_chr9" & pepe2$start >= 32139979 & pepe2$end <= 140579391),]  
pepe2$start = 32139979
pepe2$end = 140579391
pepe_df8 = pepe2[which(pepe2$chr == "zoom_chr22" & pepe2$start >= 29683567 & pepe2$end <= 49998524),]  
pepe2$start = 29683567
pepe2$end = 49998524
#pepe_df8b = pepe2[which(pepe2$chr == "zoom_chr22"& pepe2$start >= 29683567 & pepe2$end <= 49998524),]  #23960144
#pepe_df8b$chr = paste0(pepe_df8b$chr,"_b")

pepe3 = rbind(pepe_df3,pepe_df8)#,pepe_df8b)


####-----------------------------------------------------------------------------------------------------------------------------
### gene names
####-----------------------------------------------------------------------------------------------------------------------------





#----------------------------------------------------------------------------------------
# Initialize
#----------------------------------------------------------------------------------------
circos.clear()

par(mar = c(1, 1, 1, 1))
sector.width = c(0.7,0.3)#,.3)
circos.par(start.degree = 90,"gap.degree" = 8)

lo=df2[which(df2$V1 %in% c("zoom_chr22","zoom_chr9")),]

circos.genomicInitialize(lo, sector.width = sector.width,major.by = 5000000,sector.names = c("chr9","chr22"),
                         tickLabelsStartFromZero = F)

#----------------------------------------------------------------------------------------
# CN
#----------------------------------------------------------------------------------------
par("lend"="butt")
circos.genomicTrackPlotRegion(pepe3,track.height = 0.25,
                              panel.fun = function(region, value, ...) {                       
                                circos.genomicLines(region, value, type = "segment", lwd = 6,lend='butt',
                                                    col="black", ...)
                                circos.yaxis(at=c(1,2,3,4),labels.cex=1,labels=c("1","2","3","4"))
                              })

#----------------------------------------------------------------------------------------
# add SVs
#----------------------------------------------------------------------------------------
a2=a1
a2$chr = paste0("zoom_",a2$chr)
b2=b1
b2$chr = paste0("zoom_",b2$chr)
circos.genomicLink(a2,b2,col=colors.all[match(class_now,names(colors.all))],lwd=1.5)
}



