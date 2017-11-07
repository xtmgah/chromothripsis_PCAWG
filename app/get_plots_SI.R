args <- commandArgs(TRUE)
samples = read.table("../release_may2016.v1.4.tsv",sep="\t",header=T,stringsAsFactors =F)


#dd  = readRDS("/n/data1/hms/dbmi/park/icortes/STR/pancan/chromothripsis_ICGC/candidates_ind_w_inter_info_processed.rds")
dd = readRDS("../cands_final.rds")
print(nrow(dd))

dd$submitter_donor_id <- samples$submitter_donor_id[match(dd$donor_unique_id,samples$donor_unique_id)]
#source("function_for_SI_plots.R")
source("function_for_SI_plots_w_minor.R")
dd$SVs = paste("DEL: ",dd$number_DEL, "; DUP: ",dd$number_DUP,"; h2hINV: ",dd$number_h2hINV,
               ";\nt2tINV: ",dd$number_t2tINV, "; TRA: ",dd$number_TRA,sep="")
dd$position = paste(dd$chrom,":",dd$min_coord,"-",dd$max_coord,sep="")

dd$fragment_joints_FDR = round(dd$fragment_joints_FDR, digits=2)
dd$chr_breakpoint_enrichment_FDR = round(dd$chr_breakpoint_enrichment_FDR, digits=2)

dd$i_i2_max = dd$i_i2_max + 2
dd$i_i2_eval = dd$i_i2_eval + 2
dd$i_i2_max_3states = dd$i_i2_max_3states_new + 2
dd$imax = paste(dd$i_i2_max,", ",dd$i_i2_max_3states_new,sep="")


histo = read.csv("../pcawg_specimen_histology_August2016_v9.tsv",sep="\t",header=T)
dd$histo = as.vector(histo$histology_abbreviation[match(dd$donor_unique_id,histo$donor_unique_id)])

pur_ploidy = read.table("consensus.20170119.purity.ploidy.txt",sep="\t",header=T)
pur_ploidy$sample <- samples$donor_unique_id[match(pur_ploidy$samplename,samples$tumor_wgs_submitter_sample_id)]
pur_ploidy$sample <- samples$donor_unique_id[match(pur_ploidy$samplename,samples$tumor_wgs_aliquot_id)]

dd$purity <- pur_ploidy$purity[match(dd$donor_unique_id,pur_ploidy$sample)]
dd$ploidy <- round(pur_ploidy$ploidy[match(dd$donor_unique_id,pur_ploidy$sample)],digits=2)
dd$pur_plo = paste(dd$purity,dd$ploidy,sep=", ")

calls_all = readRDS("calls_all_with_all_info.rds")
dd$type <- calls_all$type_chromo[match( paste(dd$donor_unique_id,dd$chrom), paste(calls_all[c("donor_unique_id")][,1], calls_all["Chr"][,1] ) ) ]
#print(dd$type)
                    
# order dd by tissue and then donor_id
dd$donor_unique_id = as.vector(dd$donor_unique_id)
dd$histo = as.vector(dd$histo)
dd$chrom = as.vector(dd$chrom)
#library(gtools)
#dd$chrom = mixedsort(dd$chrom)
dd$chrom_numeric = dd$chrom; dd$chrom_numeric[dd$chrom_numeric =="X"] = "23"; dd$chrom_numeric = as.numeric(dd$chrom_numeric)
dd = dd[order(dd$histo, dd$donor_unique_id,dd$chrom_numeric),]
print(dd[,c("histo", "donor_unique_id","chrom")])


cols=c("histo",
       "position",
       "chrom",
       "type",
       "clusterSize",             
       "clusterSize_all",
       "SVs",
       "SVs_sample",
       "imax",                
       #"i_i2_max",                
       #"i_i2_max_3states",   
       "i_i2_eval",    
       "fragment_joints_FDR",
       "chr_breakpoint_enrichment_FDR",
       "inter_other_chroms_coords_all",
       "donor_unique_id",
        "pur_plo")


ind = as.numeric(args[1])
caja =1

for( index in c(ind,ind+1,ind+2,ind+3)){

sub = dd$submitter_donor_id[index]
table_now = dd[index,cols]

col_nams = c("Cancer type","Position", "chr", #"Start","End",
             "Type",
             "Interleaved intrachr. SVs","Total SVs (intrachr. + transl.)",
             "SV types",
             "SVs in sample","Oscillating CN (2 and 3 states)",
             "CN segments", "FDR fragment joints","FDR chr. breakp. enrich.","Linked to chrs","donor_unique_id","Purity, ploidy")
names(table_now) = col_nams
don_id2 = unlist(strsplit(table_now$donor_unique_id, "::"))[2]
table_now$donor_unique_id = NULL
chr = table_now$chr
table_now$chr = NULL
file = paste0("/n/data1/hms/dbmi/park/icortes/STR/pancan/chromothripsis_ICGC/results_shatterSeek_ICGC_lambda_nb_patient_new_CNV_new_SV_uniq_breakpoints2/",sub,".RData")
cat("plotting..\n\n\n")
#--------------------------------------------------------------------------------------
#p1 = plot_chromo(file=file,chr=chr,table_now=table_now, don=don_id2)
#gp1 <- ggplotGrob(p1[[1]] + theme(plot.margin=unit(c(0.1,1,0,0),"cm"))+ theme(text=element_text(size=7)))
#gp2 <- ggplotGrob(p1[[2]]+ theme(plot.margin=unit(c(0,1,0,0), "cm"),axis.text.x=element_blank())) 
#gp3 <- ggplotGrob(p1[[3]] + theme(plot.margin=unit(c(0,1,0,0), "cm"),legend.position="none")+theme(axis.text.x=element_text(size=7)))
#assign(paste0("gp4_",caja), p1[[4]] )
#gp3$widths <- gp2$widths
#gp1$widths <- gp2$widths
#assign(paste0("plot_",caja) , arrangeGrob(gp1,gp2,gp3,nrow=3,ncol=1,heights=c(0.4,.8,.8))  )

p1 = plot_chromo(file=file,chr=chr,table_now=table_now, don=don_id2)
gp1 <- ggplotGrob(p1[[1]] + theme(plot.margin=unit(c(0,1,0,0),"cm"))+ theme(text=element_text(size=7)))
gp2 <- ggplotGrob(p1[[2]]+ theme(plot.margin=unit(c(0,1,0,0), "cm"),axis.text.x=element_blank()))
gp3 <- ggplotGrob(p1[[3]] + theme(plot.margin=unit(c(0,1,0,0), "cm"),legend.position="none")+theme(axis.text.x=element_blank())) #text(size=7)))
assign(paste0("gp4_",caja), p1[[4]] )
gp5 <- ggplotGrob(p1[[5]] + theme(plot.margin=unit(c(0,1,0,0), "cm"),legend.position="none")+theme(axis.text.x=element_text(size=7)))
gp3$widths <- gp2$widths
gp1$widths <- gp2$widths
gp5$widths <- gp2$widths
assign(paste0("plot_",caja) , arrangeGrob(gp1,gp2,gp3,gp5,nrow=4,ncol=1,heights=c(0.2,.4,.55,.25))  )

#--------------------------------------------------------------------------------------
caja=caja+1
}

ind = as.numeric(args[1])
label = seq(1,1443,4)
label = which(label == ind)
outname = paste0("./plots_SI_w_minor/plot_",label,"_",paste(ind,ind+1,ind+2,ind+3,sep="_"),".pdf")
#outname = paste0("./plots_SI/",label,"_",paste(ind,ind+1,ind+2,ind+3,sep="_"),".pdf")
pdf(outname,width=(19.3/2.4), height=(29.7/2.4) )
#pdf(outname,width=(18.9/2.4), height=(21/2.4) )
grid.arrange(
             arrangeGrob(plot_1,gp4_1,nrow=2,heights=c(0.6,.4)),
             arrangeGrob(plot_2,gp4_2,nrow=2,heights=c(0.6,.4)),
             arrangeGrob(plot_3,gp4_3,nrow=2,heights=c(0.6,.4)),
             arrangeGrob(plot_4,gp4_4,nrow=2,heights=c(0.6,.4)),
             nrow=2,ncol=2,widths=c(2,2))
dev.off()
dev.off()

