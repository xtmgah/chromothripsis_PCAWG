##samples = read.table("release_may2016.v1.4.tsv",sep="\t",header=T,stringsAsFactors =F)
##
##dd = readRDS("cands_final.rds")
### include low confidence
##d = dd
##
##
##dd$submitter_donor_id <- samples$submitter_donor_id[match(dd$donor_unique_id,samples$donor_unique_id)]
##dd$icgc_donor_id <- samples$icgc_donor_id[match(dd$donor_unique_id,samples$donor_unique_id)]
##dd$SVs = paste("DEL: ",dd$number_DEL, "; DUP: ",dd$number_DUP,"; h2hINV: ",dd$number_h2hINV,";\nt2tINV: ",dd$number_t2tINV, "; TRA: ",dd$number_TRA,sep="")
##dd$position = paste(dd$chrom,":",dd$min_coord,"-",dd$max_coord,sep="")
##dd$fragment_joints_FDR = round(dd$fragment_joints_FDR, digits=2)
##dd$chr_breakpoint_enrichment_FDR = round(dd$chr_breakpoint_enrichment_FDR, digits=2)
##dd$i_i2_max = dd$i_i2_max + 2
##dd$i_i2_eval = dd$i_i2_eval + 2
##dd$i_i2_max_3states = dd$i_i2_max_3states_new + 2
##dd$imax = paste(dd$i_i2_max,", ",dd$i_i2_max_3states_new,sep="")
##
##histo = read.csv("pcawg_specimen_histology_August2016_v9.tsv",sep="\t",header=T)
##dd$histo = as.vector(histo$histology_abbreviation[match(dd$donor_unique_id,histo$donor_unique_id)])
##
##pur_ploidy = read.table("consensus.20170119.purity.ploidy.txt",sep="\t",header=T)
##pur_ploidy$sample <- samples$donor_unique_id[match(pur_ploidy$samplename,samples$tumor_wgs_submitter_sample_id)]
##pur_ploidy$sample <- samples$donor_unique_id[match(pur_ploidy$samplename,samples$tumor_wgs_aliquot_id)]
##
##dd$purity <- pur_ploidy$purity[match(dd$donor_unique_id,pur_ploidy$sample)]
##dd$ploidy <- round(pur_ploidy$ploidy[match(dd$donor_unique_id,pur_ploidy$sample)],digits=2)
##dd$pur_plo = paste(dd$purity,dd$ploidy,sep=", ")
##
##calls_all = readRDS("calls_all_with_all_info.rds")
##dd$type <- calls_all$type_chromo[match( paste(dd$donor_unique_id,dd$chrom), paste(calls_all[c("donor_unique_id")][,1], calls_all["Chr"][,1] ) ) ]
##
### order dd by tissue and then donor_id
##dd$donor_unique_id = as.vector(dd$donor_unique_id)
##dd$histo = as.vector(dd$histo)
##dd$chrom = as.vector(dd$chrom)
##dd$chrom_numeric = dd$chrom; dd$chrom_numeric[dd$chrom_numeric =="X"] = "23"; dd$chrom_numeric = as.numeric(dd$chrom_numeric)
##dd = dd[order(dd$histo, dd$donor_unique_id,dd$chrom_numeric),]
##
##d = dd
##
###print(head(d))

calls_all = readRDS("calls_all_with_all_info_server.rds")
#calls_all = readRDS("calls_all_with_all_info.rds")
d = calls_all
d$SV_sample = d['SV calls file']
d$SV_sample = as.vector(unlist(d$SV_sample))
d$CNV = d['SCNA calls file']
d$CNV = as.vector(unlist(d$CNV))

#names(d)[which(names(d) == "stage_backup")] = "tumor_stage"
#names(d)[3] = "Start coordinate for SV cluster in chr"
#names(d)[4] = "End coordinate for SV cluster in chr"
#names(d)[5] = "Nb. intrachromosomal SVs"
#names(d)[6] = "Total Nb. SVs (intrachr. + interchr.)"
#names(d)[7] = "Nb. SVs in sample"

#print(d$SV_sample)

for (i in 1:ncol(d)){

if (is.numeric(d[,i])){

d[,i] = round(d[,i],digits=3)

}

}

