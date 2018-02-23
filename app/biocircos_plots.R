# load package w/o installing
dev_tools::load_all('/srv/shinyapps/app/BioCircos')
nucle = c("A","C","G","T")
nucle= expand.grid(nucle,nucle)
nucle = nucle[which(nucle$Var1 != nucle$Var2),]
nucle = paste(nucle$Var1,">",nucle$Var2,sep="")
cols_muts = c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f",
			  "#ff7f00","#cab2d6", "#ffff33","#a65628","#999999")
nucle = data.frame(mut_type=nucle,
				   color=as.vector(cols_muts))

genome = list("1" = 249250621, #Hg19
			  "2" = 243199373,
			  "3" = 198022430,
			  "4" = 191154276,
			  "5" = 180915260,
			  "6" = 171115067,
			  "7" = 159138663,
			  "8" = 146364022,
			  "9" = 141213431,
			  "10" = 135534747,
			  "11" = 135006516,
			  "12" = 133851895,
			  "13" = 115169878,
			  "14" = 107349540,
			  "15" = 102531392,
			  "16" = 90354753,
			  "17" = 81195210,
			  "18" = 78077248,
			  "19" = 59128983,
			  "20" = 63025520,
			  "21" = 48129895,
			  "22" = 51304566,
			  "X" = 155270560)
#----------------------------------------------------------
plot_biocirc = function(donor,input){

	chrs_selection = input$chr_selection_circos
	if (length(input$chr_selection_circos)==0){chrs_selection=c("22")}
	chr_info = readRDS("chr_info_processed.rds")
	# chrs selection
	myGenome = genome[which(names(genome) %in% chrs_selection)]
	d_now = d[d$donor_unique_id == donor,]
	d_now = d_now[which(d_now$Chr %in% chrs_selection),]
	chr_info_now = chr_info[which(chr_info$seqnames %in% chrs_selection), ]
	## karyotype
	tracks = BioCircosArcTrack(maxRadius = 1.14,minRadius = 1,
							   "karyop",chromosomes = chr_info_now$seqnames,
							   starts = chr_info_now$start,
							   labels= as.character(chr_info_now$name),
							   ends=chr_info_now$end,
							   colors=chr_info_now$color)

	#----------------------------------------------------------
	# get range to show for indels and SNVs
	#----------------------------------------------------------
	indels_nopatho=NULL; indels_patho=NULL
	nopatho=NULL; patho=NULL

	# get the rage for the interdistance mutations
	if(input$show_patho_indels == TRUE | input$show_nopatho_indels == TRUE | input$show_patho == TRUE | input$show_nopatho == TRUE){
		# get the correct range
		indel_file = paste0("./rds_processed_indels/",donor,"_nopatho.rds")
		indels_nopatho = tryCatch( readRDS(indel_file) ,error=function(e) NULL)
		if (!is.null(indels_nopatho)){
			idxnow = which(indels_nopatho$Chr %in% chrs_selection &  indels_nopatho$dista <input$range_dist[2])
			if(length(idxnow)>0){
				indels_nopatho=indels_nopatho[idxnow,]
				if (!is.null(indels_nopatho)){
					if (nrow(indels_nopatho)>1){
						range_indels_no_patho = c(min(indels_nopatho$dista,na.rm = T), max(indels_nopatho$dista,na.rm = T))

					}else{range_indels_no_patho=c(NA,NA)}
				}else{range_indels_no_patho=c(NA,NA)}
			}else{range_indels_no_patho=c(NA,NA)}
		}else{range_indels_no_patho=c(NA,NA)}

		indel_file = paste0("./rds_processed_indels/",donor,"_patho.rds")
		indels_patho = tryCatch( readRDS(indel_file) ,error=function(e) NULL)
		if (!is.null(indels_patho)){
			idxnow=which(indels_patho$Chr %in% chrs_selection &  indels_patho$dista <input$range_dist[2])
			if(length(idxnow)>0){
				indels_patho=indels_patho[idxnow,]
				if (!is.null(indels_patho)){
					if (nrow(indels_patho)>1){

						range_indels_patho = c(min(indels_patho$dista,na.rm = T), max(indels_patho$dista,na.rm = T))

					}else{range_indels_patho=c(NA,NA)}
				}else{range_indels_patho=c(NA,NA)}

			}else{range_indels_patho=c(NA,NA)}
		}else{range_indels_patho=c(NA,NA)}

		range_indels = c( max(c(range_indels_patho[2],range_indels_no_patho[2]),na.rm=T),0)
		#-min(c(range_indels_patho[1],range_indels_no_patho[1]),na.rm=T))

		# get the correct range
		nopatho_file = paste0("./rds_processed/",donor,"_nopatho.rds")
		nopatho = tryCatch( readRDS(nopatho_file) ,error=function(e) NULL)
		if (!is.null(nopatho)){
			idxnow = which(nopatho$Chr %in% chrs_selection  & nopatho$dista <input$range_dist[2])
			if (length(idxnow) > 0){
				nopatho=nopatho[idxnow,]
				if (!is.null(nopatho)){
					if (nrow(nopatho)>1){
						range_snvs_no_patho = c(min(nopatho$dista), max(nopatho$dista))

					}else{range_snvs_no_patho=c(NA,NA)}
				}else{range_snvs_no_patho=c(NA,NA)}
			}else{range_snvs_no_patho=c(NA,NA)}
		}else{range_snvs_no_patho=c(NA,NA)}

		patho_file = paste0("./rds_processed/",donor,"_patho.rds")
		patho = tryCatch( readRDS(patho_file) ,error=function(e) NULL)
		if (!is.null(patho)){
			idxnow = which(patho$Chr %in% chrs_selection  & patho$dista <input$range_dist[2])
			if (length(idxnow)>0){
				patho=patho[idxnow,]
				if (!is.null(patho)){
					if (nrow(patho)>1){

						range_patho = c(-max(patho$dista),0)#min(patho$dista), 

					} else{range_patho=c(NA,NA)}
				}else{range_patho=c(NA,NA)}
			}else{range_patho=c(NA,NA)}
		}else{range_patho=c(NA,NA)}

		range_snvs = c(
					   max(c(range_patho[2],range_snvs_no_patho[2]),na.rm=T),0)
		# -min(c(range_patho[1],range_snvs_no_patho[1]),na.rm=T)

		#)
		range_snvs = c(#-input$range_dist,
					   #-max(
					   #-3000000,
					   -1 * as.numeric(input$range_dist[2]),
					   # c(range_snvs[1],range_indels[1]),na.rm=T),
					   0)
		range_indels=range_snvs
	}
	#----------------------------------------------------------
	# INDELS
	#----------------------------------------------------------
	## NONPATHOGENIC INDELS
	if(input$show_nopatho_indels == TRUE){
		if (!is.null(indels_nopatho)){
			idxnow=which(indels_nopatho$Chr %in% chrs_selection & indels_nopatho$dista <input$range_dist[2])

			if(length(idxnow)>=1){
				indels_nopatho = indels_nopatho[idxnow,]
				tracks = tracks +  BioCircosSNPTrack("indels_nopatho",
													 SNPMouseOverTooltipsHtml04=indels_nopatho$dista,
													 maxRadius=0.99, minRadius=0.86,
													 values=-indels_nopatho$dista,
													 opacities=.8,
													 chromosomes = indels_nopatho$Chr,
													 positions=indels_nopatho$Start,
													 labels=indels_nopatho$label, colors='black',size=2,
													 range=range_indels)
			}
		} 
		else{

			indel_file = paste0("./rds_processed_indels/",donor,"_nopatho.rds")
			indels_nopatho = tryCatch( readRDS(indel_file) ,error=function(e) NULL)
			if (!is.null(indels_nopatho)){
				idxnow = which(indels_nopatho$Chr %in% chrs_selection & indels_nopatho$dista <input$range_dist[2])
				if(length(idxnow)>0){
					indels_nopatho=indels_nopatho[idxnow,]
					if (!is.null(indels_nopatho)){
						if (nrow(indels_nopatho)>=1){

							tracks = tracks +  BioCircosSNPTrack("indels_nopatho",
																 maxRadius=0.99, minRadius=0.86,
																 values=-indels_nopatho$dista,
																 opacities=.8,
																 chromosomes = indels_nopatho$Chr,
																 positions=indels_nopatho$Start,
																 labels=indels_nopatho$label, colors='black',size=2,
																 range=range_indels)
						}
					}
				}
			}
		}
	}


	##### PATHOGENIC
	if(input$show_patho_indels == TRUE){
		if (!is.null(indels_patho)){
			idxnow=which(indels_patho$Chr %in% chrs_selection  & indels_patho$dista <input$range_dist[2])

			if(length(idxnow)>=1){
				indels_patho = indels_patho[idxnow,]
				tracks = tracks +  BioCircosSNPTrack("indels_patho",
													 maxRadius=0.99, minRadius=0.86,
													 values=-indels_patho$dista,
													 opacities=.8,
													 chromosomes = indels_patho$Chr,
													 positions=indels_patho$Start,
													 labels=indels_patho$label, colors='#31a354',size=6,
													 range=range_indels)
			}
		} else{

			indel_file = paste0("./rds_processed_indels/",donor,"_patho.rds")
			indels_patho = tryCatch( readRDS(indel_file) ,error=function(e) NULL)
			if (!is.null(indels_patho)){
				idxnow=which(indels_patho$Chr %in% chrs_selection & indels_patho$dista <input$range_dist[2])
				if(length(idxnow)>0){
					indels_patho=indels_patho[idxnow,]
					if (!is.null(indels_patho)){
						if (nrow(indels_patho)>=1){

							tracks = tracks +  BioCircosSNPTrack("indels_patho",
																 maxRadius=0.99, minRadius=0.86,
																 values=-indels_patho$dista,
																 opacities=.8,
																 chromosomes = indels_patho$Chr,
																 positions=indels_patho$Start,
																 labels=indels_patho$label, colors='#31a354',size=6,
																 range=range_indels)


						}
					}
				}
			}
		}
	}




	#----------------------------------------------------------
	# load SNVs 
	#----------------------------------------------------------
	if(input$show_nopatho == TRUE){

		if(!is.null(nopatho)){

			snv_file_nopatho = paste0("./rds_processed/",donor,"_nopatho.rds")
			nopatho = tryCatch( readRDS(snv_file_nopatho) ,error=function(e) NULL)

			if (!is.null(nopatho)){
				idxnow = which(nopatho$Chr %in% chrs_selection  & nopatho$dista <input$range_dist[2])
				if (length(idxnow) > 0){
					nopatho=nopatho[idxnow,]
					if (!is.null(nopatho)){
						if (nrow(nopatho)>=1){

							tracks = tracks +  BioCircosSNPTrack("snvs_nopatho",
																 maxRadius=0.99, minRadius=0.86,
																 values=-nopatho$dista,
																 opacities=.8,
																 chromosomes = nopatho$Chr,
																 positions=nopatho$Start,
																 labels=nopatho$label, colors=nopatho$col_mut, size = 2,
																 range=range_snvs)
						}
					}
				}
			}
		} else{
			idxnow=which(nopatho$Chr %in% chrs_selection   & nopatho$dista <input$range_dist[2])

			if(length(idxnow)>=1){
				nopatho = nopatho[idxnow,]
				tracks = tracks +  BioCircosSNPTrack("snvs_nopatho",
													 maxRadius=0.99, minRadius=0.86,
													 values=-nopatho$dista,
													 opacities=.8,
													 chromosomes = nopatho$Chr,
													 positions=nopatho$Start,
													 labels=nopatho$label, colors=nopatho$col_mut,size=2,
													 range=range_snvs)

			}
		}
	}



	######  pathogenic SNVs    
	if(input$show_patho == TRUE){

		if (is.null(patho)){
			snv_file_patho = paste0("./rds_processed/",donor,"_patho.rds")
			patho = tryCatch( readRDS(snv_file_patho) ,error=function(e) NULL)

			if (!is.null(patho)){
				idxnow = which(patho$Chr %in% chrs_selection  & patho$dista <input$range_dist[2])
				if (length(idxnow)>0){
					patho=patho[idxnow,]
					if (!is.null(patho)){
						if (nrow(patho)>=1){
							if (is.null(range_snvs_no_patho)){
								tracks = tracks +  BioCircosSNPTrack("snvs_patho",
																	 maxRadius=0.99, minRadius=0.86,
																	 opacities=.8,
																	 values=-patho$dista,
																	 chromosomes = patho$Chr,
																	 positions=patho$Start,
																	 labels=patho$label, colors='#377eb8',size = 6,
																	 range=range_snvs)
						}}
					}
				}
			}
		}else{
			idxnow=which(patho$Chr %in% chrs_selection  & patho$dista <input$range_dist[2])

			if(length(idxnow)>=1){
				patho = patho[idxnow,]
				tracks = tracks +  BioCircosSNPTrack("snvs_patho",
													 maxRadius=0.99, minRadius=0.86,
													 opacities=.8,
													 values=-patho$dista,
													 chromosomes = patho$Chr,
													 positions=patho$Start,
													 labels=patho$label, colors='#377eb8',size = 6,
													 range=range_snvs)
			}
		}
	}

	#----------------------------------------------------------
	# load CNV data
	#----------------------------------------------------------
	# XXXXXXX IMPORTANT NOTE
	# to make the CN plot in correct orientiation (that is, the CN gets higher as you move away from the center)
	# I added a "-" (minus sign) to line 1809 of the file: BioCircos/inst/htmlwidgets/lib/biocircos-1.1.2.js
	#"cnv_deviation: -(v.value-cnv_value_maxmi...."
	# Now, the range of the CN needs to be inverted around zero
	cnv_now = read.table(d_now$CNV[1],sep="\t",header=T,stringsAsFactors = F)[,1:6]
	cnv_now = cnv_now[!(is.na(cnv_now$total_cn)),]
	cnv_now = cnv_now[cnv_now$chromosome %in% chrs_selection,]
	tracks = tracks + BioCircosCNVTrack('total_cnv_track', maxRadius = .82,minRadius = 0.55,
										chromosomes = cnv_now$chromosome, starts = cnv_now$start, 
										ends=cnv_now$end, 
										values = cnv_now$total_cn, color = "#C0000",
										width=4, range=c(-(max(cnv_now$total_cn,na.rm=T))-1,0))

	tracks = tracks + BioCircosBackgroundTrack("testBGtrack1", minRadius = 0.85, maxRadius = 0.55,
											   borderColors = "black", borderSize = .1)    
	#### chromothripsis track below the CN track
	if (input$show_chromo_track){
		idxx = which(d_now$type_chromo != "No chromothripsis")
		if(length(idxx)>0){
			pepe = d_now[idxx,]  

			tracks = tracks + BioCircosArcTrack('chromo_track', maxRadius = .85,minRadius = 0.84,
												# chromosomes = cnv_now$chromosome, starts = cnv_now$start, 
												# ends=cnv_now$end, 
												# values = cnv_now$total_cn, 
												chromosomes = as.vector(pepe$Chr),
												starts = c(pepe$Start),
												ends=c(pepe$End),
												values = 1,
												color = "#eded2a", ##abd9e9",
												width=.4,range=c(.8,1.2))
		}
	}

	## minor
	minor_loh = cnv_now[which(cnv_now$minor_cn == 0),]
	minor_no_loh = cnv_now[which(cnv_now$minor_cn != 0),]
	# minor_loh
	tracks = tracks + BioCircosCNVTrack('minor_cnv_track', maxRadius = .54,minRadius = 0.45,
										chromosomes = minor_loh$chromosome, starts = minor_loh$start, 
										ends=minor_loh$end, values = minor_loh$minor_cn, color = "red",
										width=4,
										range=c(-(max(cnv_now$minor_cn,na.rm=T))-1,0))

	# minor no loh
	tracks = tracks + BioCircosCNVTrack('minor_cnv_track', maxRadius = .54,minRadius = 0.45,
										chromosomes = minor_no_loh$chromosome, starts = minor_no_loh$start, 
										ends=minor_no_loh$end, values = minor_no_loh$minor_cn, color = "black",
										width=4,
										range=c(-(max(cnv_now$minor_cn,na.rm=T))-1,0))


	tracks = tracks + BioCircosBackgroundTrack("testBGtrack2", minRadius = 0.45, maxRadius = 0.54,
											   fillColors = "#ede8e8",borderColors =  "black", borderSize = 0.1) 
	if(input$show_genes == TRUE){

		genes = readRDS("genes_processed.rds")
		genes = genes[which(genes$chr %in% chrs_selection),]
		genes_tsg = genes[which(genes$type %in% c("tsg","repair")),]
		genes_tsg$val = 1
		genes_cdg = genes[which(genes$type %in% c("cdg")),]
		genes_cdg$val = 0
		genes_tsg = rbind(genes_tsg,genes_cdg)

		tracks = tracks + BioCircosHeatmapTrack('genes',
												maxRadius = 0.4,
												minRadius = 0.37,
												chromosomes = genes_tsg$chr, starts = genes_tsg$s,
												ends=genes_tsg$e, values = genes_tsg$val, 
												labels=paste(genes_tsg$name,genes_tsg$strand,sep="; "),
												color = c("red", "blue"))

	}

	#----------------------------------------------------------
	# load SVs
	#----------------------------------------------------------
	sv_now = read.table(paste0("./bedpes/",d_now$SV_sample[1]),sep="\t",header=T,stringsAsFactors = F)
	# map colours
	sv_now = sv_now[which(sv_now$chrom1 %in% chrs_selection & sv_now$chrom2 %in% chrs_selection), ]
	if (nrow(sv_now)!=0){
		sv_now$colour = ""
		sv_now$colour[ which(sv_now$strand1 == "+" & sv_now$strand2 == "-")] = "orange"
		sv_now$colour[ which(sv_now$strand1 == "-" & sv_now$strand2 == "+")] = "blue1"
		sv_now$colour[ which(sv_now$strand1 == "+" & sv_now$strand2 == "+")] = "black"
		sv_now$colour[ which(sv_now$strand1 == "-" & sv_now$strand2 == "-")] = "forestgreen"
		sv_now$label = paste(sv_now$chrom1,":",sv_now$start1,"-",sv_now$chrom2,":",sv_now$start2,sep="")
	}

	# one track per colour
	caja=0
	sv_now_dup = sv_now[which(sv_now$colour == "blue1"),]
	if (nrow(sv_now_dup)!=0){
		tracks = tracks + BioCircosLinkTrack("Links", gene1Chromosomes = sv_now_dup$chrom1,
											 gene1Starts = sv_now_dup$start1,
											 gene1Ends = sv_now_dup$end1,
											 gene2Chromosomes = sv_now_dup$chrom2,
											 gene2Starts = sv_now_dup$start2,
											 gene2Ends = sv_now_dup$end2,
											 axisPadding = 0,
											 labels=sv_now_dup$label,
											 color = "blue",
											 displayLabel = F,
											 maxRadius = 0.4)#, minRadus=0.5)
		caja=1}
	sv_now_t2tinv = sv_now[which(sv_now$colour == "black"),]
	if (nrow(sv_now_t2tinv)!=0){
		tracks = tracks + BioCircosLinkTrack("Links", gene1Chromosomes = sv_now_t2tinv$chrom1,
											 gene1Starts = sv_now_t2tinv$start1,
											 gene1Ends = sv_now_t2tinv$end1,
											 gene2Chromosomes = sv_now_t2tinv$chrom2,
											 gene2Starts = sv_now_t2tinv$start2,
											 gene2Ends = sv_now_t2tinv$end2,
											 axisPadding = 0,
											 labels=sv_now_t2tinv$label,
											 color = "black",
											 displayLabel = F,
											 maxRadius = 0.4)#, minRadus=0.5)

		caja=1}
	sv_now_h2hinv = sv_now[which(sv_now$colour == "forestgreen"),]
	if (nrow(sv_now_h2hinv)!=0){
		tracks = tracks + BioCircosLinkTrack("Links", gene1Chromosomes = sv_now_h2hinv$chrom1,
											 gene1Starts = sv_now_h2hinv$start1,
											 gene1Ends = sv_now_h2hinv$end1,
											 gene2Chromosomes = sv_now_h2hinv$chrom2,
											 gene2Starts = sv_now_h2hinv$start2,
											 gene2Ends = sv_now_h2hinv$end2,
											 axisPadding = 0,
											 labels=sv_now_h2hinv$label,
											 color = "forestgreen",
											 displayLabel = F,
											 maxRadius = 0.4)#, minRadus=0.5)
		caja=1}
	sv_now_dele = sv_now[which(sv_now$colour == "orange"),]
	if (nrow(sv_now_dele)!=0){
		tracks = tracks + BioCircosLinkTrack("Links", gene1Chromosomes = sv_now_dele$chrom1,
											 gene1Starts = sv_now_dele$start1,
											 gene1Ends = sv_now_dele$end1,
											 gene2Chromosomes = sv_now_dele$chrom2,
											 gene2Starts = sv_now_dele$start2,
											 gene2Ends = sv_now_dele$end2,
											 axisPadding = 0,
											 labels=sv_now_dele$label,
											 color = "orange",
											 displayLabel = F,
											 maxRadius = 0.4)
		caja=1}

	BioCircos(genome=myGenome, tracks, yChr = F, chrPad = .03, displayGenomeBorder = T, 
			  genomeTicksLen = 4, genomeTicksTextSize = 0, genomeTicksScale = 50000000,
			  genomeLabelTextSize = 30, #genomeLabelDisplay  = T,
			  genomeFillColor = rep("white",23),genomeBorderColor = "black",genomeBorderSize = 2)

}
