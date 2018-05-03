library(ggplot2)
library(DT)
library(BioCircos)
library(shiny)
library(shinyWidgets)
source("biocircos_plots.R")
colours_ICGC= read.table("ICGC_colours_2.txt",sep="\t",header=F,stringsAsFactors = F,comment.char="")
d$colour_ICGC = colours_ICGC$V2[match(d$histo,colours_ICGC$V1)]

function(input, output, session) {
	choices_cancer = sort(unique(d$histo)) 


## search mutations
  #output$searchString <- renderText({
  #  values$searchString
  #}) 


	#output$txt2Test <- renderText({ input$txt2 })



	observe({
		output$cancer_type <- renderUI({
			selectizeInput(multiple=T,'donor', 'Donor ID', choices_cancer)
		})
	})


	defaultColors = colours_ICGC$V2

	series <- structure(
						lapply(defaultColors, function(color) { list(color=color) }),
						names = colours_ICGC$V1
						)

	#series2 <- structure(
#						 lapply(defaultColors, function(color) { list(color=color) }),
#						 names = levels(unique(d$histo)) 
#						 )

	yearData <- reactive({
		print(d$donor_age_at_diagnosis)
		idx = which(d$purity >= input$purity & d$ploidy >= input$ploidy & d$histo %in% input$cancer_type & 
					d$Chr %in% input$chrom)
		all_cases=d[idx,]
		df = unique(d[idx, c("donor_unique_id","purity","ploidy","histo","SVs in sample","donor_age_at_diagnosis")])

		## second data frame 
		idx2 = which(d$purity >= input$purity & d$ploidy >= input$ploidy & d$histo %in% input$cancer_type &
					 d$Chr %in% input$chrom & 
					 d[,"Nb. oscillating CN"] >= input$oscil2 & d$ratio >= input$fraction_SVs_chromo)

		d$donor_chr = paste(d$donor_unique_id, "; chromosome ",d$Chr,sep="")
		df2 = unique(d[idx2, c("donor_chr","ratio","Nb. oscillating CN","histo","SVs in sample","Chr","donor_age_at_diagnosis")])
		names(df2) = c("donor_chr","Fraction of SVs in the tumor involved in chromothripsis and mapped to this chr","Nb. oscillating CN between 2 states","cancer type","SVs in sample","Chr","donor_age_at_diagnosis")
		names(df)[4] = names(df2)[4] = "Cancer type"
		return(list(df,df2,all_cases))
	})


	output$chart <- reactive({
		# Return the data and options
		list(
			 data = googleDataTable(yearData()[[1]]),
			 options = list(
							title = "Purity vs. ploidy",
							series = series
							)
			 )
	})

	output$chart2 <- reactive({
		list(
			 data = googleDataTable(data=yearData()[[2]]),
			 options = list(
							title = "Fraction of SVs involved in chromothripsis vs uninterrupted CN oscillations",
							series = series
							)
			 )
	})


	#---------------------------------
	# plot the chromothripsis rates
	#---------------------------------
	#output$plot_rates <- renderPlot({
	#	idx = which(d$histo %in% input$cancer_type)
	#	now2=d[idx,] 
	#	# there are some cancer types with NA values for ploidy and purity.
	#	# We need to pick these because, otherwise, the rates plot gets crazy as there are some cancer type missing.. 
	#	idx = which(( now2$purity >= input$purity & now2$ploidy >= input$ploidy) |
	#				(is.na(now2$ploidy)) | (is.na(now2$purity)))
	#	now2=now2[idx,] 
	#	idx = which(now2[,"Nb. oscillating CN"] < input$min_nb_oscil)
	#	now2$chromo_label[idx] = "No"; 
	#	## remove those with less breakpoints than needed
	#	idx = which(now2$breakpoints <  input$breakpoints_cluster)
	#	now2$chromo_label[idx] = "No";
	#	## remove those with less than nb_chrs_affected altered
	#	dons_idx = ddply(now2,.(donor_unique_id),summarise,count=sum(chromo_label!="No"))
	#	dons_idx = dons_idx$donor_unique_id[dons_idx$count < input$nb_chrs_affected]
	#	now2$chromo_label[which(now2$donor_unique_id %in% dons_idx)] = "No"
	#	w_chromo = unique(now2$donor_unique_id[grep("High|Low",now2$chromo_label)])

	# Combine the selected variables into a new data frame
selectedData <- reactive({
    d[which(d$donor_unique_id == input$donor & d$Chr == input$chromosome),]
})


selectedData_donor_info <- reactive({
    out = unique(clinical[which(clinical$donor_unique_id == input$donor),])
    out = t(unique(out))
    colnames(out)="Value"
    return(out)
})


##selectedData_donor_info <- reactive({
##    out = unique(d[which(d$donor_unique_id == input$donor),])
##    print(input$donor)
##    col_names = c( "icgc_donor_id"      ,
##                  "submitted_donor_id"       ,
##                  "tcga_donor_uuid"       ,
##                  "donor_sex"        ,
##                  "donor_vital_status"     ,
##                  "donor_age_at_diagnosis"   ,
##                  "donor_survival_time"       ,
##                  "donor_interval_of_last_followup",
##                  "tumor_stage"
##                  )
##    out = out[,which(names(out) %in% col_names)]
##    out = t(unique(out))
##    print(out)
##    colnames(out)="Value"
##    return(out)
##})
selectedData_all_chrs <- reactive({
		#d$zcomment = d$comment	
		out = d[which(d$donor_unique_id == input$donor & d$Chr %in% input$cols_to_show),]

		col_names=c(
					"chromo_label" ,                                                     
					"type_chromo",   
					"Chr"    ,                                  
					"Start"  ,                                  
					"End" ,                                     
					"Intrchr. SVs" ,                            
					"Total SVs (intrachr. + transl.)",          
					"SVs in sample"  ,                          
					"Nb. DEL"  ,                                
					"Nb. DUP"  ,                                
					"Nb. h2hINV"  ,                             
					"Nb. t2tINV" ,                              
					"Nb. TRA",
					"Nb. CN segments"   ,        
					"Nb. oscillating CN"    ,                   
					"Nb. oscillaring CN 3 states"  ,           
					"CN segments chr."  ,                       
					"Nb. oscillating chr."   ,                  
					"Nb. oscillating chr 3 states",   
					"P fragment joints" ,                       
					"P chr breakpoint enrichment" ,                            
					"P exponential dist. cluster" ,                                         
					"clustered APOBEC mutations (<2.5 Kb)"  ,                           
					"Other interacting chromothripsis regions", 
					"FDR fragment joints intrachr. SVs and TRA",
					"FDR chr breakpoint enrichment",            
					"FDR exponential dist."    ,                
					"FDR exponential dist. cluster"  ,          
					"FDR fragment joints intrachr. SVs"   ,                                                    
					"TP53_mutations",                           
					"MDM2_CN",
					"comment"
					)

		# select table for only the chrs with chromothripsis
		ordered = c("1" , "2" , "3" , "4" , "5" , "6" , "7" , "8",  "9"  ,"10", "11" ,"12" ,"13", "14" ,"15",
					"16", "17" ,"18" ,"19" ,"20", "21", "22","X")
		ordered = ordered[ordered %in% input$cols_to_show]
		out = out[match(ordered,out$Chr),]

		out = out[,match(col_names,names(out))]
		names(out)[4] = "Start coordinate for SV cluster in chr"
		names(out)[5] = "End coordinate for SV cluster in chr"
		names(out)[6] = "Nb. intrachromosomal SVs"
		names(out)[7] = "Total Nb. SVs (intrachr. + interchr.)"
		names(out)[8] = "Nb. SVs in sample"
		names(out)[which(names(out) == "Nb. CN segments")] = "Nb. CN segments in SV cluster"
		names(out)[which(names(out) == "Nb. oscillating chr.")] = "Nb. oscillating CN across 2 states in chr."
		names(out)[which(names(out) == "CN segments chr.")] = "Nb. CN segments in chr."
		names(out)[which(names(out) == "Nb. oscillating CN")] = "Nb. oscillating CN across 2 states"
		names(out)[which(names(out) == "Nb. oscillaring CN 3 states")] = "Nb. oscillating CN across 3 states"
		names(out)[which(names(out) == "Nb. oscillating chr 3 states")] = "Nb. oscillating CN across 3 states in chr."
		names(out)[which(names(out) == "Nb. oscillating chr.")] = "Nb. oscillating CN across 2 states in chr."          



		names(out)[1:2] = c("Chromothripsis in chromosome?","Chromothripsis category")
		chs = out$Chr
		out$Chr = NULL
		out = data.frame(t(out))
		colnames(out) = as.vector(chs)
		# chr selection
		idx=which(colnames(out) %in% input$cols_to_show)
		out = out#[,idx] ##which(colnames(out) %in% input$cols_to_show)]
		#return(out)} else{
		return(out)
	})

	#get_cases_with_chromo_type = function(d,input){
	#	choices=sort(unique(d$histo[which(d$type_chromo %in% input$chromo_type)]))
	#	if length(choices == 0){
	#		out=choices} else{
	#		out="Biliary-AdenoCA"
	#	}
	#	return(out)
	#}



	#get_width2 = function(d,input){
	#	out=c(); out2=c()
	#	typ =c("After polyploidization","Before polyploidization","Canonical without polyploidization","With other complex events")
	#	if ( sum(input$chromo_type %in% typ)>0 ){
	#		typ_now = typ[which(typ %in% input$chromo_type)]
	#		sele=sort(unique(d$donor_unique_id[which(d$histo == input$cancer_type2 & d$type_chromo %in% typ_now)]))
	#		sele = d[which(d$histo == input$cancer_type2 & d$type_chromo %in% typ_now),]
	#		out = sort(unique(sele$donor_unique_id))
	#sor#t(unique(d$donor_unique_id[which(d$histo == input$cancer_type2 & d$type_chromo %in% typ_now)]))
	#		out2=sele$Chr[ which(sele$type_chromo %in% typ & sele$donor_unique_id == input$donor)]
	#	}
	#	if ("No chromothripsis" %in% input$chromo_type){
	#		now = d[which(d$histo == input$cancer_type2),]
	#		now_sum = ddply(now,.(donor_unique_id),summarise,tot=sum(type_chromo!="No chromothripsis"))
	#		now_sum = unique(now_sum$donor_unique_id[now_sum$tot==0])
	#		out =c(out, now_sum)
	#		out2=c("22")
	#	}
	#	### select chromosomes now
	#	#out2=c()
	#	#types_now = d[which(d$donor_unique_id == input$donor),]
	#	#types_now_chromo_type = types_now$type_chromo

	#	#typ =c("After polyploidization","Before polyploidization","Canonical without polyploidization","With other complex events")
	#	#if ( sum(types_now_chromo_type %in% typ)>0 ){
	#	#	out2= types_now$Chr[ which(types_now$type_chromo %in% typ)]
	#	#} else{ out2=c("22")}

	#	return(list(out,out2))
	#}

	get_canc_types = function(d,input){
		choices = sort(unique(d$histo[which(d$type_chromo %in% input$chromo_type)]))
		#if (length(choices) == 0){ # we assume ther is always a valid choice (i.e. we have tumors for all categories of chromotype)
		#}
		return(choices)
	}

	output$cancer_type2 <- renderUI({
		pickerInput('cancer_type2', 'Cancer type',
					selected="Bladder-TCC", #"Biliary-AdenoCA",
					choices=get_canc_types(d,input), #sort(unique(d$histo[which(d$type_chromo %in% input$chromo_type)])),
					options = list( `actions-box` = TRUE, `size`= 5, 
								   style = "btn-info",
								   `selected-text-format` = "count > 3" ), 
					multiple = TRUE )
	})

	# get donors with the specified cancer types
	get_with = function(d,input){
		d$histo=as.vector(d$histo) #XX
		out=c()
		typ =c("After polyploidization","Before polyploidization","Canonical without polyploidization","With other complex events")
		if ( sum(input$chromo_type %in% typ)>0 ){
			typ_now = typ[which(typ %in% input$chromo_type)]
			#print(nrow(d))
			#print(which(d$histo %in% input$cancer_type2 & d$type_chromo %in% typ_now))
			out = sort(unique(d$donor_unique_id[which(d$histo %in% input$cancer_type2 & d$type_chromo %in% typ_now)]))
		}
		if ("No chromothripsis" %in% input$chromo_type){
			now = d[which(d$histo %in% input$cancer_type2 & d$type_chromo %in% c("No chromothripsis") ),]
			now_sum = ddply(now,.(donor_unique_id),summarise,tot=sum(type_chromo=="No chromothripsis",na.rm=T))
			now_sum = unique(as.vector(now_sum$donor_unique_id[now_sum$tot==23]))
			#now_sum = ddply(now,.(donor_unique_id),summarise,tot=sum(type_chromo!="No chromothripsis"))
			#now_sum = unique(now_sum$donor_unique_id[now_sum$tot==0])
			out =c(out, now_sum)
		}
#XXX create a global variable with the types of chromo at the moment y make the donor choice dependent on it with observe event.
		#input3 <<- list(cancer_type=input$cancer_type2,typ = input$chromo_type, donor=input$donor,
	#					donor=input$donor, chr_selection_circos=out,range_dist=input$range_dist, show_patho_indels = input$show_patho_indels, show_nopatho_indels =input$show_nopatho_indels, show_patho = input$show_patho, show_nopatho= input$show_nopatho, show_chromo_track=input$show_chromo_track, show_genes=input$show_genes)
		return(out)
	}
	output$donor_choice <- renderUI({
		selectizeInput(multiple=F,'donor', 'Donor ID', 
					   #get_with2(d,input)[[1]]
					   get_with(d,input)
					   )
	})


	###### get the chrs with chromothripsis in the selected tumor

	get_chrs_with = function(d,input){
		out=c()
		types_now = d[which(d$donor_unique_id == input$donor),] #& d$type_chromo %in% input$chromo_type & d$histo %in% input$cancer_type2),]
		types_now_chromo_type = types_now$type_chromo

		typp =c("After polyploidization","Before polyploidization","Canonical without polyploidization","With other complex events")
		if ( sum(types_now_chromo_type %in% typp)>0 ){
			out= types_now$Chr[ which(types_now$type_chromo %in% typp)]
		} else{ out=c("22")}
		#update_circos <<- TRUE
		input2 <<- list(#cancer_type=input$cancer_type2, 
						#typ = input$chromo_type, 
						donor=input$donor, 
						chr_selection_circos=out,range_dist=input$range_dist, show_patho_indels = input$show_patho_indels, show_nopatho_indels =input$show_nopatho_indels, show_patho = input$show_patho, show_nopatho= input$show_nopatho, show_chromo_track=input$show_chromo_track, show_genes=input$show_genes)

		return(out)
	}

	output$chr_selection_circos  <- renderUI({
		pickerInput( 'chr_selection_circos', 'Chromosome selection for circos plot',
					choices = as.character(c(1:22,"X")),
					#selected=get_with2(d,input)[[2]],
					selected=get_chrs_with(d,input),
					options = list( `actions-box` = TRUE, size = 5,
								   style = "btn-info",
								   `selected-text-format` = "count > 3" ), multiple = TRUE )

	})

	observeEvent(c(
				   input$chr_selection_circos,input$range_dist,input$show_patho_indels, input$show_nopatho_indels,input$show_patho,input$show_nopatho,input$show_chromo_track,input$show_genes, input$donor # ,input$cancer_type2,input$chromo_type #,  input$donor,input$chromo_type ### XXXX input$donor added input$donor input$cancer_type2,
				   ), { output$biocirc <- renderBioCircos({
				   plot_biocirc(input2,input)#$chrs_selection_circos)
				   })
} )

	#---------------------------------------------------------------
#	output$cols_to_show <- 
#		renderUI({
#                                pickerInput('cols_to_show', 'Select chromosomes',
#                                            selected=as.character(c(1:22,"X")), ##c("1"),#get_chrs_with(d,input), ##input2$chr_selection_circos , ##coas.character(c(1:22,"X")),
#                                            choices=as.character(c(1:22,"X")),
#                                            options = list( `actions-box` = F, `size`= 5,
#                                                            style = "btn-info",
#                                                            `selected-text-format` = "count > 3" ),
#                                            multiple = TRUE )
#	})




	output$mytable2 <- DT::renderDataTable({
		DT::datatable(selectedData_all_chrs(), style = 'bootstrap') %>% formatStyle(
																					#c(as.character(1:22,"X")),
																					input$cols_to_show,
																					#get_chrs_with(d,input),
																					backgroundColor = styleEqual(c("High confidence","Linked to high confidence", "Linked to low confidence","Low confidence","No"), 
																												 c('#eded2a','#eded2a','#a1d99b','#a1d99b','#ece2f0'))
																					#c('#feb24c', '#feb24c','#a1d99b','#a1d99b','#ece2f0'))
																					)
	})

	#-------------------------------------------------
	# Table with donor information (e.g. age, etc..)
	output$table_donor_info <- DT::renderDataTable({
		DT::datatable(selectedData_donor_info(),style = 'bootstrap',options = list(sDom  = '<"top">lrt<"bottom">ip') ) 
	})
	#-------------------------------------------------

	#-------------------------------------------------
	# Table to find a case on the basis of some information 
	#-------------------------------------------------
	output$find_case <- DT::renderDataTable({
		DT::datatable(selectedData_find_case(), style = 'bootstrap',filter = list(position = "top") )
	})

	selectedData_find_case <- reactive({
		out = d# [which(d$donor_unique_id == input$donor),]
		out$tp53_bin="Yes"; out$tp53_bin[is.na(out$TP53_mutations)]="No"; out$tp53_bin=factor(out$tp53_bin)
		out2=ddply(out,.(donor_unique_id),summarise,chrs_affected=sum(chromo_label!="No"))
		out$chrs_affected = out2$chrs_affected[match(out$donor_unique_id,out2$donor_unique_id)]

		col_names=c(#"chromo",                                   
					"histo",
					"chromo_label" ,                                                     
					"type_chromo",   
					"donor_unique_id" ,
					"icgc_donor_id",
					"Chr"    ,                                  
					#"Start"  ,                                  
					#"End" ,                                     
					#"Intrchr. SVs" ,                            
					#"Total SVs (intrachr. + transl.)",          
					"breakpoints",
					"chrs_affected",
					"SVs in sample"  ,                          
					#"Nb. DEL"  ,                                
					#"Nb. DUP"  ,                                
					#"Nb. h2hINV"  ,                             
					#"Nb. t2tINV" ,                              
					#"Nb. TRA",
					"Nb. CN segments"   ,        
					"Nb. oscillating CN"    ,                   
					#"Nb. oscillaring CN 3 states"  ,           
					"CN segments chr."  ,                       
					#"Nb. oscillating chr."   ,                  
					#"Nb. oscillating chr 3 states",   
					#"P fragment joints" ,                       
					#"P chr breakpoint enrichment" ,                            
					#"P exponential dist. cluster" ,                                         
					#"clustered APOBEC mutations (<2.5 Kb)"  ,                           
					#"Other interacting chromothripsis regions", 
					#"FDR fragment joints intrachr. SVs and TRA",
					#"FDR chr breakpoint enrichment",            
					#"FDR exponential dist."    ,                
					#"FDR exponential dist. cluster"  ,          
					#"FDR fragment joints intrachr. SVs"   ,                                                    
					"TP53_mutations",
					"tp53_bin",
					"MDM2_CN"                                                                
					)

		# select table for only the chrs with chromothripsis
		#ordered = c("1" , "2" , "3" , "4" , "5" , "6" , "7" , "8",  "9"  ,"10", "11" ,"12" ,"13", "14" ,"15",
		#				"16", "17" ,"18" ,"19" ,"20", "21", "22","X")
		#out = out[match(ordered,out$Chr),]
		#out = out[order(out$Chr,decreasing=F),]
		out$chrnum = as.vector(out$Chr); out$chrnum[out$chrnum=="X"]="23"; out$chrnum=as.numeric(out$chrnum)
		out = with(out, out[order(histo,donor_unique_id,chrnum),])
		print(head(out[,c("histo","Chr","chrnum","donor_unique_id")]))
		out$chrnum=NULL
		out$TP53_mutations = factor(out$TP53_mutations)

		out = out[,match(col_names,names(out))]
		names(out)[which(names(out) == "breakpoints")] = "Nb. brekpoints in chromothripsis region"
		names(out)[which(names(out) == "histo")] = "Cancer type"
		names(out)[which(names(out) == "chrs_affected")] = "Nb. chrs with chromothripsis"
		names(out)[which(names(out) == "icgc_donor_id")] = "ICGC donor ID"
		names(out)[which(names(out) == "tp53_bin")] = "TP53 mutated?"
		names(out)[which(names(out) == "TP53_mutations")] = "TP53 mut."
		names(out)[which(names(out) == "type_chromo")] = "Chromothipsis category"
		names(out)[which(names(out) == "CN segments chr.")] = "Nb. CN segments in chr."
		out[,1] = factor(out[,1])
		out[,2] = factor(out[,2])
		out[,3] = factor(out[,3])
		out[,4] = factor(out[,4])
		out[,5] = factor(out[,5])
		out[,6] = factor(out[,6])



		names(out)[1:2] = c("Cancer type","Chromothripsis in chromosome?") #Confidence of the chromothripsis call")
		rownames(out)=NULL
		return(out)
	})


	#---------------------------------
	# plot the chromothripsis rates 2
	#---------------------------------
	output$plot_rates2 <- renderPlot({
		idx = which(d$histo %in% input$cancer_type)
		now2=d[idx,] 
		# there are some cancer types with NA values for ploidy and purity.
		# We need to pick these because, otherwise, the rates plot gets crazy as there are some cancer type missing.. 
		idx = which(( now2$purity >= input$purity & now2$ploidy >= input$ploidy) |
					(is.na(now2$ploidy)) | (is.na(now2$purity)))
		now2=now2[idx,] 


		idx = which(now2[,"Nb. oscillating CN"] < input$min_nb_oscil)
		now2$chromo_label[idx] = "No"; 
		now2$type_chromo[idx] = "NA"; 

		## across 3 states
		idx = which(now2[,"Nb. oscillaring CN 3 states"] < input$min_nb_oscil3)
		now2$chromo_label[idx] = "No";
		now2$type_chromo[idx] = "NA";


		## remove those with less breakpoints than needed
		idx = which(now2$breakpoints <  input$breakpoints_cluster)
		now2$chromo_label[idx] = "No";
		now2$type_chromo[idx] = "NA"; 

		## remove those with less than nb_chrs_affected altered
		dons_idx = ddply(now2,.(donor_unique_id),summarise,count=sum(chromo_label!="No"))
		dons_idx = dons_idx$donor_unique_id[dons_idx$count < input$nb_chrs_affected]
		now2$chromo_label[which(now2$donor_unique_id %in% dons_idx)] = "No"
		now2$type_chromo[which(now2$donor_unique_id %in% dons_idx)] = "NA"

		w_chromo = unique(now2$donor_unique_id[grep("High|Low",now2$chromo_label)])
		now2$chromo=0; now2$chromo[which(now2$donor_unique_id %in% w_chromo)]=1
		# first low to overwirte high
		are_low = unique(now2$donor_unique_id[grep("Low",now2$chromo_label)])
		are_high = unique(now2$donor_unique_id[grep("High",now2$chromo_label)])
		# samples with only low!
		are_low = are_low[!(are_low %in% are_high)]
		#now2$tt = ""; now2$tt[which(now2$donor_unique_id %in% are_low)]= "low"
		#now2$tt[which(now2$donor_unique_id %in% are_high)]= "high"
		now2$tt = ""; now2$tt[grep("Low",now2$chromo_label)] = 'low'
		now2$tt[grep("High",now2$chromo_label)] = 'high'

		now2$high = 0; now2$high[which(now2$donor_unique_id %in% are_high)]=1  ## tumors with high
		now2$low=0; now2$low[which(now2$donor_unique_id %in% are_low & !(now2$donor_unique_id %in% are_high))]=1   #low only

		#now2$high = 0; now2$high[grep("High",now2$chromo_label)] = 1
		#now2$low = 0; now2$low[grep("Low",now2$chromo_label)] = 0

		## add cano labels
		now2$cano = "0"; now2$cano[grep("poly",now2$type_chromo)]="1"
		now2$cano = paste( now2$tt, now2$cano)
		# el problema es que son samples, y por tanto, una misma sample can have low and high...
		#dons = unique(calls_all$donor_idx)
		now2$cano_sample=0
		for (iid in unique(c(are_low,are_high))){
			aaa = now2[which(now2$donor_unique_id == iid),]
			aa = sum(aaa$cano == "high 1",na.rm=T)
			bb = sum(aaa$cano == "low 1",na.rm=T)
			if (aa>0){now2$cano_sample[which(now2$donor_unique_id == iid)] = 1}
			if (aa==0 & bb >0 & (sum(aaa$tt == 'high')==0)){now2$cano_sample[which(now2$donor_unique_id == iid)] = 2}
		}

		d1 = ddply(now2,.(histo),summarise,chromo=sum(chromo),tot=length(CNV),high=sum(high),low=sum(low),
				   cano_high=length(which(cano_sample==1)),
				   cano_low=length(which(cano_sample==2)))

		d1$histo = as.vector(d1$histo)
		d1 = rbind(d1, data.frame(histo="All selected tumors",chromo=sum(d1$chromo),tot=sum(d1$tot),high=sum(d1$high),low=sum(d1$low),
								  cano_high=sum(d1$cano_high), cano_low=sum(d1$cano_low)) )

		d1$chromo = d1$chromo/23
		d1$high = d1$high/23
		d1$low = d1$low/23
		d1$cano_high = d1$cano_high / 23
		d1$cano_low = d1$cano_low / 23
		d1$tot = d1$tot/23
		d1$percentage = 100*(d1$chromo/d1$tot)
		d1$percentage_low = 100*(d1$low/d1$tot)
		d1$percentage_high = 100*(d1$high/d1$tot)
		## percentages cano
		d1$percentage_cano_high = 100*(d1$cano_high/d1$tot)
		d1$percentage_cano_low = 100*(d1$cano_low/d1$tot) 
		d1 = d1[order(d1$percentage,decreasing = T),]
		d1$histo = as.vector(d1$histo)
		d1$histo = factor(d1$histo,levels=d1$histo)
		d1$histo = as.vector(d1$histo)
		d1$histo = factor(d1$histo,levels=d1$histo)

		d1$percentage_cano_high = d1$cano_high / d1$tot
		d1$percentage_rest_high = (d1$high -d1$cano_high) / d1$tot
		d1$percentage_cano_low = d1$cano_low / d1$tot 
		d1$percentage_rest_low = (d1$low - d1$cano_low) / d1$tot
		#d1$percentage_rest_low = (d1$low - d1$high  - d1$cano_low) / d1$tot

		d1$percentage_cano_high = d1$percentage_cano_high * 100
		d1$percentage_rest_high = d1$percentage_rest_high * 100
		d1$percentage_cano_low  = d1$percentage_cano_low  *100
		d1$percentage_rest_low  = d1$percentage_rest_low  * 100


		d7 = melt(d1[,c("histo","percentage_cano_high", "percentage_cano_low", "percentage_rest_high","percentage_rest_low")])
		## add the info for the label
		d7$tot = d1$tot[match(d7$histo,d1$histo)]
		d7$chromo = d1$chromo[match(d7$histo,d1$histo)]
		n_elements=length(input$cancer_type)+1
		#d7$position = rep(( (d7$chromo[1:37]/d7$tot[1:37])*100 )+ 7.5 , 4)
		#d7$position = rep( d7$value[1:n_elements] + d7$value[(n_elements+1):(n_elements*2)]+9, 2)
		d7$position = (d7$chromo/d7$tot)*100 + 9
		#d7$position = rep( d7$value2[1:n_elements] + d7$value[(n_elements+1):(n_elements*2)]+9, 2)


		d7$variable = as.vector(d7$variable)
		d7$variable = factor(d7$variable,levels = rev(c("percentage_cano_high","percentage_rest_high","percentage_cano_low","percentage_rest_low")))
		t_size=15


		p_rates <- ggplot(d7,aes(x=histo,y=value,fill=variable)) + geom_bar(stat="identity",colour="black",size=0.2) +
			theme(axis.text.x=element_text(angle=90,vjust=.5,hjust=1,size=t_size,colour="black"),
				  axis.ticks.x=element_blank(),
				  plot.margin=unit(c(4,1,1,1),"cm"),text=element_text(size=t_size,colour="black")) +
									 ylab("Percentage of tumors with chromothripsis") + xlab(NULL) +
									 geom_text(aes( y=position, label = paste0("frac(",d7$chromo, ",", d7$tot, ")")),parse = TRUE,size=5.5) +
									 theme(panel.border = element_blank(), axis.line = element_line(),text=element_text(size=t_size,colour="black")) +
									 theme(legend.position = c(0.5,1.1),legend.direction = "horizontal",legend.background=element_blank(),
										   legend.title=element_blank(),
										   legend.text=element_text(size=t_size+2),
										   plot.background = element_blank(),
										   panel.grid.major = element_blank(),
										   panel.grid.minor = element_blank(),
										   legend.key.size = unit(2, 'lines'),
										   axis.text = element_text(size=t_size), axis.title=element_text(size=t_size+3)) +
									 scale_y_continuous(limits=c(0,115),expand=c(0.01,0.01),breaks=c(0,25,50,75,100),labels=as.character(c(0,25,50,75,100))) +
									 theme(
										   panel.background  = element_rect(fill = 'white'))  +
									 scale_fill_manual("Confidence",labels=rev(c(" High confidence: canonical"," High confidence: with other complex events",
																				 " Low confidence: canonical"," Low confidence: with other complex events   ")),
													   #values =c("#FFFF99","#addd8e","#9ecae1","#deebf7")
													   #values=c("#a6611a","#dfc27d","#80cdc1","#018571")  #XX
													   #values=c("#7fc97f","#beaed4","#fdc086","#ffff99"),
													   values=c("#a6cee3","#1f78b4","#b2df8a","#33a02c") #XXX
													   #values=c("#ffffcc","#a1dab4","#f1eef6","#d7b5d8")
													   ) +
									 guides(fill=guide_legend(ncol=2,nrow=2))


								 return(p_rates)    

	})

}






#library(ggplot2)
#library(DT)
#library(BioCircos)
#library(shiny)
#library(shinyWidgets)
#source("biocircos_plots.R")
#colours_ICGC= read.table("ICGC_colours_2.txt",sep="\t",header=F,stringsAsFactors = F,comment.char="")
#d$colour_ICGC = colours_ICGC$V2[match(d$histo,colours_ICGC$V1)]
#
#function(input, output, session) {
#	choices_cancer = sort(unique(d$histo)) 
#
#	observe({
#		output$cancer_type <- renderUI({
#			selectizeInput(multiple=T,'donor', 'Donor ID', choices_cancer)
#		})
#	})
#
#
#	defaultColors = colours_ICGC$V2
#
#	series <- structure(
#						lapply(defaultColors, function(color) { list(color=color) }),
#						names = colours_ICGC$V1
#						)
#	yearData <- reactive({
#		idx = which(d$purity >= input$purity & d$ploidy >= input$ploidy & d$histo %in% input$cancer_type & 
#					d$Chr %in% input$chrom)
#		all_cases=d[idx,]
#		df = unique(d[idx, c("donor_unique_id","purity","ploidy","histo","SVs in sample","donor_age_at_diagnosis")])
#
#		## second data frame 
#		idx2 = which(d$purity >= input$purity & d$ploidy >= input$ploidy & d$histo %in% input$cancer_type &
#					 d$Chr %in% input$chrom & 
#					 d[,"Nb. oscillating CN"] >= input$oscil2 & d$ratio >= input$fraction_SVs_chromo)
#
#		d$donor_chr = paste(d$donor_unique_id, "; chromosome ",d$Chr,sep="")
#		df2 = unique(d[idx2, c("donor_chr","ratio","Nb. oscillating CN","histo","SVs in sample","Chr","donor_age_at_diagnosis")])
#		names(df2) = c("donor_chr","Fraction of SVs in the tumor involved in chromothripsis and mapped to this chr","Nb. oscillating CN between 2 states","cancer type","SVs in sample","Chr","donor_age_at_diagnosis")
#		names(df)[4] = names(df2)[4] = "Cancer type"
#		return(list(df,df2,all_cases))
#	})
#
#
#	output$chart <- reactive({
#		# Return the data and options
#		list(
#			 data = googleDataTable(yearData()[[1]]),
#			 options = list(
#							title = "Purity vs. ploidy",
#							series = series
#							)
#			 )
#	})
#
#	output$chart2 <- reactive({
#		list(
#			 data = googleDataTable(data=yearData()[[2]]),
#			 options = list(
#							title = "Fraction of SVs involved in chromothripsis vs uninterrupted CN oscillations",
#							series = series
#							)
#			 )
#	})

#
#	# Combine the selected variables into a new data frame
#	selectedData <- reactive({
#		d[which(d$donor_unique_id == input$donor & d$Chr == input$chromosome),]
#	})
#
#	selectedData_donor_info <- reactive({
#		out = unique(clinical[which(clinical$donor_unique_id == input$donor),])
#		out = t(unique(out))
#		colnames(out)="Value"
#		return(out)
#	})
#
#	selectedData_all_chrs <- reactive({
#		out = d[which(d$donor_unique_id == input$donor & d$Chr %in% input$cols_to_show),]
#




