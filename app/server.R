library(ggplot2)
library(DT)
library(BioCircos)
source("biocircos_plots.R")
colours_ICGC= read.table("ICGC_colours_2.txt",sep="\t",header=F,stringsAsFactors = F,comment.char="")
d$colour_ICGC = colours_ICGC$V2[match(d$histo,colours_ICGC$V1)]

function(input, output, session) {
  choices_cancer = sort(unique(d$histo)) 

  observe({
    if ("Select all" %in% input$cancer_type) {
      # choose all the choices _except_ "Select All"
      selected_choices <- c(choices_cancer) #, "Select all","Clear selection")
      updateSelectInput(session, "cancer_type", selected = selected_choices)
    } 
    if ("Clear selection" %in% input$cancer_type){

      updateSelectInput(session, "cancer_type", selected = "Biliary-AdenoCA")
    }
  })

  defaultColors = colours_ICGC$V2

  series <- structure(
            lapply(defaultColors, function(color) { list(color=color) }),
            names = colours_ICGC$V1
            )

  series2 <- structure(
             lapply(defaultColors, function(color) { list(color=color) }),
             names = levels(unique(d$histo)) 
             )

  yearData <- reactive({
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
  output$plot_rates <- renderPlot({
    idx = which(d$histo %in% input$cancer_type)
    now2=d[idx,] 
    # there are some cancer types with NA values for ploidy and purity.
    # We need to pick these because, otherwise, the rates plot gets crazy as there are some cancer type missing.. 
    idx = which(( now2$purity >= input$purity & now2$ploidy >= input$ploidy) |
          (is.na(now2$ploidy)) | (is.na(now2$purity)))
    now2=now2[idx,] 


    idx = which(now2[,"Nb. oscillating CN"] < input$min_nb_oscil)
    now2$chromo_label[idx] = "No"; 
    ## remove those with less breakpoints than needed
    idx = which(now2$breakpoints <  input$breakpoints_cluster)
    now2$chromo_label[idx] = "No";
    ## remove those with less than nb_chrs_affected altered
    dons_idx = ddply(now2,.(donor_unique_id),summarise,count=sum(chromo_label!="No"))
    dons_idx = dons_idx$donor_unique_id[dons_idx$count < input$nb_chrs_affected]
    now2$chromo_label[which(now2$donor_unique_id %in% dons_idx)] = "No"

    w_chromo = unique(now2$donor_unique_id[grep("High|Low",now2$chromo_label)])
    now2$chromo=0; now2$chromo[which(now2$donor_unique_id %in% w_chromo)]=1
    # first low to overwirte high
    are_low = unique(now2$donor_unique_id[grep("High|Low",now2$chromo_label)])
    are_high = unique(now2$donor_unique_id[grep("High",now2$chromo_label)])
    #are_low = are_low[!(are_low %in% are_high)]
    now2$high = 0; now2$high[which(now2$donor_unique_id %in% are_high)]= 1
    now2$low = 0; now2$low[which(now2$donor_unique_id %in% are_low)]= 1

    d1 = ddply(now2,.(histo),summarise,chromo=sum(chromo),tot=length(CNV),high=sum(high),low=sum(low))
    d1$histo = as.vector(d1$histo)
    d1 = rbind(d1, data.frame(histo="All selected tumors",chromo=sum(d1$chromo),tot=sum(d1$tot),high=sum(d1$high),low=sum(d1$low)))

    d1$chromo = d1$chromo/23
    d1$high = d1$high/23
    d1$low = d1$low/23
    d1$tot = d1$tot/23
    d1$percentage = 100*(d1$chromo/d1$tot)
    d1$percentage_low = 100*(d1$low/d1$tot)
    d1$percentage_high = 100*(d1$high/d1$tot)
    d1 = d1[order(d1$percentage_low,decreasing = T),]
    d1$histo = as.vector(d1$histo)
    d1$histo = factor(d1$histo,levels=d1$histo)
    d1$percentage_low = d1$percentage_low - d1$percentage_high
    d6 = melt(d1[,c(1,7,8)])

    ## add the info for the label
    d6$tot = d1$tot[match(d6$histo,d1$histo)]
    d6$chromo = d1$chromo[match(d6$histo,d1$histo)]
    n_elements=length(input$cancer_type)+1
    d6$position = rep( d6$value[1:n_elements] + d6$value[(n_elements+1):(n_elements*2)]+9, 2)

    library(RColorBrewer)
    n <- 19
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    col_vector = unique(col_vector)[1:19]


    p_rates <- ggplot(d6,aes(x=histo,y=value,fill=variable)) + geom_bar(stat="identity",colour="black",size=0.1) +
      theme(axis.text.x=element_text(angle=90,vjust=.5,hjust=1),axis.ticks.x=element_blank(),
          plot.margin=unit(c(.1,1,0,1),"cm"),text=element_text(size=12)) +
                  ylab("Percentage of samples with chromothripsis") + xlab(NULL) +
                  geom_text(aes( y=position, label = paste0("frac(",d6$chromo, ",", d6$tot, ")")),parse = TRUE,size=5.5) +
                  theme(panel.border = element_blank(), axis.line = element_line(),text=element_text(size=12)) +
                  scale_fill_manual("Confidence",labels=c("Low","High"),
                            values =col_vector[c(4,5)])+
                  theme(legend.position = c(0.75,.95),legend.direction = "horizontal",legend.background=element_blank(),
                      legend.title=element_text(size=14),legend.text=element_text(size=14),
                      plot.background = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      axis.text = element_text(size=12)) +
                  scale_y_continuous(limits=c(0,115),expand=c(0.01,0.01),breaks=c(0,25,50,75,100),labels=as.character(c(0,25,50,75,100))) +
                  theme(panel.background  = element_rect(fill = 'white'), plot.title = element_text(size=16,face = "bold")) +
                  ggtitle("Percentage of tumors with chromothripsis")

                return(p_rates)    

  })

  output$biocirc <- renderBioCircos({
    plot_biocirc(input$donor,input)
  })

  # reset chrs choices
  observeEvent(input$reset, {
           input$chr_selection_circos <- "22"
  })  

  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    d[which(d$donor_unique_id == input$donor & d$Chr == input$chromosome),]

  })

  selectedData_donor_info <- reactive({
    out = unique(d[which(d$donor_unique_id == input$donor),])
    col_names = c( "icgc_donor_id"      ,                      
            "submitted_donor_id"       ,                
            "tcga_donor_uuid"       ,                   
            "donor_sex"        ,                        
            "donor_vital_status"     ,                  
            "donor_age_at_diagnosis"   ,                
            "donor_survival_time"       ,               
            "donor_interval_of_last_followup",          
            "tumor_stage" 
            )

    out = out[,which(names(out) %in% col_names)]  
    return(unique(out))
  })




  selectedData_all_chrs <- reactive({
    out = d[which(d$donor_unique_id == input$donor),]

    col_names=c(#"chromo",                                   
          "chromo_label" ,                                                     
          "type_chromo",   
          #"donor_unique_id" ,                         
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
          "MDM2_CN"                                                                
          )

    # select table for only the chrs with chromothripsis
    ordered = c("1" , "2" , "3" , "4" , "5" , "6" , "7" , "8",  "9"  ,"10", "11" ,"12" ,"13", "14" ,"15",
          "16", "17" ,"18" ,"19" ,"20", "21", "22","X")
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
    colnames(out) = chs
    return(out)
  })

  output$cancer_type2 <- renderUI({
    selectizeInput(multiple=T,'cancer_type2', 'Cancer type',selected="Biliary-AdenoCA", 
             sort(unique(d$histo[which(d$type_chromo %in% input$chromo_type)])))

  })

  output$donor_choice <- renderUI({
    selectizeInput(multiple=F,'donor', 'Donor ID', 
             sort(unique(d$donor_unique_id[which(d$histo == input$cancer_type2 & d$type_chromo %in% input$chromo_type)]))
             )
  })

  output$mytable2 <- DT::renderDataTable({
    DT::datatable(selectedData_all_chrs(), style = 'bootstrap')
  })

  #Sys.sleep(3)

  # Hide the loading message when the rest of the server function has executed
  #hide(id = "loading-content", anim = TRUE, animType = "fade")
  #show("app-content")

  #-------------------------------------------------
  # Table with donor information (e.g. age, etc..)
  output$table_donor_info <- DT::renderDataTable({
    DT::datatable(selectedData_donor_info(),style = 'bootstrap' )
  })
  #-------------------------------------------------

}