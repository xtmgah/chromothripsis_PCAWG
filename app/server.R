library(ggplot2)
#library(DT)

function(input, output, session) {
#d= readRDS("calls_all_with_all_info.rds")
source("load_data.R")

  

output$myImage <- renderImage(deleteFile = FALSE,{
  # Return a list containing the filename
  cat("\n\n\n\n\n\n\n")
  print(input$donor)
  print(unlist(strsplit(as.vector(input$donor),"::"))[2])
  list(src = paste0("circos_plots_all/",unlist(strsplit(input$donor,"::"))[2],".png"),
       #list(src = "www/your_png_file.png",
       contentType = 'image/png')
})


  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    d[which(d$donor_unique_id == input$donor & d$Chr == input$chromosome),]

    
  })

#out = d[1:5,1:5]

selectedData_donor_info <- reactive({
  out = unique(d[which(d$donor_unique_id == input$donor),])
  col_names = c( "icgc_donor_id"      ,                      
               "submitted_donor_id"       ,                
               "tcga_donor_uuid"       ,                   
               "donor_sex"        ,                        
               "donor_vital_status"     ,                  
               #"donor_diagnosis_icd10"  ,                  
               #"first_therapy_type"  ,                     
               #"first_therapy_response"  ,                 
               "donor_age_at_diagnosis"   ,                
               "donor_survival_time"       ,               
               "donor_interval_of_last_followup",          
               #"tobacco_smoking_history_indicator",        
               #"tobacco_smoking_intensity"         ,       
               #"alcohol_history"                    ,      
               #"alcohol_history_intensity"           ,     
               #"response"                             ,                               
               "tumor_stage" 
)
  
out = out[,which(names(out) %in% col_names)]  
  return(unique(out))
})






selectedData_all_chrs <- reactive({
  out = d[which(d$donor_unique_id == input$donor),]
  #out = out[order(out$Chr),]
#  out = out[order(out$chrom),]

#   # change colnames
#   cols=c(#"histo",
#     "icgc_donor_id",
#     "position",
#          "chrom",
#          "type",
#          "clusterSize",
#          "clusterSize_all",
#          "SVs",
#          "SVs_sample",
#          "imax",
#          "i_i2_eval",
#          "fragment_joints_FDR",
#          "chr_breakpoint_enrichment_FDR",
#          "inter_other_chroms_coords_all",
#          #"donor_unique_id",
#          "purity","ploidy")
#   out = out[,which(names(out) %in% cols)]

#   col_nams = c(#"Cancer type"
#                "ICGC donor ID","Position", "chr",
#                                "Type",
#                                "Interleaved intrachr. SVs","Total SVs (intrachr. + transl.)",
#                                "SV types",
#                                "SVs in sample","Oscillating CN (2 and 3 states)",
#                                "CN segments", "FDR fragment joints","FDR chr. breakp. enrich.","Linked to chrs",#"donor_unique_id",
#                "Purity","Ploidy")
# names(out) = col_nams[match(names(out),cols)]

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

ordered = c("1" , "2" , "3" , "4" , "5" , "6" , "7" , "8",  "9"  ,"10", "11" ,"12" ,"13", "14" ,"15",
            "16", "17" ,"18" ,"19" ,"20", "21", "22","X")
out = out[match(ordered,out$Chr),]
chs = out$Chr
out$Chr = NULL
out = data.frame(t(out)) #[,2:ncol(out)]))
#print(chs)
colnames(out) = chs


  return(out)
})
  

  
  output$plot1 <- renderPlot({
#     palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
#               "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
#     
#     par(mar = c(5.1, 4.1, 0, 1))
#     plot(selectedData(),
#          col = clusters()$cluster,
#          pch = 20, cex = 3)
#     points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    #ggplot(selectedData(),aes(Start,End)) + geom_point()
    source("circos.R")  ##_chromoanasynthesis_40e17bf8.R")
    #plot(1,1)
    })

output$test <- renderPlot({plot(1,1)})

## table
#print( t(selectedData()[1,1:5]))
#pepe = selectedData()
#pp = data.frame(t(pepe))
#names(pp) = rownames(pepe)
#print(pepe)
#output$table1 <-  DT::renderDataTable(pepe)


output$donor_choice <- renderUI({
  selectInput('donor', 'Donor ID', unique(d$donor_unique_id[which(d$histo == input$cancer_type)]))
})


output$mytable2 <- DT::renderDataTable({
  DT::datatable(selectedData_all_chrs() )
})


#-------------------------------------------------
# Table with donor information (e.g. age, etc..)
output$table_donor_info <- DT::renderDataTable({
  DT::datatable(selectedData_donor_info() )
})
#-------------------------------------------------

#options = list(orderClasses = TRUE, autoWidth=TRUE, columnDefs = list(list(className = 'dt-center', targets = 5)),pageLength = 10)

#------------------------------------------------------------------------------------------
# zoomable plot
#------------------------------------------------------------------------------------------

#ranges2 <- reactiveValues(x = NULL, y = NULL)


# output$plot3 <- renderPlot({
#   ggplot(selectedData(),aes(min_coord,max_coord)) + geom_point() +
#     coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
# })

# When a double-click happens, check if there's a brush on the plot.
# If so, zoom to the brush bounds; if not, reset the zoom.
# observe({
#   brush <- input$plot2_brush
#   if (!is.null(brush)) {
#     ranges2$x <- c(brush$xmin, brush$xmax)
#     ranges2$y <- c(brush$ymin, brush$ymax)
#     
#   } else {
#     ranges2$x <- NULL
#     ranges2$y <- NULL
#   }
# })


# output$donor_choice <- renderUI({
#   selectInput('donor', 'Donor ID', unique(d$donor_unique_id[which(d$histo == input$cancer_type)]))
# })





# output$pdfviewer <- renderUI({
#     tags$iframe(style="height:600px; width:100%", src="096b4f32-10c1-4737-a0dd-cae04c54ee33.pdf")
#   })
#   
  #return(paste('<iframe style="height:600px; width:100%" src="', input$pdfurl, '"></iframe>', sep = ""))

#output$pdffile = paste('<iframe style="height:600px; width:100%" src="', input$pdfurl, '"></iframe>', sep = "")

#------------------------------------------------------------------------------------------
# plot chromothripsis 
#------------------------------------------------------------------------------------------
source("function_for_SI_plots_w_minor.R")

# ranges2 <- reactiveValues(x = NULL, y = NULL)

##p,breakpoints,CNV_plot,CNV_plot_minor
output$plot_chromo <- renderPlot({
  oo = selectedData()
  pp = plot_chromo(chr=input$chromosome, don= input$donor, data=oo)
  gp1 <- ggplotGrob(pp[[1]] + theme(plot.margin=unit(c(0,1,0,0),"cm")))
  gp2 <- ggplotGrob(pp[[2]]+ theme(plot.margin=unit(c(0,1,0,0), "cm"),axis.text.x=element_blank()))
  gp3 <- ggplotGrob(pp[[3]] + theme(plot.margin=unit(c(0,1,0,0), "cm"),legend.position="none")+theme(axis.text.x=element_blank()))
  gp4 <- ggplotGrob(pp[[4]] + theme(plot.margin=unit(c(0,1,0,0), "cm"),legend.position="none"))
  gp1$widths <- gp2$widths
  gp3$widths <- gp2$widths
  gp4$widths <- gp2$widths
  pp = grid.arrange(gp1,gp2,gp3,gp4,nrow=4,ncol=1,heights=c(0.1,.45,.4,.2)) 
#  pp = arrangeGrob(gp1,gp2,gp3,gp4,nrow=4,ncol=1,heights=c(0.2,.4,.55,.25)) 
  print(pp)
  
})

# output$plot3 <- renderPlot({
#   ggplot(selectedData(),aes(min_coord,max_coord)) + geom_point() +
#     coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
# })


#------------------------------------------------------------------------------------------
# When a double-click happens, check if there's a brush on the plot.
# If so, zoom to the brush bounds; if not, reset the zoom.
# observe({
#   brush <- input$plot2_brush
#   if (!is.null(brush)) {
#     ranges2$x <- c(brush$xmin, brush$xmax)
#     ranges2$y <- c(brush$ymin, brush$ymax)
#     
#   } else {
#     ranges2$x <- NULL
#     ranges2$y <- NULL
#   }
# })




  
}