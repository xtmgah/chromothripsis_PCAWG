library(grid)
library(gridExtra)
library(plyr)
library(reshape)
library(DT)
library(BioCircos)
source("load_data.R")
d$Chr = d$chrom
library(shinythemes)
library(googleCharts)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(shinydashboard)

choices_cancer = sort(unique(d$histo)) 
shinyUI(
		fluidPage(title="Chromothripsis PCAWG",theme = shinytheme("sandstone"), 
				  tags$link(
							rel = "stylesheet", 
							href="http://fonts.googleapis.com/css?family=Open+Sans"
							),        

				  tags$body(style="font-family: 'Open Sans';"),
				  tags$head(
							tags$style(HTML("
											.navbar .navbar-nav {float: right}
											"))
											##  .navbar .navbar-header {float: right}
											),

									   navbarPage(title=div("Landscape of chromothripsis in human cancers",responsive = T,
															style="background-colour:#000000;font-style:'Open Sans'; ", 
															class="w3-bar w3-light-grey w3-border", tags$a(class="w3-bar-item w3-button w3-black w3-large")),
												  tabPanel(title=div(style="font-size: 30px;",icon("home")),

														   ## embargo information
														   div(style=" right: 300px;",#position: absolute;
															   dropdownButton( tags$h3("Embargo policy",style="color: red;"),
																			  tags$h4("Please note that the data reported in this site are under embargo. 
																					  Please contact Jennifer Jennings (jennifer.jennings [at] oicr.on.ca) and visit https://dcc.icgc.org/pcawg for further 
																					  information."), 
																					  circle = TRUE, status = "danger", icon = icon("warning"), width = "600px", 
																					  tooltip = tooltipOptions(title = "Embargo policy alert !") 
																					  )
																			  ),

															   div(includeCSS("expanding-card-grid-with-flexbox/css/style2.css"),
																   includeCSS("expanding-card-grid-with-flexbox/scss/style.scss"),
																   includeScript("expanding-card-grid-with-flexbox/js/index.js"),
																   includeHTML("expanding-card-grid-with-flexbox/lo.html")),
															   googleChartsInit(),
															   fluidRow(sidebarPanel(width=12,h2("Chromothripsis rates in human cancers",style="font-style:'Open Sans';text-align: center;"),
																					 h4("The barplot on the right depicts the chromothripsis rates in the cancer types selected
																						that satisfy the minimum values indicated on the left-hand menu.",style="font-style:'Open Sans';text-align: center; text-justify: inter-word;")
																						# selectizeInput("cancer_type", 'Cancer type',multiple = TRUE,choices=choices_cancer,selected="Biliary-AdenoCA"),
																						# actionButton("selectall", "Select all")

																						)),
																		fluidRow(
																				 #### input to control barplot
																				 #shiny::column(width=3,
																				 sidebarPanel(width=3,
																							  pickerInput( inputId = "cancer_type", label = "Cancer type", choices = choices_cancer, 
																										  selected = "Biliary-AdenoCA",
																										  options = list( `actions-box` = TRUE, size = 10, 
																														 style = "btn-info",
																														 `selected-text-format` = "count > 3" ), multiple = TRUE ),

																							  sliderInput("breakpoints_cluster", "Nb. breakpoints in chromothripsis region", #"Brekpoints in cluster (including interchr SVs)",
																										  min = 6, max = 1824,
																										  value = 0, animate = TRUE),
																							  #  ),
																							  #  shiny::column(2, #offset = 1,

																							  sliderInput("min_nb_oscil", "# CN oscillations",
																										  min = 3, max = 100,
																										  value = 4, animate = TRUE),

																							  sliderInput("min_nb_oscil3", "# CN oscillations across 3 CN states",
																										  min = 3, max = 100,
																										  value = 4, animate = TRUE),

																							  #  ),
																							  #shiny::column(2, #offset = 1,
																							  sliderInput("purity", "Purity",
																										  min = min(d$purity,na.rm=T), max = max(d$purity,na.rm=T),
																										  value = min(d$purity,na.rm=T), animate = TRUE),
																							  #),
																							  #shiny::column(2, #offset = 1,
																							  sliderInput("ploidy", "Ploidy",
																										  min = min(d$ploidy,na.rm=T), max = max(d$ploidy,na.rm=T),
																										  value = min(d$ploidy), animate = TRUE),
																							  #),
																							  #shiny::column(3, #offset = 1,
																							  sliderInput("nb_chrs_affected", "Number of chromosomes affected per tumor",
																										  min = 1, max = 23,
																										  value = 1, animate = TRUE)
																							  #)



																							  ),
																				 #shiny::column(width = 9,
																				 mainPanel(
																						   # exaplantion of rates plot:
																						   dropdownButton(
																										  h3("Chromothripis rates in human cancers",style="face: bold;"),
																										  h4("The plot below reports the percentage of tumors harboring chromothripsis events. 
																											 The fraction on top of the bars indicates the number of tumors showing chomothripsis over the total
																											 number of tumors of that type."),
																											 h4("We provide functionalities to set more stringent cut-off values for the properties that a 
																												region needs to satisfy to be considered as chromothrispis. 
																												These are:",
																												tyle="font-style:'Open Sans';"),
																											 tags$br(),
																											 h4(tags$b("1. Minimum number of breakpoints in the chromothripsis region")),
																											 tags$br(),
																											 h4(tags$b("2. Minimum number of uninterrupted CN oscillations between 2 states.")),
																											 h4("Please note that for chromothripsis events where secondary events 
																												alter the CN profile only the region with the highest number of CN oscillations is considered."),
																												tags$br(),
																												h4(tags$b("3. Minimum number of  CN oscillations across 3 states.")),
																												h4("Given that considering oscillations across only 2 states is stringent in a number of cases due to the presence of low-level 
																												   CN amplifications (which break the uninterrupted CN oscillation pattern), we also considered the number of oscillations across 3 CN states."),
																												   tags$br(),
																												   h4(tags$b("4. Purity")),
																												   h4("Only consider tumors of higher purity than the threshold selected to calculate the chromothripsis rates."),
																												   tags$br(),
																												   h4(tags$b("5. Ploidy")),
																												   h4("Only consider tumors of higher ploidy that the purity threshold selected to calculate the rates"),																		    
																												   tags$br(),
																												   h4(tags$b("6. Number of chromosomes affected per tumor")),
																												   h4("Only consider tumors with chromothripsis in at least the number of chromosomes selected."),
																												   tags$br(),
																												   h4("Please click on the box 'Chromothripsis detection' above for further information about how the chromothripsis regions are classified."),
																												   circle = TRUE, status = "info", icon = icon("info-circle"), width = "1000px",
																												   tooltip = tooltipOptions(title = "Click here for details about the plot below !")),

																												withSpinner(plotOutput("plot_rates2",height="650px"))
																												)
																											 ),

																										  ##### ploidy panel
																										  fluidRow(sidebarPanel(width=12,h2("Relationship between ploidy, purity, CN oscillations and SVs involved in chromothripsis",
																																			style="font-style:'Open Sans';text-align: center;")
																										  #h4("The barplot on the right depicts the chromothripsis rates in the cancer types selected
																										  #    that satisfy the minimum values indicated below.",style="font-style:'Open Sans';text-align: center; text-justify: inter-word;")



																										  )),
																										  fluidRow(
																												   shiny::column(6, align="center", offset = 3,
																																 tags$button("Click here to expand",inputId = "bubbles", type = "button", 
																																			 class = "btn btn-info", `data-toggle` = "collapse", 
																																			 `data-target` = "#bubbles" ))),

																										  div(id="bubbles", class="collapse",
																											  fluidRow(



																													   # chromosome choices

																													   #sidebarPanel(width=12, 
																													   #selectizeInput( 'chrom', 'Chromosome', 
																													   #                choices = as.character(c(1:22,"X")), multiple = TRUE, 
																													   #                selected=as.character(c(1:22,"X") )),

																													   shiny::column(width=2,
																																	 dropdownButton(
																																					h3("Fraction of SVs involved in chromothripsis vs number of uninterrupted CN oscillations",style="face: bold;"),
																																					h4("Each bubble on the left-hand side plot represents the chromothripsis event detected (if any)
																																					   in a tumor and in a given chromosome (hence, for each tumor there are as many bubbles as chromosomes harboring chromothripsis). 
																																					   The size of the bubble is proportional to the total number of somatic SVs detected in the tumor."),
																																					   tags$br(),
																																					   h4("The x-axis corresponds to the fraction of SVs in the tumor that are involved in chromothripsis in a given chromosome. 
																																						  The y-axis represents the number of uninterrupted CN oscillations in the chromothripsis region."),
																																						  tags$br(),
																																						  h4("The menus on top of the plot permit to restrict the selection to a specific set of chromosomes,
																																							 and to only show events displaying a minimum number of CN oscillations or fraction of SVs in the tumor involved in chromothripsis.
																																							 Only the cancer types selected in the rates plot above are shown.",
																																							 style="font-style:'Open Sans';"),

																																						  tags$br(),
																																						  h3("Explore the purity and ploidy of the tumors",style="face: bold;"),  
																																						  h4("Each bubble corresponds to a tumor, being its size proportional
																																							 to the total number of somatic SVs detected in it. 
																																							 Only the cancer types selected in the rates plot above are shown.",style="font-style:'Open Sans';"),


																																							 circle = TRUE, status = "info", icon = icon("info-circle"), width = "1000px",
																																							 tooltip = tooltipOptions(title = "Click here for details about the plots below !"))

																																						  ),

																																					   shiny::column(width=2,
																																									 pickerInput( inputId = "chrom", label = "Chromosomes", choices = as.character(c(1:22,"X")),
																																												 selected=as.character(c(1:22,"X")),
																																												 options = list( `actions-box` = TRUE, size = 10, 
																																																style = "btn-info",
																																																`selected-text-format` = "count > 3" ), multiple = TRUE )
																																									 ),

																																					   shiny::column(width=3,
																																									 sliderInput("oscil2", "# CN oscillations between 2 states",
																																												 min = min(d[,"Nb. oscillating CN"],na.rm=T), 
																																												 max = 100,#max(d[,"Nb. oscillating CN"],na.rm=T),
																																												 value = 3, animate = TRUE)
																																									 ),
																																					   shiny::column(width=3,
																																									 sliderInput("fraction_SVs_chromo", "Fraction SVs in chromothripsis",
																																												 min = min(d[,"ratio"],na.rm=T), 
																																												 max = max(d[,"ratio"],na.rm=T),
																																												 value = 0, animate = TRUE)
																																									 )
																																					   #)
																																					   ),
																																					fluidRow(   
																																							 mainPanel(width=12,
																																									   shiny::column(width=6,
																																													 ### information
																																													 #actionButton( inputId = "info_bubble1", label = "Launch confirmation dialog" ),

																																													 withSpinner(googleBubbleChart("chart2",width="100%", height = "475px",
																																																				   options = list(
																																																								  fontSize = 13,
																																																								  #legend=list(position='none'),
																																																								  # Set axis labels and ranges
																																																								  hAxis = list(
																																																											   title = "Fraction SVs in tumor mapped to chromothripsis regions",
																																																											   viewWindow = list(-.3,1)
																																																											   ),
																																																								  vAxis = list(
																																																											   title = "# of uninterrupted oscillations between 2 CN states",
																																																											   viewWindow = list(-4,1000)
																																																											   ),
																																																								  # The default padding is a little too spaced out
																																																								  chartArea = list(
																																																												   top = 50, left = 75,
																																																												   height = "75%", width = "75%"
																																																												   ),
																																																								  # Allow pan/zoom
																																																								  explorer = list(),
																																																								  # Set bubble visual props
																																																								  bubble = list(
																																																												opacity = 0.4, stroke = "none",
																																																												# Hide bubble label
																																																												textStyle = list(
																																																																 color = "none"
																																																																 )
																																																												),
																																																								  # Set fonts
																																																								  titleTextStyle = list(
																																																														fontSize = 16
																																																														),
																																																								  tooltip = list(
																																																												 textStyle = list(
																																																																  fontSize = 12
																																																																  )
																																																												 )
																																																								  )
																																																				   ) #closes bubblechart 2

																																													 )),

																																									   shiny::column(width=6,

																																													 #       dropdownButton(
																																													 # 		  h5("Explore the purity and ploidy of the tumors. Each bubble corresponds to a tumor, being its size proportional
																																													 # 			 to the total number of somatic SVs detected in it.",style="font-style:'Open Sans';"),
																																													 # 		  circle = TRUE, status = "info", icon = icon("info-circle"), width = "550px",
																																													 # 		  tooltip = tooltipOptions(title = "Click here for details about the plot below !")),

																																													 withSpinner( googleBubbleChart("chart", 
																																																					width="100%", height = "475px",
																																																					options = list(
																																																								   fontSize = 13,

																																																								   # Set axis labels and ranges
																																																								   hAxis = list(
																																																												title = "Purity",
																																																												viewWindow = list(-.5,1.5)
																																																												),
																																																								   vAxis = list(
																																																												title = "Ploidy",
																																																												viewWindow = list(-.5,7)
																																																												),
																																																								   # The default padding is a little too spaced out
																																																								   chartArea = list(
																																																													top = 50, left = 75,
																																																													height = "75%", width = "75%"
																																																													),
																																																								   # Allow pan/zoom
																																																								   explorer = list(),
																																																								   # Set bubble visual props
																																																								   bubble = list(
																																																												 opacity = 0.4, stroke = 0.1,colour="black", #none
																																																												 # Hide bubble label
																																																												 textStyle = list(
																																																																  color = "none"
																																																																  )
																																																												 ),
																																																								   # Set fonts
																																																								   titleTextStyle = list(
																																																														 fontSize = 16
																																																														 ),
																																																								   tooltip = list(
																																																												  textStyle = list(
																																																																   fontSize = 12
																																																																   )
																																																												  )
																																																								   )
																																																					))  #closing first chart


																																													 )
																																									   )


																																							 )


																																					),

																																	 tags$br(),
																																	 fluidRow(sidebarPanel(width=12,h2("Find cases of interest quickly!",style="text-align: center"),
																																						   h4("Click on the button below to browse detailed information for chromsomes 1:22 and X for all PCAWG tumors.",style="text-align: center"),
																																						   h4("Use the filters on top of the columns to find tumors of your interest
																																							  and explore them interactively using the circos plots below.",style="text-align: center")
																																							  #HTML('<button data-toggle="collapse" data-target="#demo">Click to expand</button>')
																																							  #tags$button(class=materialSwitch(inputId = "demo", label = "Primary switch", status = "danger",
																																							  #`data-target` = "#demo"))


																																							  #h4("(e.g. breast tumors with canonical chromothripsis displaying at least 10 CN oscillations).",style="text-align: center"),
																																							  #h4("Then explore them interactively using the circos plots below!",style="text-align: center")
																																							  ),



																																						   shiny::column(6, align="center", offset = 3,
																																										 tags$button("Click here to expand",inputId = "demo", type = "button", 
																																													 class = "btn btn-info", `data-toggle` = "collapse", 
																																													 `data-target` = "#demo" )),
																																						   shiny::column(width=12,
																																										 div(id="demo",
																																											 DT::dataTableOutput("find_case"),  class="collapse",
																																											 style = "overflow-x: scroll"),
																																										 tags$br()

																																										 # # output cases for gene selection
																																										 # tags$head(tags$script(src="script_search.js")),
																																										 # ## link the CSS file
																																										 # tags$head(tags$link(rel="stylesheet", 
																																										 #                     type="text/css",
																																										 #                     href="style_search.css")),
																																										 # tags$div(id = 'placeholder2'),
																																										 # tags$em((paste('Making sure it worked (type', 
																																										 #                'in the box above and see', 
																																										 #                'your result here):'))),
																																										 # textOutput('txt2Test', inline = TRUE)

																																										 )
																																						   ),
																																			  ### circos
																																			  fluidRow(sidebarPanel(width=12,h2("Interactive circos plots reporting SNVs, 
																																												indels, total CNV, minor CN (LOH), and SV calls",
																																												style="text-align: center"))),


																																			  fluidRow(
																																					   #	 shiny::column(3,
																																					   sidebarPanel(width=3,
																																									wellPanel(
																																											  h5("The selection boxes below only affect the plots below them (not the bubble plots above)",
																																												 style="font-style:'Open Sans';"),
																																											  # type chromo
																																											  #selectizeInput( 'chromo_type', 'Chromothripsis categories?', 
																																											  #				choices = c("After polyploidization","Before polyploidization","Canonical without polyploidization","No chromothripsis","With other complex events"), multiple = TRUE, 
																																											  #					selected=c("After polyploidization","Before polyploidization","Canonical without polyploidization","With other complex events") ),
																																											  #tags$button(class='btn btn-info dropdown-toggle',
																																											  #tags$button(class="btn btn-info dropdown-toggle", #btn btn-info disabled",
																																											  #div(
																																											  pickerInput(inputId = "chromo_type", label = "Chromothripsis categories", 
																																														  choices = c("After polyploidization","Before polyploidization","Canonical without polyploidization","No chromothripsis","With other complex events"),
																																														  selected=c("After polyploidization","Before polyploidization","Canonical without polyploidization","With other complex events"),
																																														  options = list( `actions-box` = TRUE, `size`= 5, 
																																																		 style = "btn-info",
																																																		 `selected-text-format` = "count > 3" ), 
																																														  multiple = TRUE )
																																											  #)
																																											  ,


																																											  # actionButton("selectall", "Select all types:"),

																																											  uiOutput('cancer_type2'),
																																											  #textInput("text", "Enter string to search", "foo"),
																																											  #actionButton("go", "Search"),
																																											  uiOutput("donor_choice"),
																																											  uiOutput("chr_selection_circos"),




																																											  #),




																																											  #tags$br(),

																																											  #h3('Interactive circos plots reporting SNVs, indels, total CNV, minor CN (LOH), and SV calls')
																																											  #,
																																											  #fluidRow(

																																											  #sidebarPanel(width=3,
																																											  h5("Circos plot options",style="font-weight: bold;"),
																																											  checkboxInput( inputId='show_patho_indels', label='Show pathogenic indels',value = FALSE),
																																											  checkboxInput( inputId='show_nopatho_indels', label='Show non-pathogenic indels',value = FALSE),
																																											  checkboxInput( inputId='show_patho', label='Show pathogenic SNVs',value = FALSE),
																																											  # "nonsynonymous SNV","stopgain","stoploss","frameshift deletion","frameshift insertion"
																																											  checkboxInput( inputId='show_nopatho', label='Show non-pathogenic SNVs',value = FALSE),
																																											  # range for distanc
																																											  sliderInput(step = 50000,"range_dist", "Range intermutation distance:", 
																																														  min = 0, max = 6000000, value = c(0,1000000)),

																																											  # show genes?
																																											  checkboxInput( inputId='show_genes', label='Show gene track',value = TRUE),

																																											  # show chromothripsis track?
																																											  checkboxInput( inputId='show_chromo_track', label='Highlight chromothripsis regions',value = TRUE)#,

																																											  #   #checkboxGroupInput
																																											  #   selectizeInput( 'chr_selection_circos', 'Chromosome selection for circos plot', 
																																											  # 				 choices = as.character(c(1:22,"X")), 
																																											  # 				 multiple = TRUE, selected=as.character(c(1,22,"X")) )

																																											  ### do this with renderUI so the chrs that appear are those with chromothripsis
																																											  #uiOutput("chr_selection_circos")






																																											  #pickerInput(inline = F,multiple=T,'chr_selection_circos', options = list(`actions-box` = TRUE),
																																											  #            'Chromosome selection for circos plot', as.character(c(1:22,"X")),selected=as.character(c(22,"X")) )

																																											  )
																																									),
																																					   mainPanel(width=9,
																																								 div(style="display: inline-block;",
																																									 dropdownButton(
																																													div(style="display: inline-block;",
																																														tags$h3("Track information (from outside to inside)",style = "display: inline-block;")),

																																													# cytobands
																																													div( tags$h4(tags$b("Track 1. hg19 cytobands"))),

																																													# intermutation distance
																																													div(tags$h4(tags$b("Track 2. Intermutation distance"),style = "display: inline-block;"),
																																														tags$br(),                                         
																																														tags$h4("This track displays the intermutation distance for:
																																																(i) pathogenic indels (large green dots),
																																																(ii) nonpathogenic indels (small black dots), (iii) pathogenic SNVs (large blue dots), 
																																																and (iv) nonpathogenic SNVs (small dots coloured according to the type of substitution)
																																																and SNVs. 
																																																Pathogenic mutations are defined as 
																																																nonsynonymous, stopgain, stoploss, frameshift deletion,frameshift insertion. 
																																																The y-axis, which can be modified using the slider on the left-hand side, represents the distance to the next mutation in the genome.")),

																																																# chromothripsis region
																																																tags$br(),  
																																																div(tags$h4(tags$b("Track 2. Chromothripsis regions"),style = "display: inline-block;"),
																																																	tags$h4("High-confidence chromothripsis regions are indicated by a yellow bar on the same track as the SNVs and indels.
																																																			Low-confidence regions are indicated with a green bar. Please see the tables below the circos plot for further information.")),

																																																			# CN
																																																			tags$br(),  
																																																			div(tags$h4(tags$b("Track 3. Total copy number (CN)"),style = "display: inline-block;"),
																																																				tags$h4("The total CN values are displayed in black. This track has a light blue background.")),

																																																			# minor CN
																																																			tags$br(),  
																																																			div(tags$h4(tags$b("Track 4. Minor copy number"),style = "display: inline-block;"),
																																																				tags$h4("Loss-of-heterozygosity (LOH) regions 																																		  are shown in red. This track has a grey background.")),

																																																			# genes 
																																																			tags$br(),  
																																																			div(tags$h4(tags$b("Track 5. Gene annotations"),style = "display: inline-block;"),
																																																				tags$h4("Tumor suppressors and oncogenes are displayed in blue and red, respectively.")),

																																																			# SVs 
																																																			tags$br(),  
																																																			div(tags$h4(tags$b("Track 6. Structural variations. "),style = "display: inline-block;"),
																																																				tags$h4("Duplication-like SVs, deletion-like SVs, head-to-head and tail-to-tail inversions 
																																																						are depicted in blue, orange, black, and green, respectively.")),

																																																						# NOTE
																																																						tags$br(),  
																																																						div(tags$h4(tags$b("NOTE"),style = "display: inline-block;"),
																																																							tags$h4("Please note that if you display the SNVs/indels for multiple chromosomes at the same time the plot
																																																									might take a few seconds to load. Please be patient.. Only the chromosomes harboring chromothripsis are shown
																																																									for the selected tumor (other chromosomes can be selected by using the menu on the
																																																															left-hand side).")),



																																																									circle = TRUE, status = "info", icon = icon("info-circle"), width = "950px",
																																																									tooltip = tooltipOptions(title = "Track details",placement="bottom"))),

																																																						#includeHTML("test_igv.html"),
																																																						div(style="display: inline-block; padding: 10px;",
																																																							dropdownButton(
																																																										   div( withSpinner(DT::dataTableOutput("table_donor_info")), 
																																																											   style = "overflow-x: scroll;"),
																																																										   circle = TRUE, status = "info", icon = icon("user-md"), width = "500px",
																																																										   tooltip = tooltipOptions(title = "Patient information and clinical data")
																																																										   )),

																																																						withSpinner(BioCircosOutput("biocirc", height='900px',width="1300px")
																																																									#plotOutput("plot_chromo_gg",height='600px')
																																																									#)
																																																									))
																																																				),



																																																			fluidRow(sidebarPanel(width=12,h2("Patient information, values for the statistical metrics, and additional information",style="text-align: center"))),
																																																			#   fluidRow(#offset=1,width=11,
																																																			# 		   shiny::column(width=12,
																																																			# 
																																																			# 		                 
																																																			# 						 div( withSpinner(DT::dataTableOutput("table_donor_info")), 
																																																			# 							 style = "overflow-x: scroll")
																																																			# 						 )
																																																			# 		   ),

																																																			#tags$br(),
																																																			#tags$br(),tags$br(),

																																																			# 
																																																			#tags$br(),

																																																			fluidRow(shiny::column(width=2,#uiOutput('cols_to_show')
																																																								   pickerInput('cols_to_show', 'Select chromosomes',
																																																											   selected=as.character(c(1:22,"X")),
																																																											   choices=as.character(c(1:22,"X")),
																																																											   options = list( `actions-box` = F, `size`= 5,
																																																															  style = "btn-info",
																																																															  `selected-text-format` = "count > 3" ),
																																																											   multiple = TRUE )
																																																								   )),

																																																			fluidRow(#offset=1,width=11,
																																																					 shiny::column(width=12,
																																																								   class="table-hover",
																																																								   div(
																																																									   withSpinner( DT::dataTableOutput("mytable2")), style = 'overflow-x: scroll') 
																																																								   )
																																																					 )
																																																			#----------------------------------


																																																			),


																																																	# # ------------------------------------------------------------------------------------------------
																																																	# # Acknowledgments
																																																	# # ------------------------------------------------------------------------------------------------
																																																	tabPanel(title=div(style="font-size: 30px;",icon("envelope"),id="acknow"),
																																																			 h3("Acknowledgements"),
																																																			 tags$br(),
																																																			 h4("The results published here are partly based upon data generated by The Cancer 
																																																				Genome Atlas and obtained from the Database of Genotypes and Phenotypes (dbGaP) with accession number phs000178.v8.p7.",
																																																				style="font-style:'Open Sans';  width: 80%; display: block; align: center; margin-left: 7em;"),
																																																			 tags$br(),
																																																			 h4("Information about TCGA can be found at http://cancergenome.nih.gov. We thank the Research
																																																				Information Technology Group at Harvard Medical School for providing computational resources.
																																																				This work was supported by grants from the Ludwig Center at Harvard (I.C.C., J.K.L., and P.J.P.).",
																																																				style="font-style:'Open Sans';  width: 80%; display: block; align: center; margin-left: 7em;"),
																																																			 tags$br(),
																																																			 h4("This project has received funding from the European Union’s Framework Programme 
																																																				For Research and Innovation Horizon 2020 (2014-2020) under the Marie Curie Sklodowska-Curie 
																																																				Grant Agreement No. 703543 (I.C.C.).",
																																																				style="font-style:'Open Sans';  width: 80%; display: block; align: center; margin-left: 7em;"),
																																																			 tags$br(),

																																																			 h4("This work was performed as part of the PCAWG Structural Variation Working Group.",
																																																				style="font-style:'Open Sans';  width: 80%; display: block; align: center; margin-left: 7em;"),
																																																			 tags$br(),
																																																			 h4("This site was designed by Isidro Cortes-Ciriano (isidrolauscher at gmail.com).",
																																																				style="font-style:'Open Sans';  width: 80%; display: block; align: center; margin-left: 7em;"),
																																																			 tags$a(href="https://github.com/scottx611x", "Shiny App deployment by Scott Oullette",
																																																					style="font-style:'Open Sans';  width: 80%; display: block; align: center; margin-left: 9em;")


																																																			 ),


																																																	##### SI ------------------------------------------------------------------------------------------------
																																																	# ------------------------------------------------------------------------------------------------
																																																	tabPanel(div("Supplementary Information",
																																																				 class="nav navbar-nav navbar-right", 
																																																				 style="height: 30px; line-height: 30px; text-align: center; font-style: 'Open Sans;"),


																																																			 div(includeHTML("expanding-card-grid-with-flexbox/SI.html"),
																																																				 includeCSS("expanding-card-grid-with-flexbox/css/style2.css"),
																																																				 includeCSS("expanding-card-grid-with-flexbox/scss/style.scss"),
																																																				 includeScript("expanding-card-grid-with-flexbox/js/index.js")),

																																																			 h4("In all Supplementary Data Files intrachromosomal SVs are depicted as arcs with the breakpoints represented by black points,
																																																				whereas the breakpoints corresponding to interchromosomal SVs are depicted as colored points. Duplication-like SVs, deletion-like SVs,
																																																				head-to-head and tail-to-tail inversions are depicted in blue, orange, black, and green, respectively. The value for the statistical
																																																				criteria described above for each event is provided below its representation.",style="font-style:'Open Sans';  width: 80%; display: block; align: center; margin-left: 8em;")

																																																				)
																																																			 ), div(class="footer",includeHTML("freebie-footer-templates/footer-distributed-with-address-and-phones.html"),
																																																			 includeCSS("freebie-footer-templates/css/footer-distributed-with-address-and-phones.css")) 
																																																	)
																																																#))
																																																)
