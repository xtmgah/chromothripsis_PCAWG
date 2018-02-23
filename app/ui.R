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

appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"

choices_cancer = sort(unique(d$histo)) 
#----------------------------------------------------------------------------------
shinyUI(
    fluidPage(theme = shinytheme("sandstone"), 
          tags$link(
              rel = "stylesheet", 
              href="http://fonts.googleapis.com/css?family=Open+Sans"
              ),        

          tags$body(style="font-family: 'Open Sans';"),
          #useShinyjs(),
          #inlineCSS(appCSS),
          
          ## Loading message
          #div(
          #    id = "loading-content",
          #    h1("Chromothripsis explorer is loading.. Please wait for a few seconds..",style=("font-family: 'Open Sans';"))
          #    ),

          #hidden(div(id="app-content",
               navbarPage(title=div("Chromothipsis in human cancers",responsive = T,
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
                         fluidRow(
                              #### input to control barplot
                              shiny::column(width=3,
                                    h5("The barplot on the right depicts the chromothripsis rates in the cancer types selected
                                       that satisfy the minimum values indicated below.",style="font-style:'Open Sans';text-align: justify; text-justify: inter-word;"),

                                       sliderInput("min_nb_oscil", "Mininum number of CN oscillations",
                                             min = 3, max = 600,
                                             value = 4, animate = TRUE),


                                       sliderInput("breakpoints_cluster", "Brekpoints in cluster (including interchr SVs)",
                                             min = 6, max = 1824,
                                             value = 0, animate = TRUE),
                                       sliderInput("nb_chrs_affected", "Number of chromosomes affected per tumor",
                                             min = 1, max = 23,
                                             value = 1, animate = TRUE)
                                       ),
                                    shiny::column(width = 9,
                                            withSpinner(plotOutput("plot_rates"))
                                            )
                                    ),

                              fluidRow( 
                                   sidebarPanel(

                                        h5("Please select the set of cancer types you want to explore.
                                           The cancer type selection affects the barplot showing the chromothripsis rates,
                                           as well as the two bubble plots on the right side.",style="text-align: justify; text-justify: inter-word;"),

                                           ## Select cancer type
                                           checkboxGroupInput(inline = F,'cancer_type', 'Cancer type',
                                                    choiceNames = c(choices_cancer,"Select all", "Clear selection"),  
                                                    c(choices_cancer,"Select all","Clear selection"), selected="Biliary-AdenoCA")
                                           ),

                                        mainPanel( 
                                              h5("Explore the purity and ploidy of the tumors. Each bubble corresponds to a tumor, being its size proportional
                                               to the total number of somatic SVs detected in it.",style="font-style:'Open Sans';"),
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
                                                         )), #closing first chart

                                               fluidRow(
                                                    shiny::column(4, offset = 1,
                                                          sliderInput("purity", "Purity",
                                                                min = min(d$purity,na.rm=T), max = max(d$purity,na.rm=T),
                                                                value = min(d$purity,na.rm=T), animate = TRUE)
                                                          ),
                                                    shiny::column(4, offset = 1,
                                                          sliderInput("ploidy", "Ploidy",
                                                                min = min(d$ploidy,na.rm=T), max = max(d$ploidy,na.rm=T),
                                                                value = min(d$ploidy), animate = TRUE)
                                                          )
                                                    ),


                                               h5("Explore the chromothripsis events detected in the tumors of the types selected on the left-hand menu.
                                                Note that chromosomes without chromothripsis events are not shown.
                                                Each bubble corresponds to a chromosome (you can select a specific set of chromosomes below),
                                                and their size is proportional to the number of somatic SVs detected in the tumor (all chromosomes considered)",
                                                tyle="font-style:'Open Sans';"),


                                               h5("The y axis represents the number of uninterrupted CN oscillations in the chromothripsis region. 
                                                The x axis shows corresponds to fraction of SVs involved in chromothripsis in a given chromosome with respect
                                                to the total number of SVs detected in the tumor.",
                                                style="font-style:'Open Sans';"),

                                               withSpinner(googleBubbleChart("chart2",width="100%", height = "475px",
                                                         options = list(
                                                                fontSize = 13,
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

                                               ))
                                              ), 

                                        # chromosome choices
                                        fluidRow(
                                             shiny::column(3, offset = 1,
                                                     selectizeInput( 'chrom', 'Chromosome', 
                                                            choices = as.character(c(1:22,"X")), multiple = TRUE, 
                                                            selected=as.character(c(1:22,"X") ))
                                                     ),

                                             shiny::column(3, offset = 1,
                                                     sliderInput("oscil2", "# CN oscillations between 2 states",
                                                           min = min(d[,"Nb. oscillating CN"],na.rm=T), 
                                                           max = max(d[,"Nb. oscillating CN"],na.rm=T),
                                                           value = 3, animate = TRUE)
                                                     ),
                                             shiny::column(3, offset = 0,
                                                     sliderInput("fraction_SVs_chromo", "Fraction of SVs in chromothripsis region",
                                                           min = min(d[,"ratio"],na.rm=T), 
                                                           max = max(d[,"ratio"],na.rm=T),
                                                           value = 0, animate = TRUE)
                                                     )

                                             ),
                                        fluidRow(
                                             shiny::column(4,
                                                     wellPanel(
                                                         h5("The selection boxes below only affect the plots below them (not the bubble plots above)",
                                                          style="font-style:'Open Sans';"),
                                                         # type chromo
                                                         selectizeInput( 'chromo_type', 'Chromothripsis categories?', 
                                                                choices = c("After polyploidization","Before polyploidization","Canonical without polyploidization","No chromothripsis","With other complex events"), multiple = TRUE, 
                                                                selected=c("After polyploidization","Before polyploidization","Canonical without polyploidization","With other complex events") ),

                                                         uiOutput('cancer_type2'),
                                                         uiOutput("donor_choice")
                                                         )
                                                     ),

                                             shiny::column(7, 
                                                     wellPanel( 
                                                         h4("Select the type of chromothripsis events and cancer types youw ant to explore on the left-hand side menu.",style="text-align: justify; text-justify: inter-word;"),
                                                         tags$br(),

                                                         h4("An interactive and highy customizable circos plot permits the exploration of SNV, indels, CN and SV profiles. Please click on the information button for further details about the tracks. Depending on the number of SNVs, indels or SVs detected in the selected tumor the tracks might take a few seconds to load; please be patient.",
                                                          style="text-align: justify; text-justify: inter-word;"),

                                                         tags$br(),
                                                         h4("Finally, two tables show information about the selected patient, comprehensive information about values for the statistical criteria used to detect chromothripsis for all chromosomes in the selected tumor, as well as additional information. Each column corresponds to one chromosome.",style="text-align: justify; text-justify: inter-word;")

                                                         )

                                                     )
                                             ),



                                        tags$br(),

                                        h3('Interactive circos plots reporting SNVs, indels, total CNV, minor CN (LOH), and SV calls')
                                        ,
                                        fluidRow(

                                             sidebarPanel(width=3,h5("Circos plot options",style="font-weight: bold;"),
                                                    checkboxInput( inputId='show_patho_indels', label='Show pathogenic indels',value = FALSE),
                                                    checkboxInput( inputId='show_nopatho_indels', label='Show non-pathogenic indels',value = FALSE),
                                                    checkboxInput( inputId='show_patho', label='Show pathogenic SNVs',value = FALSE),
                                                    # "nonsynonymous SNV","stopgain","stoploss","frameshift deletion","frameshift insertion"
                                                    checkboxInput( inputId='show_nopatho', label='Show non-pathogenic SNVs',value = FALSE),
                                                    # range for distanc
                                                    sliderInput(step = 500000,"range_dist", "Range intermutation distance:", 
                                                          min = 0, max = 6000000, value = c(0,1000000)),

                                                    # show genes?
                                                    checkboxInput( inputId='show_genes', label='Show gene track',value = TRUE),

                                                    # show chromothripsis track?
                                                    checkboxInput( inputId='show_chromo_track', label='Highlight chromothripsis regions',value = TRUE),

                                                    #checkboxGroupInput
                                                    selectizeInput( 'chr_selection_circos', 'Chromosome selection for circos plot', 
                                                           choices = as.character(c(1:22,"X")), 
                                                           multiple = TRUE, selected=as.character(c(1,22,"X")) )

                                                    #pickerInput(inline = F,multiple=T,'chr_selection_circos', options = list(`actions-box` = TRUE),
                                                    #            'Chromosome selection for circos plot', as.character(c(1:22,"X")),selected=as.character(c(22,"X")) )


                                                    ),
                                             mainPanel(width=9,
                                                   dropdownButton(
                                                          div(
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
                                                                  tags$h4("Chromothripsis regions are indicated by a yellow bar on the same track as the SNVs and indels.")),

                                                                # CN
                                                                tags$br(),  
                                                                div(tags$h4(tags$b("Track 3. Total copy number (CN)"),style = "display: inline-block;"),
                                                                  tags$h4("The total CN values are displayed in black. This track has a light blue background.")),

                                                                # minor CN
                                                                tags$br(),  
                                                                div(tags$h4(tags$b("Track 4. Minor copy number"),style = "display: inline-block;"),
                                                                  tags$h4("Loss-of-heterozygosity (LOH) regions                                                                       are shown in red. This track has a grey background.")),

                                                                      # genes 
                                                                      tags$br(),  
                                                                      div(tags$h4(tags$b("Track 5. Gene annotations"),style = "display: inline-block;"),
                                                                        tags$h4("Tumor suppressors and oncogenes are displayed in blue and red respectively.")),

                                                                      # SVs 
                                                                      tags$br(),  
                                                                      div(tags$h4(tags$b("Track 6. Structural variations. "),style = "display: inline-block;"),
                                                                        tags$h4("Duplication-like SVs, deletion-like SVs, head-to-head and tail-to-tail inversions 
                                                                            are depicted in blue, orange, black, and green, respectively.")),

                                                                            # NOTE
                                                                            tags$br(),  
                                                                            div(tags$h4(tags$b("NOTE"),style = "display: inline-block;"),
                                                                              tags$h4("Please note that if you display the SNVs/indels for multiple chromosomes at the same time the plot
                                                                                  might take a few seconds to load. Please be patient.. Only chromosomes 1, 22 and X are selected
                                                                                  when launching the site to reduce loading times (please adjust your selection using the menu on the
                                                                                                           left-hand side).")),



                                                                                  circle = TRUE, status = "danger", icon = icon("info-circle"), width = "1100px",
                                                                                  tooltip = tooltipOptions(title = "Click here for details about the tracks !")),


                                                   withSpinner(BioCircosOutput("biocirc", height='900px',width="1000px")
                                                                              #plotOutput("plot_chromo_gg",height='600px')
                                                                              #)
                                                                              ))
                                                                            ),


                                                                        #----------------------------------
                                                                        div(HTML('<h2>Patient information</h2>')),
                                                                        fluidRow(#offset=1,width=11,
                                                                             shiny::column(width=12,
                                                                                   div(DT::dataTableOutput("table_donor_info"), 
                                                                                     style = "overflow-x: scroll"))
                                                                             ),

                                                                        tags$br(),
                                                                        h2('Values for the statistical metrics and additional information for chromosomes 1-22 and X'),
                                                                        # 
                                                                        tags$br(),
                                                                        fluidRow(#offset=1,width=11,
                                                                             shiny::column(width=12,
                                                                                   class="table-hover",
                                                                                   div(DT::dataTableOutput("mytable2"), style = 'overflow-x: scroll'))##style = "font-size:80%;"))
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
                                                                             style="height: 30px; line-height: 30px; text-align: center; font-style: 'Open Sans"),


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