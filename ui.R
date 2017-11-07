library(grid)
library(gridExtra)
library(plyr)
#require("ggrepel")



#d= readRDS("calls_all_with_all_info.rds")
#d=readRDS("cands_final.rds")
source("load_data.R")
#library(DT)
d$Chr = d$chrom
# pageWithSidebar(
#   headerPanel('Chromothripsis in PCAWG'),
#   sidebarPanel(
#     selectInput('donor', 'Donor ID', unique(d$donor_unique_id)[1:10]),
#     selectInput('chromosome', 'Chromosome', unique(d$Chr),
#                 selected=names(iris)[[2]]),
#     numericInput('clusters', 'Cluster count', 3,
#                  min = 1, max = 9)
#   ),
#   mainPanel(
# 
#                plotOutput('plot1')
# 
# 
#   )
# )

shinyUI(fluidPage(
  

  
  HTML('<br><div style="margin-top: 20px; margin-bottom:20px;"><h2 style="background-color: WhiteSmoke;font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  text-align: center">
     <hr>Comprehensive analysis of chromothripsis in 2600 human cancers using whole-genome sequencing<hr></div>'),
  

  
  # #  titlePanel("Chromothripsis Pan-Cancer Analysis of Whole Genomes (PCAWG)",)
  #    tags$div(class = "header", checked = NA,
  #             #tags$style(HTML(' body {  font-family: "Calibri";}')),
  #             tags$h1("Comprehensive analysis of chromothripsis in 2600 human cancers using whole-genome sequencing", align = "center")
  # #             tags$h5("Isidro Cortés-Ciriano<sup>1,2,3</sup>, June-Koo Lee1,2, Ruibin Xi4, Dhawal Jain1, Youngsook L. Jung1, 
  # #  Dmitry Gordenin5, Leszek. J. Klimczak6, Cheng-Zhong Zhang1,7, David S. Pellman7,8, Peter J. Park1,2,* 
  # #  on behalf of the PCAWG Structural Variation Working Group and the ICGC/TCGA Pan-Cancer Analysis of Whole Genomes Network", align='center')
  # ),
  
  
  HTML('<hr><div style= "text-align: center;margin-left: 10%;margin-right: 10%;">
<h3  style="text-align: center;  line-height: 30px; font-family: Vollkorn;"> <br></br>
Isidro Cortés-Ciriano<sup>1,2,3</sup>, June-Koo Lee<sup>1,2</sup>, Ruibin Xi<sup>4</sup>, Dhawal Jain<sup>1</sup>, Youngsook L. Jung<sup>1</sup>, 
Dmitry Gordenin<sup>5</sup>, Leszek. J. Klimczak<sup>6</sup>, Cheng-Zhong Zhang<sup>1,7</sup>, David S. Pellman<sup>7,8</sup>, Peter J. Park<sup>1,2,*</sup> 
on behalf of the PCAWG Structural Variation Working Group and the ICGC/TCGA Pan-Cancer Analysis of Whole Genomes Network</h3></div><hr>'),
  
  
  
  HTML('<div><h4 style="text-align: center; font-family: Vollkorn;"> 
<p>1 Department of Biomedical Informatics, Harvard Medical School, Boston, Massachusetts, USA</p>
<p>2 Ludwig Center at Harvard, Boston, MA 02115, USA</p>
<p>3 Centre for Molecular Science Informatics, Department of Chemistry, University of Cambridge, Lensfield Road, Cambridge CB2 1EW, United Kingdom</p>
<p>4 School of Mathematical Sciences and Center for Statistical Science, Peking University, Beijing 100871, China</p>
<p>5 Genome Integrity and Structural Biology Laboratory</p>
<p>6 Integrative Bioinformatics Group, National Institute of Environmental Health Sciences, US National Institutes of Health, Research Triangle Park, North Carolina, USA</p>
<p>7 Department of Pediatric Oncology, Dana-Farber Cancer Institute, Boston, Massachusetts 02215, USA</p>
<p>8 Howard Hughes Medical Institute and Department of Cell Biology, Harvard Medical School, Boston, Massachusetts 02115, USA</p>
<p>* Correspondence should be addressed to P.J.P. (peter_park@hms.harvard.edu)</p>

</h4><br></br></div>'),
  
  
  HTML('<br><div style="margin-top: 20px; margin-bottom:20px;"><h2 style="background-color: WhiteSmoke;font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  text-align: center">
     <hr>Data and workflow followed for the detection of chromothripsis<hr></div>'),
  
  
  #### Abstract
  HTML('<br><div style="margin-top: 20px; margin-bottom:20px;"><h3 style="background-color: WhiteSmoke;font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  line-height: 30px;text-align: center;margin-left: 10%;margin-right: 10%;">
     <hr>
Chromothripsis is a newly discovered mutational process involving massive, clustered genomic rearrangements that occurs
in cancer, human congenital diseases, and other contexts. Recent studies in cancer suggest that chromothripsis may be 
far more common than initially inferred from low resolution DNA copy number data. Here, we analyze the patterns of 
chromothripsis across of 2,658 tumors spanning 39 cancer types using whole-genome sequencing data. 
We find that chromothripsis events are pervasive across human cancers, with >50% frequency in several cancer types.
Instead of simple oscillations between two copy number states, a large fraction of the events involve multiple chromosomes as
well as additional structural alterations that give rise to complex copy number profiles. We frequently detect signatures of 
replicative processes and templated insertions in chromothripsis events displaying oscillations across two or more copy number states. 
In addition, chromothripsis contributes to oncogene amplification as well as to inactivation of genes such as mismatch-repair related genes 
whose loss promotes tumor development. 
Thus, chromothripsis is a major process driving genome evolution in human cancer.</h3><hr></div>'),
  #### Abstract
  
  
  HTML('<center><img src="fig1.png" width="90%",height="90%"></center>'),
  
  
  ##### SI ------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------
  HTML('<br><div style="margin-top: 20px; margin-bottom:20px;"><h2 style="background-color: WhiteSmoke;font-family: Vollkorn; margin-top: 20px; margin-bottom:20px;  text-align: center"> 
<hr><p style="font-family: Vollkorn; line-height: 30px;text-align: center;margin-left">
     Supplementary Information</p><hr></div>'),
  
  HTML('<div style="margin-top: 20px; margin-bottom:20px;"><h3 style=" font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  line-height: 30px;text-align: center;margin-left: 10%;margin-right: 10%;"> The information for all tumors examined, as well as the values for the statistical criteria used are listed in
    <a href="Table_S1.txt" download>Supplementary Table 1</a></h3> <hr>'),
  
  # --- Supplementary Data files ---- 
  HTML('<div style="margin-top: 20px; margin-bottom:20px;"><h3 style=" font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  line-height: 30px;text-align: center;margin-left: 10%;margin-right: 10%;">
     The following files contain a visual depiction of the chromothripsis events detected ETC as referenced in our manuscript</h3>'),
  HTML('<a href="Data_Files/Data_File_1.pdf" download><h3 style=" font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  line-height: 30px;text-align: center;margin-left: 10%;margin-right: 10%;">Supplementary Data File 1: High-confidence chromothripsis calls</h3></a>'),
  HTML('<a href="Data_Files/Data_File_2.pdf" download><h3 style=" font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  line-height: 30px;text-align: center;margin-left: 10%;margin-right: 10%;">Supplementary Data File 2: Low-confidence chromothripsis calls</h3></a>'),
  HTML('<a href="Data_Files/Data_File_3.pdf" download><h3 style=" font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  line-height: 30px;text-align: center;margin-left: 10%;margin-right: 10%;" >Supplementary Data File 3: True negative chromothripsis calls comprising clusters of tandem duplications or deletions</h3></a>'),
  HTML('<a href="Data_Files/Data_File_4.pdf" download><h3 style=" font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  line-height: 30px;text-align: center;margin-left: 10%;margin-right: 10%;" >Supplementary Data File 4: Large clusters of interleaved SVs not identified as chromothripsis by our method</h3></a>'),
  HTML('<a href="Data_Files/Data_File_5.pdf" download><h3 style=" font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  line-height: 30px;text-align: center;margin-left: 10%;margin-right: 10%;" >Supplementary Data File 5: Regions displaying oscillating CN segments and without SVs mapped therein</h3></a>'),
  

HTML('<br><div style="margin-top: 20px; margin-bottom:20px;"><h2 style="background-color: WhiteSmoke;font-family: Vollkorn; margin-top: 20px; margin-bottom:20px;  
text-align: center"><hr>Explore the chromothripsis events detected across the entire PCAWG cohort<hr></h2></div>'),
  
# fluidRow( 
#     column(2,
#            wellPanel(
#              selectInput('cancer_type', 'Cancer type', c("All",unique(dd$histo))),
#              selectInput('donor', 'Donor ID', unique(d$donor_unique_id)),
#              #selectizeInput('donor2', 'Donor ID', unique(d$donor_unique_id), selected = NULL, multiple = FALSE,options = NULL),
#              selectInput('chromosome', 'Chromosome', as.character(c(1:22,"X")),
#              selectInput('type_chromo', 'Chromothripsis category', c("All",unique(dd$histo)))
#              )
#            )
#            ),
#     column(10,#plotOutput("plot_chromo", height = 800))
#            DT::dataTableOutput("mytable2"))  
# ),


fluidRow( 
  column(4,
         wellPanel(
           
           selectInput('cancer_type', 'Cancer type', unique(d$histo)),
           #selectInput('donor', 'Donor ID', unique(d$donor_unique_id[which(d$histo == input$cancer_type)]))
           uiOutput("donor_choice"),
           #selectizeInput('donor2', 'Donor ID', unique(d$donor_unique_id), selected = NULL, multiple = FALSE,options = NULL),
           selectInput('chromosome', 'Chromosome', as.character(c(1:22,"X")),
                       selectInput('type_chromo', 'Chromothripsis category', c("All",unique(d$histo)))
           )
         )
  ),
  column(8, HTML('<div style="margin-top: 20px; margin-bottom:20px;"><h3 style="font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  line-height: 30px;text-align: justify;margin-left: 10%;margin-right: 10%;"> 
Select a cancer type and tumor of interest on the left-hand side menu.
<br>
<br>
The first table below displays information about the patient,
whereas the second table provides
the values for the statistical criteria used to detect chromothripsis, as well as additional
information associated to the chromothripsis regions and clusters of SVs identified (e.g., confidence and type
of chromothripsis event). Each column corresponds to one chromosome.
<br>
<br>
Finally, a circos plot of the entire genome is provided, 
as well as a depiction of the 
chromosome selected on the left-hand menu.
    </h3></div>'))
  ),

HTML('<hr>'),

HTML('<div><h2>Patient information</h2></div>'),
fluidRow( 
  column(width=12,
         DT::dataTableOutput("table_donor_info")) 
),

HTML('<hr>'),
HTML('<br><div><h2>Values for the statistical metrics across chromosomes 1-22 and X</h2></div>'),

fluidRow( 
    column(width=12, 
           DT::dataTableOutput("mytable2")) 
  ),



HTML('<hr>'),
HTML('<br><div><h2>Circos plot and chromosome-level view of CNV and SVs</h2></div>'),

  
fluidRow(
    column(width = 12, class = "well",
           #h4("The left plot represents the entire chromosome, whereas the left plot depicts the region affected by chromothripsis (if any)"),
           fluidRow(
             column(width = 6,
                    #plotOutput("test", height = 800)
                    imageOutput("myImage")
             ),
             column(width = 6,
                    plotOutput("plot_chromo", height = 800)
             )
           )
    )
),
  
  
  
  
  # ------------------------------------------------------------------------------------------------
  HTML('<br><div style="margin-top: 20px; margin-bottom:20px;"><h2 style="background-color: WhiteSmoke;font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  text-align: center">
     <hr><p style="font-family: Vollkorn; line-height: 30px;text-align: center;margin-left">
     Acknowledgements</p><hr></div>'),
  
  HTML('<hr><div><h3 style=" font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  line-height: 30px;text-align: center;margin-left: 15%;margin-right: 15%;">The results published here are partly based upon data generated by The 
Cancer Genome Atlas and obtained from the Database of Genotypes and Phenotypes (dbGaP) with accession number phs000178.v8.p7.
<br><br>
Information about TCGA can be found at http://cancergenome.nih.gov. We thank the Research Information Technology Group at 
Harvard Medical School for providing computational resources. This work was supported by grants from the Ludwig Center at Harvard (I.C.C. and P.J.P.). 
<br><br>
This project has received funding from the European Union’s Framework Programme For Research and Innovation Horizon 2020 (2014-2020) under 
the Marie Curie Sklodowska-Curie Grant Agreement No. 703543 (I.C.C.).
</h3>
 </div>
     <hr><p style="text-align: center;font-family: Vollkorn;"> This site was designed by Isidro Cortes-Ciriano</p><br></br>')
  
  
  
)
)

