library(grid)
library(gridExtra)
library(plyr)
library(DT)
source("load_data.R")
d$Chr = d$chrom


shinyUI(fluidPage(
  navbarPage("Chromothipsis analysis across 2,658 whole genomes",
             
             
             
             tabPanel("Home",
footer=HTML('<hr><div style="margin-top: 20px; margin-bottom:20px;"><h4 style="background-color: WhiteSmoke;font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  text-align: center">Footer</h4></div>'),
                
HTML('<br><div style="margin-top: 20px; margin-bottom:20px;"><h2 style="background-color: WhiteSmoke;font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  text-align: center">
     <hr>Companion site for:<br><br>Comprehensive analysis of chromothripsis in 2,658 human cancers using whole-genome sequencing<hr></div>'),
  
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

#### Abstract
HTML('<br><div><h3 style="background-color: WhiteSmoke;font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  line-height: 30px;text-align: center;margin-left: 10%;margin-right: 10%;
padding-top: 50px;
    padding-right: 80px;
    padding-bottom: 50px;
    padding-left: 80px;">
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
Thus, chromothripsis is a major process driving genome evolution in human cancer.</h3><hr></div>')

  
             ),

##-------------
tabPanel("Workflow",
  
  HTML('<br><div style="margin-top: 20px; margin-bottom:20px;"><h2 style="background-color: WhiteSmoke;font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  text-align: center">
     <hr>Data and workflow followed for the detection of chromothripsis<hr></div>'),
#### Abstract
HTML('<center><img src="fig1.png" width="90%",height="90%"></center>'),

HTML('<br><div #style="margin-top: 20px; margin-bottom:20px;"
><h3 style="background-color: WhiteSmoke;font-family: Vollkorn; 
margin-top: 20px; margin-bottom: 20px;  line-height: 30px;text-align: justify;margin-left: 10%; margin-right: 10%;
padding-top: 50px;
    padding-right: 80px;
    padding-bottom: 50px;
    padding-left: 80px; ">
After removing low-quality samples using stringent quality control criteria,
we applied our chromothripsis detection method to 2,543 tumor-normal pairs spanning 37
cancer types.
Of these, 2,428 cases harbored SVs and were considered for further analysis. 
The SVs were identified by the ICGC SV subgroup, which applied four algorithms 
and selected those SVs found by at least two algorithms. 
<br><br>
To identify chromothripsis-like patterns in the cancer genomes, 
we extended the set of statistical criteria proposed by Campbell and Korbel (Cell, 2013).
Given that chromothripsis events generate clusters 
of interleaved rearrangements (i.e., the genomic regions bridged by their breakpoints overlap but are not nested), 
we firstly scanned the each chromosome in each cancer genome for the presence of clusters of interwoven SVs.
To find clusters, we constructed an undirected graph whose nodes correspond to SVs and whose 
edges connect interleaved SVs. Thus, clusters of SVs can be detected by finding the connected components in the graph. 
The connected component in each chromosome with the highest number of SVs was considered for further analysis. 
We confined our analyses to the entire mappable genome with the exception of chromosome Y.
<br><br>
Once the SV clusters were detected, we tested whether the distribution of DNA fragment joints (i.e., duplication-like, deletion-like, 
head-to-head and tail-to-tail inversions) diverged from a multinomial distribution with equal probabilities for each category 
using the goodness-of-fit test for the multinomial distribution (chiq.test function from the R package stats), as described by 
Korbel and Campbell (fragment joints test). In addition, we used the binomial test corrected for 
mappability to evaluate the enrichment of SVs in each chromosome (chromosomal enrichment test). We also evaluated whether the 
distribution of breakpoints differed from an exponential distribution as described by Korbel and Campbell 
(exponential distribution of breakpoints test). 
<br><br>
The genomic regions delimited by the distal breakpoints composing 
the clusters of interleaved SVs were further examined for the presence of contiguous genomic segments oscillating between two CN states. 
To tune the parameters in our method, we used statistical thresholds and visual inspection. 
For the minimum number of oscillating CN segments, we used two thresholds:
<br><br>
- high-confidence calls display uninterrupted oscillations between two states in at least 7 adjacent segments
<br>
- low-confidence calls involved between 4 and 6 uninterrupted CN oscillations 
<br><br>
Our chromothripsis calls were further classified into "canonical" if at least 60% of the CN segments in the complex rearrangement oscillate between 
2 states, and "with other complex events" in cases where chromothripsis co-localizes with other genomic alterations.
Finally, we visually inspected all candidate chromothripsis events that satisfied the statistical criteria described above.

</h3><hr></div>')

),



##### SI ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
tabPanel("Supplementary Information",
#
  
HTML('<div style="margin-top: 20px; margin-bottom:20px;"><h3 style=" font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  line-height: 30px;text-align: center;margin-left: 10%;margin-right: 10%;"> 
     The information for all tumors examined, as well as the values for the statistical 
     criteria used are listed in <a href="Table_S1.txt" download>Supplementary Table 1</a></h3> </div><hr>'),

HTML('<br><div #style="margin-top: 20px; margin-bottom:20px;"
><h3 style="background-color: WhiteSmoke;font-family: Vollkorn; 
margin-top: 20px; margin-bottom: 20px;  line-height: 30px;text-align: justify;margin-left: 10%; margin-right: 10%;
padding-top: 50px;
    padding-right: 80px;
    padding-bottom: 50px;
    padding-left: 80px; ">
The following files contain a visual depiction of the chromothripsis events detected, as referenced in our manuscript.
<br></br>
In all Supplementary Data Files intrachromosomal SVs are depicted as arcs with the breakpoints represented by black points, 
whereas the breakpoints corresponding to interchromosomal SVs are depicted as colored points. Duplication-like SVs, deletion-like SVs, 
head-to-head and tail-to-tail inversions are depicted in blue, orange, black, and green, respectively. The value for the statistical 
criteria described above for each event is provided below its representation.
</h3></div>'),

  HTML('<a href="Data_Files/Data_File_1.pdf" download><h3 style=" font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  line-height: 30px;text-align: center;margin-left: 10%;margin-right: 10%;">Supplementary Data File 1: High-confidence chromothripsis calls</h3></a>'),
  HTML('<a href="Data_Files/Data_File_2.pdf" download><h3 style=" font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  line-height: 30px;text-align: center;margin-left: 10%;margin-right: 10%;">Supplementary Data File 2: Low-confidence chromothripsis calls</h3></a>'),
  HTML('<a href="Data_Files/Data_File_3.pdf" download><h3 style=" font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  line-height: 30px;text-align: center;margin-left: 10%;margin-right: 10%;" >
Supplementary Data File 3: Regions displaying CN oscillations not classified as chromothripsis. These include: (i) CN oscillating profiles characterized by clusters of tandem duplications or deletions, (ii) candidate chromothripsis cases satisfying the statistical criteria but considered false positives by visual inspection, 
       and (iii) chromosomes displaying at least 7 CN oscillations with few or no SVs mapped.</h3></a>'),
  HTML('<a href="Data_Files/Data_File_4.pdf" download><h3 style=" font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  line-height: 30px;text-align: center;margin-left: 10%;margin-right: 10%;" >
       Supplementary Data File 4: Large clusters of interleaved SVs (at least 20) not identified as chromothripsis by our method</h3></a>')
  #HTML('<a href="Data_Files/Data_File_5.pdf" download><h3 style=" font-family: Vollkorn; 
#margin-top: 20px; margin-bottom:20px;  line-height: 30px;text-align: center;margin-left: 10%;margin-right: 10%;" >Supplementary Data File 5: Regions displaying at least 7 CN oscillations and without clusters of SVs mapped therein</h3></a><hr>')

),


# ------------------------------------------------------------------------------------------------
##### Chromothripsis explorer ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
tabPanel("Chromothripsis explorer",

fluidRow( 
  column(4,
         wellPanel(
           
#            selectInput('cancer_type', 'Cancer type', sort(unique(d$histo))) ,
#            selectInput('donor', 'Donor ID', unique(d$donor_unique_id[which(d$histo == input$cancer_type)])),
#            uiOutput("donor_choice"),
#            selectInput('chromosome', 'Chromosome', as.character(c(1:22,"X")))
#            
           
           selectInput('cancer_type', 'Cancer type', sort(as.vector(unique(d$histo)))),
           #selectInput('donor', 'Donor ID', unique(d$donor_unique_id[which(d$histo == input$cancer_type)]))
           uiOutput("donor_choice")  #,
           #selectizeInput('donor2', 'Donor ID', unique(d$donor_unique_id), selected = NULL, multiple = FALSE,options = NULL),
           #selectInput('chromosome', 'Chromosome', as.character(c(1:22,"X"))  ##,
          #selectInput('type_chromo', 'Chromothripsis category', c("All",unique(d$histo)))
           #) 
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
chromosome selected in the left-hand menu. Depending on the number of SVs detected in the selected tumor the plots might take a few seconds to load; please be patient.
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
  column(4,
         wellPanel(
           selectInput('chromosome', 'Chromosome', as.character(c(1:22,"X"))
         ) 
  )
  )
),



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
)
  
),


# ------------------------------------------------------------------------------------------------
# Acknowledgments
# ------------------------------------------------------------------------------------------------
tabPanel("Acknowledgements and funding",
  
  HTML('<hr><div><h3 style=" font-family: Vollkorn; 
margin-top: 20px; margin-bottom:20px;  line-height: 30px;text-align: center;margin-left: 15%;margin-right: 15%;">The results published here are partly based upon data generated by The 
Cancer Genome Atlas and obtained from the Database of Genotypes and Phenotypes (dbGaP) with accession number phs000178.v8.p7.
<br><br>
Information about TCGA can be found at http://cancergenome.nih.gov. We thank the Research Information Technology Group at 
Harvard Medical School for providing computational resources. This work was supported by grants from the Ludwig Center at Harvard (I.C.C., J.K.L., and P.J.P.). 
<br><br>
This project has received funding from the European Union’s Framework Programme For Research and Innovation Horizon 2020 (2014-2020) under 
the Marie Curie Sklodowska-Curie Grant Agreement No. 703543 (I.C.C.).
</h3>
 </div>
     <hr><p style="text-align: center;font-family: Vollkorn;"> This site was designed by Isidro Cortes-Ciriano (isidrolauscher@gmail.com)</p>
     <p style="text-align: center;font-family: Vollkorn;"> Shiny App deployment by <a href="https://github.com/scottx611x">Scott Ouellette</a></p><br></br><br></br>')
  

)

), tags$style(type = 'text/css', '.navbar { background-color: "black";
                                               
                                               font-size: 30px;
              color: "black"; }',
              
              '.navbar-dropdown { background-color: #262626;
              font-family: Arial;
              font-size: 13px;
              color: #FF0000; }')
)
)

#font-family: Arial;
