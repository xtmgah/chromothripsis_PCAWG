FROM rocker/shiny:latest
RUN sudo apt-get update && apt-get install -y \
         nano \
         libssl-dev \
         libxml2-dev
RUN  echo 'install.packages(c("gridExtra", "DT", "ggplot2", "grid", "gridExtra", "plyr", "reshape", "shinythemes", "shinyjs", "htmlwidgets"),\
           repos="http://cran.us.r-project.org", \
           dependencies=TRUE)' \
           > /tmp/packages.R && Rscript /tmp/packages.R

RUN R -e "install.packages(c('devtools'), dependencies=TRUE)"
RUN R -e "devtools::install_github('jcheng5/googleCharts')"

COPY app/ /srv/shinyapps/app

RUN R CMD INSTALL /srv/shinyapps/app/dist/BioCircos.tar.gz

EXPOSE 3242
CMD ["R", "-e", "shiny::runApp('/srv/shinyapps/app', 3242, host='0.0.0.0')"]
