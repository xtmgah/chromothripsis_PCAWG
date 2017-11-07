FROM rocker/shiny:latest
COPY app/ /srv/shinyapps/app
EXPOSE 3242
RUN  echo 'install.packages(c("gridExtra", "DT"),\
           repos="http://cran.us.r-project.org", \
           dependencies=TRUE)' \
           > /tmp/packages.R && Rscript /tmp/packages.R
CMD ["R", "-e", "shiny::runApp('/srv/shinyapps/app', 3242, host='0.0.0.0')"]
