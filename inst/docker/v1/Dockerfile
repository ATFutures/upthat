FROM rocker/geospatial
RUN install2.r shiny pct shinyjs 
RUN R -e "devtools::install_github('atfutures-labs/upthat')"
RUN export ADD=shiny && bash /etc/cont-init.d/add