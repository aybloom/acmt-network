FROM rocker/verse:4.3.3

# Copy local files into app container
RUN mkdir home/rstudio/workspace
COPY workspace/ /home/rstudio/workspace
RUN chown -R rstudio /home/rstudio/workspace


# Install packages for running acmt
RUN apt-get update
RUN yes | apt-get install libudunits2-dev
RUN yes | apt-get install libgdal-dev
RUN yes | apt-get install curl

#fish vim nano
#RUN apt-get install -y --allow-unauthenticated fish vim nano curl libudunits2-dev libgdal-dev

RUN R -e "install.packages(c('geosphere', 'lwgeom', 'raster', 'sf', 'tidycensus', 'tigris', 'units', 'USAboundaries', 'reshape2', 'osrm', 'leaflet'))"
RUN echo "setwd(\"/home/rstudio/workspace/\")" > ~/../home/rstudio/.Rprofile  #set up working diretory
ARG BYPASS_SSL
RUN echo BYPASS_SSL=$BYPASS_SSL > ~/../home/rstudio/.Renviron  #pass the flag for R to access
RUN chown -R rstudio ~/../home/rstudio/.Renviron  # let rstudio have permission to write .Renviron
