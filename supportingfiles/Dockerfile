# Shiny Application Server with Additional Packages and dependencies
#
FROM rocker/shiny-verse:4.3

# shiny-verse already includes a tonne of packages, shinydashboard, RPostgres and DBI for example
# Adding a few more that I've needed along the way

# shinydashboard and shinydashboardPlus for nicer looking interface elements
# ShinyBS Provides nice pop-ups and modals in shiny
# shinyjs enables some easier integration of custom javascript into shiny app
# rgdal, sf and leaflet are used for producing maps within the app - REQUIREMENTS libgdal-dev, libudunits2-dev
# shinyWidgets offer some improvements to the standard shiny input panels
# neo4r for connecting to neo4j database # note we're using a forked version for stability (protect against breaking changes to the original repo)
# visNetwork is a good package for displaying networks
# dataone for interacting with KNB/DataONE api - REQUIREMENTS librdf0 librdf0-dev
# aws.s3 provides an interface to amazon web services S3 storage for saving files to AWS
# plotly provides improvement to base R plotting
# leaflet.extras has some useful features for adding elements to map interface

RUN apt update && apt install -y  libgdal-dev libudunits2-dev librdf0 librdf0-dev 

RUN R -e 'remotes::install_version("shinydashboard",version = "0.7.2")' \
	&& R -e 'remotes::install_version("waiter",version = "0.2.5")' \
	&& R -e 'remotes::install_version("fresh", version = "0.2.0")' \
	&& R -e 'remotes::install_version("shinydashboardPlus", version = "2.0.3")' \
	&& R -e 'remotes::install_version("shinyjs",version = "2.1.0")' \
	&& R -e 'remotes::install_version("shinyBS",version = "0.61.1")' \
	&& R -e 'remotes::install_version("shinybusy",version = "0.3.1")' \
	&& R -e 'remotes::install_version("DT",version = "0.26")' \
	&& R -e 'remotes::install_version("sf",version = "1.0-9")' \
	&& R -e 'remotes::install_version("rgdal",version = "1.6-2")' \
	&& R -e 'remotes::install_version("leaflet",version = "2.1.1")' \
	&& R -e 'remotes::install_version("shinyWidgets",version = "0.7.5")' \
	&& R -e 'remotes::install_github("Missing-Salmon-Alliance/neo4r")' \
	&& R -e 'remotes::install_version("visNetwork",version = "2.1.2")' \
	&& R -e 'remotes::install_version("dataone",version = "2.2.2")' \
	&& R -e 'remotes::install_version("aws.s3",version = "0.3.21")' \
	&& R -e 'remotes::install_version("plotly",version = "4.10.1")' \
	&& R -e 'remotes::install_version("leaflet.extras",version = "1.0.0")'
