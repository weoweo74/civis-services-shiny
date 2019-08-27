FROM civisanalytics/civis-services-shiny:1.3

RUN apt-get update && apt-get install -y \
    git

COPY ./requirements.txt /requirements.txt
RUN Rscript -e "packages <- readLines('/requirements.txt');
install.packages(raster);
install.packages(shiny);
install.packages(RColorBrewer);
install.packages(malariaAtlas);
install.packages(shinydashboard);
install.packages(stringr);
install.packages(shinyalert);
install.packages(shinyBS);
install.packages(shinythemes);
install.packages(shinycssloaders);
install.packages(shinyjs);
install.packages(mapview);
install.packages(leaflet);
install.packages(kableExtra);
install.packages(plotfunctions;
install.packages(sf);
#devtools::install_github("r-spatial/mapview@develop")
install.packages(DT)"
COPY ./app/app.r ./app/app.r
COPY entrypoint.sh /

EXPOSE 3838

ENTRYPOINT ["/entrypoint.sh"]
