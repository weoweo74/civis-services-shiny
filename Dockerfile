FROM civisanalytics/datascience-r:2.3.0

RUN apt-get update && apt-get install -y \
    git

COPY ./requirements.txt /requirements.txt
RUN Rscript -e "packages <- readLines('/requirements.txt'); install.packages(packages)"

COPY ./asyncR/ /src/asyncR
RUN rm -rf /src/asyncR/.git && \
    cd /src/asyncR && \
    Rscript -e "devtools::install(dependencies=TRUE)"

COPY ./greedyopt/ /src/greedyopt
RUN rm -rf /src/greedyopt/.git && \
    cd /src/greedyopt && \
    Rscript -e "devtools::install(dependencies=TRUE)"

COPY ./app/app.r ./app/app.r
COPY entrypoint.sh /

EXPOSE 3838

ENTRYPOINT ["/entrypoint.sh"]
