FROM civisanalytics/civis-services-shiny:1.3

RUN apt-get update && apt-get install -y \
    git

COPY ./requirements.txt1 /requirements.txt1
RUN Rscript -e "packages <- readLines('/requirements.txt1'); install.packages(packages)"

COPY ./app/app.r ./app/app.r
COPY entrypoint.sh /

EXPOSE 3838

ENTRYPOINT ["/entrypoint.sh"]
