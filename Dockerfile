FROM civisanalytics/datascience-r:2.3.0

RUN apt-get update && apt-get install -y \
    git \
    software-properties-common \
    gnupg \
    libudunits2-dev \
    libgeos-dev \
    libcairo-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    default-jre \
    default-jdk \
    gdebi-core \
    wget \
    pandoc \
    pandoc-citeproc \
    fonts-lato && \
    add-apt-repository -y ppa:ubuntugis/ppa && \
    apt-get update ; \
    apt-get install -y libgdal-dev

COPY ./requirements.txt /requirements.txt
RUN Rscript -e "packages <- readLines('/requirements.txt'); install.packages(packages)"

COPY entrypoint.sh /

EXPOSE 3838

ENTRYPOINT ["/entrypoint.sh"]
