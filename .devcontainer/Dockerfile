FROM ubuntu:20.04


RUN apt-get update \
    && DEBIAN_FRONTEND="noninteractive" TZ="Europe/London"  apt-get install -y \
    wget \
    gcc \
    g++ \
    gfortran \
    libreadline-dev \
    zlib1g \
    locales \
    zlib1g-dev \
    libbz2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    liblzma-dev \
    libpcre2-dev \
    openjdk-17-jre \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libicu66 \
    make \
    git \
    gpg \
    vim \
    && rm -rf /var/lib/apt/lists/* \
    && wget https://cran.r-project.org/src/base/R-4/R-4.2.1.tar.gz \
    && tar -xf R-4.2.1.tar.gz \
    && cd R-4.2.1 \
    && ./configure --with-x=no --enable-R-shlib \
    && make \
    && make install \
    && cd .. \
    && rm R-4.2.1.tar.gz \
    && rm -rf R-4.2.1


RUN sed -i '/de_DE.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen
ENV LANG de_DE.UTF-8  
ENV LANGUAGE de_DE:de  
ENV LC_ALL de_DE.UTF-8   

COPY dev.bashrc /dev.bashrc
RUN cat /dev.bashrc >> ~/.bashrc

RUN R -e "install.packages('devtools', repos='https://ftp.gwdg.de/pub/misc/cran/')"
RUN R -e "install.packages('renv', repos='https://ftp.gwdg.de/pub/misc/cran/')"
RUN R -e "install.packages('jsonlite', repos='https://ftp.gwdg.de/pub/misc/cran/')"
RUN R -e "install.packages('languageserver', repos='https://ftp.gwdg.de/pub/misc/cran/')"
RUN R -e "install.packages('lintr', repos='https://ftp.gwdg.de/pub/misc/cran/')"
