FROM alpine:latest


RUN apk add --no-cache \
    R \
    R-dev \
    openssl-dev \
    libressl-dev \
    libxml2-dev \
    make \
    git 

RUN R -e "install.packages('devtools', repos='https://ftp.gwdg.de/pub/misc/cran/')"
RUN R -e "install.packages('renv', repos='https://ftp.gwdg.de/pub/misc/cran/')"
RUN R -e "install.packages('languageserver', repos='https://ftp.gwdg.de/pub/misc/cran/')"

# ** [Optional] Uncomment this section to install additional packages. **
# RUN apk update \
#     && apk add --no-cache <your-package-list-here>

RUN adduser -u 1000 -g 1000 user --disabled-password
USER user
