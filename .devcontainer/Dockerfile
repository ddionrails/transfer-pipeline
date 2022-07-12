FROM debian:stable-slim


RUN apt-get update && apt-get install -y \
    r-base \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('devtools')"
RUN R -e "install.packages('renv')"
RUN R -e "install.packages('languageserver')"

# ** [Optional] Uncomment this section to install additional packages. **
# RUN apk update \
#     && apk add --no-cache <your-package-list-here>
