FROM rocker/r-ver:4.1.0
RUN apt-get update && apt-get install -y  git-core imagemagick libcairo2-dev libcurl4-openssl-dev libgit2-dev libicu-dev libpng-dev libsodium-dev libssl-dev libxml2-dev nano make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.2")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.1")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.1.3")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "0.3.4")'
RUN Rscript -e 'remotes::install_version("stringi",upgrade="never", version = "1.7.3")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.9")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.1")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.2.3")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.14")'
RUN Rscript -e 'remotes::install_version("renv",upgrade="never", version = "0.15.5")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.8.0")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("sodium",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("flextable",upgrade="never", version = "0.6.9")'
RUN Rscript -e 'remotes::install_version("rhandsontable",upgrade="never", version = "0.3.8")'
RUN Rscript -e 'remotes::install_version("gfonts",upgrade="never", version = "0.1.3")'
RUN Rscript -e 'remotes::install_version("rebus",upgrade="never", version = "0.1-3")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinybusy",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("shinyFeedback",upgrade="never", version = "0.4.0")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.7.1")'
RUN Rscript -e 'remotes::install_version("readxl",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "2.1.2")'
RUN Rscript -e 'remotes::install_version("kableExtra",upgrade="never", version = "1.3.4")'
RUN Rscript -e 'remotes::install_version("janitor",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("formattable",upgrade="never", version = "0.2.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.23")'
RUN Rscript -e 'remotes::install_version("bs4Dash",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.3")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'renv::install("remotes");remotes::install_local(upgrade="never")'
#RUN rm -rf /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0', shiny.maxRequestSize=40*1024^2);RiskReport::run_app()"
