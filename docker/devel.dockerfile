FROM rocker/verse:devel
COPY * /home/rstudio/easyalluvial/
WORKDIR '/home/rstudio/easyalluvial/'
RUN R -e "devtools::install_dev_deps('.', upgrade = 'never')"
RUN git clone https://github.com/erblast/parcats.git /home/rstudio/parcats
RUN git clone https://github.com/tidyverse/tidyr.git /home/rstudio/tidyr
RUN git clone https://github.com/tidyverse/dplyr.git /home/rstudio/dplyr
RUN R -e "devtools::install('/home/rstudio/tidyr')"
RUN R -e "devtools::install('/home/rstudio/dplyr')"
RUN R -e "devtools::install('/home/rstudio/parcats')"
CMD R -e "devtools::check('.')" -e "devtools::install('.')" -e "devtools::check('/home/rstudio/parcats')"