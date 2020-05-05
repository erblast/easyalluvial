FROM rocker/verse:latest
COPY * /home/rstudio/easyalluvial/
WORKDIR '/home/rstudio/easyalluvial/'
RUN R -e "devtools::install_dev_deps('.', upgrade = 'never')"
RUN R -e "remotes::install_cran('parcats', upgrade= 'never')"
RUN git clone https://github.com/erblast/parcats.git /home/rstudio/parcats
CMD R -e "devtools::check('.')" -e "devtools::install('.')" -e "devtools::check('/home/rstudio/parcats')"