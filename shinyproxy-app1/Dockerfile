FROM rocker/r-base:4.4.2

RUN apt-get update && apt-get install -y \
  build-essential libgl1-mesa-dev libglu1-mesa-dev libv8-dev cmake \
  libglpk-dev libbz2-dev liblzma-dev libfontconfig1 libharfbuzz-dev \
  libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev  libfribidi-dev \
  libcurl4-openssl-dev libxml2-dev libarchive-dev librsvg2-dev git vim \
  libsodium-dev python3-pip \
  && rm -rf /var/lib/apt/lists/*

#removed iq package for now, requires python 3.8, may not need for customer version
#Run apt install -y python3.8
#RUN pip install directlfq

ADD ./Shiny_DPDP/Shiny_Install_OKD.R /code/Shiny_Install_OKD.R
RUN Rscript /code/Shiny_Install_OKD.R
