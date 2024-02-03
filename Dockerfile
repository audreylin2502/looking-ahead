FROM rocker/shiny

ENV DEBIAN_FRONTEND noninteractive

RUN apt update
RUN apt install -y nano git

RUN R -e "install.packages('shiny')"

RUN rm -r /srv/shiny-server/*
RUN git clone https://github.com/audreylin2502/looking-ahead.git
RUN cp -r looking-ahead/* /srv/shiny-server/
RUN rm -r looking-ahead
