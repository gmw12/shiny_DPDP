FROM gregwaitt/shinyproxy-app:latest

RUN echo "\noptions(shiny.port=3838, shiny.host='0.0.0.0')" >> /etc/R/Rprofile.site

RUN mkdir /data

RUN chmod -R a+rwX /data

RUN chgrp -R 0 /data && \
    chmod -R g=u /data

RUN chmod -R a+rwX /tmp

RUN chgrp -R 0 /tmp && \
    chmod -R g=u /tmp

# install R code
COPY Shiny_DPDP /app
WORKDIR /app

EXPOSE 3838

CMD ["R", "-q", "-e", "shiny::runApp('/app')"]
