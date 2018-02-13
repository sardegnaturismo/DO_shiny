# shiny-server on ubuntu 16.04


# To build:
#  1.  cd to the Dockerfile directory
#  2.  docker build -t ubuntu/shiny-server:latest .
#
# To run:
#
#  docker run --rm -d -p 80:3838 ubuntu/shiny-server:latest
# 
# To run with local volumes:
#
#  docker run --rm -d -p 80:3838 \
#     -v /srv/shinyapps/:/srv/shiny-server/ \
#     -v /srv/shinylog/:/var/log/shiny-server/ \ 
#     ubuntu/shiny-server:latest

FROM ubuntu:16.04

LABEL \
    mantainer=Beetobit \
    vendor=Wellnet \
    com.shiny-server.is-beta="false" \
    com.shiny-server.is-production="true" \
    com.shiny-server.version="1.0" \
    com.shiny-server.release-date="2017-10-06"

# =====================================================================
# R Server
# =====================================================================

# Don't print "debconf: unable to initialize frontend: Dialog" messages
ARG DEBIAN_FRONTED=noninteractive



EXPOSE 3838

# RUN mkdir -p /var/log/shiny-server && chown shiny.shiny /var/log/shiny-server
COPY ./config/shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY . /srv/shiny-server/dashboard/
RUN chmod -R 777  /srv/shiny-server/dashboard

CMD ["/opt/shiny-server/bin/shiny-server"]
 


 # install.packages("RAmazonS3", repos = "http://www.omegahat.net/R") http://shiny.rstudio.com/articles/share-data.html
 # https://github.com/cloudyr/aws.s3
 # https://shiny.rstudio.com/articles/persistent-data-storage.html