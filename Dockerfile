FROM ubuntu:20.04

#Install packages
RUN apt update && \
    apt upgrade -y && \
    apt autoremove && \
    apt-get clean

COPY ./geolite-api /root/geolite-api

WORKDIR /root

EXPOSE 3000

CMD ["/root/geolite-api"]
