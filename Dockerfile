FROM ubuntu:22.04

#Install packages
RUN apt update && \
    apt upgrade -y && \
    DEBIAN_FRONTEND=noninteractive apt-get install build-essential ca-certificates -y --fix-missing && \
    apt autoremove && \
    apt-get clean

COPY ./geolite-api /root/geolite-api

RUN chmod a+x /root/geolite-api && \
    update-ca-certificates && \
    echo 'hosts: files dns' > /etc/nsswitch.conf

WORKDIR /root

EXPOSE 3000

CMD ["/root/geolite-api"]
