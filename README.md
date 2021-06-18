## geolite-api: API to Geolite Data

Spins up a web server which will return json files based on queries to
the [GeoLite2 database](https://dev.maxmind.com/geoip/geoip2/geolite2/).

It is recommended that you have ~4GB of free memory to use this.

#### Running

Run the `geolite-api` application in a directory that the user that owns
the process can write to. The application will begin by downloading several
CSVs from Geolite's website and then building an in-memory data structure
from them. Building the data structure may take several minutes depending
on the hardware the application is running on. The application will use
100% of a single CPU during this time. After this finishes, CPU utilization
will drop down to nearly 0%, and the application will start listening for
HTTP requests on port 3000. The port is not currently configurable.

#### Build Instructions

This application is built with `cabal-install`. If you don't know how to
use this tool, find someone who does, and they can build the application
for you.

#### Routes

Query the API with requests to these two routes:

    /ipv4/:addr
    /ipv6/:addr
