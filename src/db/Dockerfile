# Postgis
FROM postgis/postgis:18-3.6

# install software
# Switch to root for package installs
USER root

# Update and install required software including postgis client tools
RUN apt-get update && \
    apt-get install -y --no-install-recommends --no-upgrade\
    postgresql-client-18 \
    postgis \
    wget \
    unzip \
    perl && \
    rm -rf /var/lib/apt/lists/*

# install software
#RUN localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8
ENV LANG en_US.utf8
ENV LC_ALL en_US.utf8

# Make data directory with permissions
RUN mkdir -p /gisdata && chmod 777 -R /gisdata

# Copy nation an state scripts
#COPY ./load_data.sh /usr/local/bin/load_data.sh
#RUN chmod +x /usr/local/bin/load_data.sh

COPY ./load_data.sh /docker-entrypoint-initdb.d/10-load_data.sh
RUN chmod +x /docker-entrypoint-initdb.d/10-load_data.sh

# Overwrite the default /postgis.sh file
#COPY ./postgis.sh /docker-entrypoint-initdb.d/

# Switch back to postgres user
USER postgres
