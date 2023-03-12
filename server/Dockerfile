FROM debian:bullseye-slim
LABEL maintainer="alb42 <alb42@web.de>" Description="AmiTube conversation Server"
ARG DEBIAN_FRONTEND=noninteractive

# Install build & usage deps and some useful tools
RUN apt-get -y update --fix-missing && apt-get -y upgrade && \
    apt-get -y --no-install-recommends install curl openssl ca-certificates && \
    curl https://packages.sury.org/php/README.txt -o /tmp/install_php_repo.sh && \
    chmod +x /tmp/install_php_repo.sh && \
    bash /tmp/install_php_repo.sh && \
    apt-get -y --no-install-recommends install \
      nginx \
      php8.2-fpm \
      php8.2-curl \
      php8.2-gmp \
      php8.2-curl \
      php8.2-xml \
      libpng-dev  \
      python3 \
      python3-pip \
      ffmpeg \
      imagemagick  \
      bc \
      sox  \
      git  \
      g++  \
      gcc  \
      cron \
      make
RUN python3 -m pip install -U yt-dlp && \
    ( \
        cd /tmp; \
        git clone https://github.com/mschordan/agaconv.git && \
        cd agaconv && \
        make -O -j `nproc` && \
        cp bin/agaconv-encode /usr/bin && \
        rm -rf /tmp/*; \
    ) && \
    apt-get -y purge gcc g++ git libpng-dev make && \
    apt-get -y autoremove && \
    apt-get -y autoclean && \
    rm -rf /usr/src/* /tmp/* /var/lib/apt/lists/* /var/tmp/*

# Install binaries and sourcecode
ADD docker/entrypoint.sh        /entrypoint.sh
ADD docker/nginx/default.conf   /etc/nginx/sites-available/default
ADD docker/php/amitube.conf     /etc/php/8.2/fpm/pool.d/
ADD docker/binaries/agaconv     /usr/bin/
COPY src/                       /var/www/html/

# Create log dir, set rights, final cleanup
RUN mkdir -p /var/log/amitube /var/www/html/cdxlstore /var/www/html/tmp && \
    chmod -R a+rwx /var/log/amitube /var/www && \
    chmod -R a+rx /usr/local/bin /entrypoint.sh && \
    chmod -R a+rx /usr/bin/agaconv && \
    chown -R www-data:www-data /var/www /var/log/amitube && \
    crontab -l | { cat; echo "0 0 * * * python3 -m pip install yt-dlp --upgrade"; } | crontab -

ENTRYPOINT ["/entrypoint.sh"]
