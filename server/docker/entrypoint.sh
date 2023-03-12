#!/bin/sh
/etc/init.d/cron start
/etc/init.d/php8.2-fpm start
/etc/init.d/nginx start
/bin/bash # just to block the container from exiting, since we only run background daemons
