#!/bin/sh
/etc/init.d/cron start
/etc/init.d/php8.2-fpm start

exec "$@"

