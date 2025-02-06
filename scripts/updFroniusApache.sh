#! /bin/bash

CREATEDB=../bin/CreateFroniusDB

APACHEWWW=/var/www/fronius
APACHECONF=/etc/apache2/sites-avail/fronius.conf

# these values must be set by the user!!
HTTPHOST=fronius.mrsoft.org
EMAIL=admin@froniussite.org
SSLCERT=/etc/ssl/certs/fronius.crt
SSLKEY=/etc/ssl/private/fronius.key


echo Updateing the fronius apache configuration
echo the following file will be updated: $APACHECONF
read -p "Enter path to www root [$APACHEWWW]: " www
APACHEWWW=${www:-$APACHEWWW}
read -p "Enter output file [$APACHECONF]: " conf
APACHECONF=${conf:-$APACHECONF}
read -p "Enter http host url [$HTTPHOST] " http
HTTPHOST=${http:-$HTTPHOST}
read -p "Enter email address [$EMAIL] " eml
EMAIL=${eml:-$EMAIL}
read -p "SSL Key file [$SSLKEY] " key
SSLKEY=${key:-$SSLKEY}
read -p "SSL Cert file [$SSCERT] " cert
SSLCERT=${cert:-$SSLCERT}

$CREATEDB -createApacheConf -httpHost "$HTTPHOST" -apacheCfgOut "$APACHECONF" -apacheCfgIn fronius.conf.tpl -httpDocRoot "$APACHEWWW" -sslkey "$SSLKEY" -sslcert "$SSLCERT" -email "$EMAIL"


