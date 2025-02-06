#! /bin/bash

### test if everything is in place

# test if firebird is installed

not_service_exists() {
    local x=$1
    if systemctl status "${x}" 2> /dev/null | grep -Fq "active"; then
            return 1
    else
            return 0
    fi
}

if not_service_exists 'apache2*';
then
    echo "Apache2 not found - install or start apache2 first"
    exit 1
fi
echo "Apache service found!"

if not_service_exists 'firebird*';  
then
    echo "Firebird does not exist - install firebird first"
    exit 1
fi

echo "Firebird in place... starting procedures"
  
### test if create database binary is there

sudoFunc () {
    echo in sudo mode
    # Must be running as sudo
    if [ $(id -u) != 0 ]; then
        zenity --error --text "Sudo password authentication failed. Aborting."
        exit 99
    fi


    ### setup some variables
    CWD=$(pwd)
    INPDAEMON=../bin/froniusd
    CGISCRIPT=../bin/fcgifronius
    CGIDIR=/usr/lib/cgi-bin
    DBFILE=/var/lib/fronius/fronius.fdb
    DBFILECONNSTR=127.0.0.1:/var/lib/fronius/fronius.fdb
    CREATEDB=../bin/CreateFroniusDB
    DAEMON=/usr/bin/froniusd
    SQLFILE=$CWD/froniusdb.sql
    LOGFILE=/var/log/fronius.log
    DAEMONSERVICE=fronius
    #DAEMONSERVICE=fronius.service
    FBLIBRARY=libfbclient.so.2

    # apache config
    FRONIUSHOST=fronius
    APACHEWWW=/var/www/fronius
    APACHECONF=/etc/apache2/sites-available/fronius.conf

    # these values must be set by the user!!
    HTTPHOST=fronius.mrsoft.org
    EMAIL=admin@froniussite.org
    SSLCERT=/etc/ssl/certs/fronius.crt
    SSLKEY=/etc/ssl/private/fronius.key
    
    
    ### test if idaemon is already there 
    echo test if daemon has been compiled
    if [ ! -f "$INPDAEMON" ]
    then
        echo no fronius found
        echo compile fronius daemon first
        exit 1
    fi


    ### test if creation tool is compiled
    if [ ! -f "$CREATEDB" ]
    then
        echo no database creation tool found
        echo compile CreateFroniusDb.ctpr first and copy it to the current folder
        exit 1
    fi
 
    ### put fronius daemon into /usr/bin
    ### check if database already exists

    
    ### copy files in place - only overwrite if the file is newer...
    echo trying to copy $INPDAEMON
    cp -u "$INPDAEMON" "$DAEMON"
    chmod a+x $DAEMON
    chown root:root $DAEMON

    # create database
    
    FRONIUSDBDIR=/var/lib/fronius
    if [ ! -d $FRONIUSDBDIR ] 
    then 
        echo create directory $FRONIUSDBDIR 
        mkdir $FRONIUSDBDIR
    fi

    ### The directory needs to be owned by firebird so it can create it... 
    chown firebird:firebird $FRONIUSDBDIR

    if [ ! -f "$DBFILE" ]
    then
        echo start db creation tool...
    
        $CREATEDB -db $DBFILECONNSTR -sql $SQLFILE -createconf -log $LOGFILE -httpHost "$HTTPHOST" -fbclientlib $FBLIBRARY
        chown firebird:firebird $DBFILE	
    else
        echo database already exists
    fi

    ### check if we the http directory already exists -> overwrite if necessary
    if [ -d $APACHEWWW ]
    then
        echo "Fronius http root already exists."
        read -r -p "Overwrite (Y/N)" response
        if [ "$response" = "y" ]
        then
            # remove the old http root -> indiates the installer shall copy our content
            echo "Remove apache www root"
            rm -R $APACHEWWW
        fi
    fi
 
    echo copy the service script  
    if [ ! -f "/etc/init.d/$DAEMONSERVICE" ]
    then 
        cp ./$DAEMONSERVICE /etc/init.d/$DAEMONSERVICE
        chmod a+x /etc/init.d/$DAEMONSERVICE
        chown root:root /etc/init.d/$DAEMONSERVICE 
        update-rc.d $DAEMONSERVICE defaults 97 03
    else
        systemctl stop fronius
    fi

    ##################################################
    ### Now update the apache server!
    
    # stop the server...
    echo stop apache2 
    systemctl stop apache2

    # copy the htdocs
    echo "copy http site"
    if [ ! -d $APACHEWWW ]
    then 
        echo "Copying htdocs to $APACHEWWW"
        mkdir $APACHEWWW

        # copy and change ownership so it is recognized by apache:
        cp -R ../ApacheWebModule/htdocs/* $APACHEWWW
        chown -R www-data:www-data $APACHEWWW 
    fi

    # copy fronius cgi 
    echo "copy cgi"
    cp -u "$CGISCRIPT" "$CGIDIR"
    chown www-data:www-data $CGIDIR/fcgifronius
    

    # create the configuration
    echo "Create apache configuration"
    $CREATEDB -createApacheConf -httpHost "$HTTPHOST" -apacheCfgOut "$APACHECONF" -apacheCfgIn fronius.conf.tpl -httpDocRoot "$APACHEWWW" -sslkey "$SSLKEY" -sslcert "$SSLCERT" -email "$EMAIL"

    # enable config in apache    
    a2ensite fronius.conf
    echo apache fronius site enabled

    echo Make sure the apache server is loaded and certificates are in place.
    echo The scripts were made with default values which may not be right.
    echo Update the script by
    echo sudo bash ./updFroniusApache.sh
    echo
    echo Test the configuration using 
    echo apachectl configtest
    echo if all is well start apache by
    echo sudo systemctl start apache2
    echo 
    echo start the fronius data gathering daemon by
    echo sudo systemctl start fronius
    echo 
    echo open the browser using the address https://$HTTPHOST 
    echo to test the configuration.
}


### execute database creation
FUNC=$( declare -f sudoFunc )
sudo -H sh -c "$FUNC; sudoFunc;"


