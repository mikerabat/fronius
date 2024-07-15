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

if not_service_exists 'firebird*';  
then
    echo "Firebird does not exist - install firebird first"
    exit 1
fi

echo "Firebird in place... starting prcedures"
  
### test if create database binary is there

sudoFunc () {
    # Must be running as sudo
    if [ $(id -u) != 0 ]; then
        zenity --error --text "Sudo password authentication failed. Aborting."
        exit 99
    fi


    ### setup some variables
    CWD=$(pwd)
    INPDAEMON=../bin/froniusd
    DBFILE=/var/lib/fronius/fronius.fdb
    CREATEDB=../bin/CreateFroniusDB
    DAEMON=/usr/bin/froniusd
    SQLFILE=$CWD/froniusdb.sql
    LOGFILE=/var/log/fronius.log
    DAEMONSCRIPT=fronius
    DAEMONSERVICE=fronius.service

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

    # create database
    
    FRONIUSDBDIR=/var/lib/fronius
    if [ ! -d $FRONIUSDBDIR ] 
    then 
        echo create directory $FRONIUSDBDIR 
        mkdir $FRONIUSDBDIR
    fi

    if [ ! -f "$DBFILE" ]
    then
        echo start db creation tool...
    
        $CREATEDB -db $DBFILE -sql $SQLFILE -createconf -log $LOGFILE
    else
        echo database already exists
    fi
 
    
    echo copy the service script    
    cp ./$DAEMONSCRIPT /etc/init.d/$DAEMONSCRIPT
    chmod a+x /etc/init.d/$DAEMONSCRIPT
    update-rc.d $DAEMONSCRIPT defaults 97 03
    # K calls stop - S calls starts
    # cp -u $DAEMONSERVICE /etc/systemd/system 
}


### execute database creation
FUNC=$( declare -f sudoFunc )
sudo -H sh -c "$FUNC; sudoFunc;"


