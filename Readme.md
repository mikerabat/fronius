# Fronius Data aquisition project #

This project implements an simple interface to the Fronius Web interface. 
It consists of a daemon (or in windows a service) that collects the data
from an inverter. It is adviasable the the whole solar power interface also have
an smart meter installed that is connected to the inverter. 

## Warning ##

This project is still in the making - not all parts are working properly!!! I still 
need to figure out how to create the apache module for Linux (Raspberry pi). Help is appreciated :)

## Components ##

The project basically consists of 3 components:

* Data aquisition: For windows there is a services, for Linux (e.g. the Raspberry PI) there is a daemon.
* Apache Module: An apache module that can be installed with Apache 2.4 servers that serves the pages for the data representation.
* Admin tools: There are 2 tools for administration - CreateDB and FroniusAdm . One to create a database the other one to clear user credentials.

The project has been implemented in Delphi/FPC so after install there are binaries executed. 

## Why ##

The delivered app was not very reliable and also Fronius charges for more than
the last 3 days of data aquisition. The user data interface may not look as cool
as the Fronius app since I'm not a graphic designer but the backend at least stores
all days.

## Prerequisites ##

For the server to work one needs an Apache 2.4 server and Firebird installed. 
To compile the projects one either needs to download Delphi for Windows or
CodeTyphon - the later works also for Linux/RasperryPI .

## Installation on a Raspberry Pi ##

Installation on a Raspberry Pi:

first... update raspi:

```
   sudo apt-update
   sudo apt-upgrade
```

Install [Firebird Database](https://firebirdsql.org/). Basically I used the tutorial from
[here](https://linuxhint.com/install-firebird-raspberry-pi/)

```
   sudo apt-get install firebird-server
```

The project solely relies on *WebAuthn* as means for login - there is one user that is allowed to
create a passkey or a Fido2 key for login purposes. Thus we need two extra library 
that can communicate with keys and verify aslssertsions/credentials.
The first one is OpenSSL1.1. If it's not alread there 
install it according to the script from [here](https://gist.github.com/estshorter/c747c88cbcc53fc7318a5cf788e8fc9b)

```
   cd #;
   mkdir OpenSSLInst
   cd OpenSSLInst
```

Now create the setup file `nano ./install-openssl.sh` in that directory.
Fill in the content:

```
   #!/bin/bash -eu

   OPENSSL_VER=3.2.0

   mkdir openssl
   cd openssl
   wget https://www.openssl.org/source/openssl-${OPENSSL_VER}.tar.gz
   tar xf openssl-${OPENSSL_VER}.tar.gz
   cd openssl-${OPENSSL_VER}
   ./config zlib shared no-ssl3
   make -j4
   sudo make install
   
   cd ../..
```

Install Apache 2:
```
   sudo apt-get apache2
```   
The apache version was 2.4.62 at the time of writing. 

### Installing the IDE ###

Download and install [CodeTyphon](https://www.pilotlogic.com/sitejoom/index.php/downloads.html).
The version at the creation of this document was 8.3.
There are a few caviats on the 32 bit installation of CodeTyphon on a Raspberry Pi. The
main problem is that the standard size of the swap file is too small for building and using the
whole IDE. Check out the installation hints from [Pilot Logic](https://www.pilotlogic.com/sitejoom/index.php/wiki?id=683)
Though it works the installation of the IDE on a Rapberry Pi3 is not recommended since it takes ages to build.

At the current time of development Firebird has a problem on my Raspberry Pi 4 machine - the installed 
Firebird 3 client library is not compatible with the CodeTyphon Firebird 3.0 library. It needs to use the
legacy api for that.

### Prepare additional Packages in the IDE ###
The final binaries are ok though. The project does not need much computational power to work.

To compile the apache cgi module one needs to install 
* ds_CGI_IDE
* lz_FpWeb
* pl_FreeSpider
* pl_firebirddb
* pl_indy

from the Install/Unistall Packages -> hit rebuild IDE.

### Rebuilding the Firebird pacakge ###

It may happen that the firebird client library still does not support all functions that the package expects.

To get Firebird actually working open the pl_FirebirdDB package in the IDE (my version is pl_FirebirdDB V8.4.1). 
On my machine the package is located at:
/usr/local/codetyphon/typhon/components/packages_pl/pl_FirebirdDB/pl_firebirddb.ctpkg
Open the file IB.pas.
Add the line 
{$DEFINE LEGACYFIREBIRDAPIONLY}
before it gets check at any point in the file...
Rebuild the package (triggers an IDE recompile).

### Building the necessary executables ###

Either unzip the provided binaries (currently for Rapberry PI 64 bit only)

or build the projects yourself:
Open the project _./DB/CreateFronusDB.ctpr_ and build it. The binary needs to be located in the _./bin_ folder.
Open the project _./Daemon/froniusd.ctpr_ and build it. Again make sure the output is in the bin directory.
Compile the project _./ApacheWebModule/FPC_fcgifronius.ctpr_. Put the binary in the bin directory.

### Run the install script ###

There is a script to create all necessary links, scripts and the database. Before you run it
adjust the exposed variables to your needs - you should be fine to only adjust the fronius host and the httphost
to your network needs.

Run:

```
   cd scripts
   sudo bash install.sh
```

If successfull it creates 
* A database at /var/lib/fronius/fronius.fdb
* Installs the fronius acquisition daemon in /usr/bin/froniusd
* Puts the main configuration file in /etc/froniusd.conf
* Installs the daemon service as fronius in /etc/init.d/fronius
* Creates the file fronius.conf in /etc/apache2/sites-available
* Enables the site.
* Copies the fronius web site to /var/www/fronius
* Copies the file fcgifronius to /usr/lib/cgi-bin
* Adjusts the ownership of the firebird db and the http docs:

```
   sudo chown firebird:firebird /var/lib/fronius/fronius.fdb
   sudo chown www-data:www-data /usr/lib/cgi-bin/fcgifronius
```

### Configure apache

In case you have already configured ssl and issued a certificate you can skip the following steps.
Note that the automatic apache configuration relies on the fact that the certificate and key files are located as:

_/etc/ssl/private/fronius.key_ and _/etc/ssl/certs/fronius.crt_
if not then please adjust the /etc/apache2/sites-available/fronius.conf file accordingly!

We need ssl enabled on the apache:

```
   sudo a2enmod ssl
```

Create a self signed certificate for our needs
```
   sudo openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout /etc/ssl/private/fronius.key -out /etc/ssl/certs/fronius.crt
```

or edit and run the command.
Edit the variables in the file for your needs.
```
   cd scripts
   sudo bash createSelfSignedCertificate
```

this creates the certificate files and places them to the standard directory.


Test the configuration and restart apache

```
   sudo apache2ctl configtest
   sudo systemctl restart apache2
```

### Notes

This routine was created on my local raspberry pi at home - maybe some things do not work as expected... Sorry for that.
The log files are found in _/var/log/apache2/fronius.log_ for apache related stuff and _/var/log/fronius.log_ for the daemon output.

The system does not support any passwords! Webauthn is the only way to login. You can only add (one) devices if you are connected to the same
lan segment as the web server resides (e.g. the home wlan). Supporting multiple devices is still on the todo list - as well as std passwords. 
Only secure http is supported!

The website is graphically not yet there - but if there is somebody out there who is talented and has time please go for it :)

There are a few things on my todo list:
* Update to fcgi or an apache module - For now I use the a simple cgi for the communication, which I admit is a waste of resources. I couldn't figure to get
  the Apache module working using FPC and a Raspberry PI due to compiler issues.
* Eventually move the current power flow content update to a websocket which is more modern than polling.







