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



### Building the projects ###

Download and install [CodeTyphon](https://www.pilotlogic.com/sitejoom/index.php/downloads.html).
The version at the creation of this document was 8.3.
There are a few caviats on the 32 bit installation of CodeTyphon on a Raspberry Pi. The
main problem is that the standard size of the swap file is too small for building and using the
whole IDE. Check out the installation hints from [Pilot Logic](https://www.pilotlogic.com/sitejoom/index.php/wiki?id=683)
Though it works the installation of the IDE on a RP3 is not recommended since it takes ages to build.

The final binaries are ok though. The project does not need much computational power to work.

To compile the apache module one needs to install 
* ds_CGI_IDE
* lz_FpWeb
* pl_FreeSpider
* pl_firebirddb
* pl_indy

from the Install/Unistall Packages -> hit rebuild IDE.

I'm currently stuck here - the project does not compile - a linker error occurs.



