# R package cxaudit
A collection of R utility functions and objects to implement a simple GxP 
compliant audit trail for apps and services.

The R package relies on the R auditor service as the method to store the audit trail.

_A future release will enable cxaudit to be used as a package dependency with 
functions and methods to store and maintain audit records directly_.

<br/>

_Note that this is a development release with limited functionality_.

<br/>



## Getting Started


<br/>
<br/>

### Dependencies
The auditor service depends on the R package cxapp for configuration, logging and
API authentication. Some of those topics will be briefly mentioned here but please 
refer to the cxapp documentation for additional details.

You can find the cxapp package here [https://github.com/cxlib/r-package-cxapp](https://github.com/cxlib/r-package-cxapp).

<br/>

### Installing cxaudit

Download and install the latest release of cxaudit from https://github.com/cxlib/r-package-cxaudit/releases/latest

You can also install the latest release directly using `install.packages()`.   

```
# -- install dependencies
#    note: see DESCRIPTION for pacakge dependencies
#    note: cxapp can be found at https://github.com/cxlib/r-package-cxapp

install.packages( "https://github.com/cxlib/r-package-cxaudit/releases/download/v0.4.0/cxaudit_0.4.0.tar.gz", type = "source", INSTALL_opts = "--install-tests" )
```

To install prior releases, replace the version number references in the URL.

<br/>




### Auditor service configuration

The cxaudit package and auditor service configuration options are set in the
`APP_HOME/app.properties` file. See an example default configuration (below) 
to quickly get started. 

The cxaudit package relies on the R package cxapp for configuration options. 
See above link for further details.

<br/>


```

# -- auditor service

# enable/disable auditor service
AUDITOR = <enable | disable>


# environment name represeted in audit records
AUDITOR.ENVIRONMENT = host.example.com


# auditor service URL
AUDITOR.URL = <url + port>

# note: storing the clear text access token as a property value is not recommended
AUDITOR.TOKEN = <access token>

# access token as an environment variable
# AUDITOR.TOKEN = $<environmental variable>
# AUDITOR.TOKEN = [env] <environmental variable>

# access token in a key vault
# AUDITOR.TOKEN = [vault] <secret>

```

<br/>

Setting `AUDITOR` to `disable` disables `cxaudit_commit()` from submitting or
comitting audit records to the auditor service. Also note that disabling the 
auditor service will also disable any error checking of submitted audit records.

The `AUDITOR.ENVIRONMENT` property defines the environment name as it is 
represented in the audit record. If not set, the `nodename` property of 
`Sys.info()` is used.

The `AUDITOR.URL` is the auditor service URL, including the port.

The `AUDITOR.TOKEN` is the associated authorization bearer token for the 
connection to the auditor service. Note that the access token is stored in 
clear text, so the use of an environmental variable or key/secrets vault is
strongly recommended.


