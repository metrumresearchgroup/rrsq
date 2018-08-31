
<!-- README.md is generated from README.Rmd. Please edit that file -->

JSON can be used as an interchange format

``` r
library(collectr)
jsonlite::toJSON(collect_r_details(), auto_unbox = TRUE, pretty =TRUE)
```

    ## {
    ##   "rpath": "/Library/Frameworks/R.framework/Resources/bin/R",
    ##   "renvs": {
    ##     "R_ARCH": "",
    ##     "R_BROWSER": "/usr/bin/open",
    ##     "R_BZIPCMD": "/usr/bin/bzip2",
    ##     "R_DOC_DIR": "/Library/Frameworks/R.framework/Resources/doc",
    ##     "R_GZIPCMD": "/usr/bin/gzip",
    ##     "R_HOME": "/Library/Frameworks/R.framework/Resources",
    ##     "R_INCLUDE_DIR": "/Library/Frameworks/R.framework/Resources/include",
    ##     "R_LIBS": "/Library/Frameworks/R.framework/Versions/3.5/Resources/library",
    ##     "R_LIBS_SITE": "",
    ##     "R_LIBS_USER": "~/Library/R/3.5/library",
    ##     "R_PACKRAT_CACHE_DIR": "/Users/devin/PACKRAT_CACHE_DIR",
    ##     "R_PAPERSIZE": "a4",
    ##     "R_PAPERSIZE_USER": "a4",
    ##     "R_PDFVIEWER": "/usr/bin/open",
    ##     "R_PLATFORM": "x86_64-apple-darwin15.6.0",
    ##     "R_PRINTCMD": "lpr",
    ##     "R_QPDF": "/Library/Frameworks/R.framework/Resources/bin/qpdf",
    ##     "R_RD4PDF": "times,inconsolata,hyper",
    ##     "R_SESSION_TMPDIR": "/var/folders/bk/qx8bnfbx739_g83s00t_85th0000gn/T//RtmptHIE9f",
    ##     "R_SHARE_DIR": "/Library/Frameworks/R.framework/Resources/share",
    ##     "R_SYSTEM_ABI": "osx,gcc,gxx,gfortran,?",
    ##     "R_TEXI2DVICMD": "/usr/local/bin/texi2dvi",
    ##     "R_UNZIPCMD": "/usr/bin/unzip",
    ##     "R_ZIPCMD": "/usr/bin/zip"
    ##   },
    ##   "loaded_pkgs": {},
    ##   "cwd": "/Users/devin/Repos/collectr"
    ## }
