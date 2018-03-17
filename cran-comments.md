## Test environments
* local OS X via homebrew, R 3.4.4
* local Debian 10 (Buster, testing), R 3.4.3
* Windows 10, unknown R version (ran by colleague)
* win-builder (release and devel)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking installed package size ... NOTE
  installed size is  5.3Mb
  sub-directories of 1Mb or more:
    data   3.5Mb
    doc    1.3Mb

  Just slightly above 5MB, hope it is still ok.

## Vignette
vignette build takes long, due to GL rendering
