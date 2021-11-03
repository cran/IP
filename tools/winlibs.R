# Build against static libraries compiled with Rtools
if(!file.exists("../windows/libidn-1.30/include/idna.h")){
  ## as of R 3.3.0, setInternet2 is now defunct. Use of setInternet2 should be replaced with the method = "wininet" argument to the url and download.file functions
#   setInternet2()
  download.file("https://github.com/rwinlib/libidn/archive/v1.30.zip", "lib.zip", quiet = TRUE)
  dir.create("../windows", showWarnings = FALSE)
  unzip("lib.zip", exdir = "../windows")
  unlink("lib.zip")
}

