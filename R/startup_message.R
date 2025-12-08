.onAttach <- function(libname, pkgname)
{
  packageStartupMessage("
  This package utilizes a regularly updated version of ROAD, 
  so results may vary over time.
  
  An active internet connection is required, and the server may occasionally be
  offline. For the current ROAD version and scheduled downtime, please refer 
  to https://github.com/sommergeo/roadDB
 ")
}