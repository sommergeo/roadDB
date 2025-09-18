.onAttach <- function(libname, pkgname)
{
  packageStartupMessage("
  Dear user, welcome to roadDB 1.0.
  Please note the following:
    The roadDB package executes SQL queries in ROAD_9.9.2025. 
    ROAD_9.9.2025 is a ROAD copy created on September 9, 2025.
    This ROAD copy is updated every three months.")
}