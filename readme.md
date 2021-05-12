# Hydrogenic Atom

## Contents
  - `report/`
  - `src/`
  - `obj/`
  - `mod/`
  - `bin/`
  - `output/`
  - `analytic/`
  - `makefile`
  - `jobs.sh`

## Compilation
  To compile the code:
  ```bash
  make dirs
  make hydrogenic_atom
  ```

  To calculate the analytic one-electron Hydrogen atom radial functions, a small
  fortran file `analytic.f90` must be compiled and executed in `analytic/`:
  ```bash
  cd analytic
  gfortran analytic.f90
  a.out
  ```
  which will produce two files, for l = 0, 1, of the first 3 analytic radial
  functions.

  To compile the report, note that the figures have been externalised and must
  be compiled after the report has been initially compiled:
  ```bash
  cd report
  make -f phys4000_workshop_1.makefile
  ```
  The report can then be compiled again to yield the correct figures.