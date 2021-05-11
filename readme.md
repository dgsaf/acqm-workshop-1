# Hydrogenic Atom

## Contents
  - `report/`
  - `src/`
  - `obj/`
  - `mod/`
  - `bin/`
  - `output/`
  - `makefile`
  - `jobs.sh`

## Compilation
  To compile the code:
  ```bash
  make dirs
  make hydrogenic_atom
  ```

  To compile the report, note that the figures have been externalised and must
  be compiled beforehand:
  ```bash
  cd report
  make -f phys4000_workshop_1.makefile
  ```