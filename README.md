# Calculation of second order source

This repository gathers together all of the components needed to generate
the second order source and provides a single wrapper script that runs
the whole thing end-to-end.

## Compile
 
Before running the main script, compile h1Lorenz and SecondOrderRicci by
running `scons` inside their respective directories.

## Running

1. Create a radial grid file. This may be generated using the Mathematica file in h1Lorenz/notebooks.
2. Create h1P.m file using scripts in Punctures/FirstOrder and place in data/h1P/.
3. Run `./generate_source.py <r0> <lmaxret> <lmaxS> <lmax2> <grid> <h1P>` where:
   * r0 is the orbital radius
   * lmaxret is the number of first order retarded modes to compute
   * lmaxS is the number of first order S/R modes to use
   * lmax2 is the number of modes of the second order Ricci to compute
   * grid is the path to the radial grid file.
   * h1P is the path to h1P.m.
