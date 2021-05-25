#!/usr/bin/env python

import sys
import os
import subprocess
import shutil

def main():
    # Parse arguments
    if len(sys.argv) != 7:
        print('Usage:', sys.argv[0], '<r0> <lmaxret> <lmaxS> <lmax2> <grid> <h1P>')
        sys.exit(1)
    (r0, lmaxret, lmaxS, lmax2, grid, h1P) = sys.argv[1:]
    
    # Check radial grid and h1P files exist
    if not os.path.isfile(grid):
        print('Radial grid file', grid, 'does not exist.')
        sys.exit(1)

    if not os.path.isfile(h1P):
        print('h1P file', h1P, 'does not exist.')
        sys.exit(1)
    
    # Create output directories
    h1dir = 'data/h1/r0_'+r0
    h1retdir = h1dir + '/no-ddh/h1ret'
    evenstaticdir = h1dir + '/EvenStatic/h1ret'
    d2Rdir = 'data/d2R/r0_'+r0
    os.makedirs(h1retdir)
    os.makedirs(evenstaticdir)

    # Run h1Lorenz
    subprocess.run(['mpirun', '-n', '2', './h1Lorenz/h1Lorenz', r0, lmaxret, grid, h1retdir])

    # Run even static modes
    mathcode = 'r0=' + r0 + '; lminret = 2; lmaxret=' + lmaxret + '; gridFile="' + grid + '"; dataDir="' + evenstaticdir + '"; Get["FirstOrderMathematica/LorenzGaugeStatic/BarackLoustoEvenStaticModes.wl"]; Quit[];'
    subprocess.run(['wolfram', '-run', mathcode])

    # Process h1 data to generate ret, R and S fields
    mathcode = 'r0=' + r0 + '; mmax = 10; lmax=' + lmaxS + '; lmaxret=' + lmaxret + '; \\[CapitalDelta]rmax=2; h1PFile="' + h1P + '"; h1dir="' + h1dir + '"; Get["Punctures/First Order/h1S-exact-order-eps2.wl"]; Quit[];'
    subprocess.run(['wolfram', '-run', mathcode])

    # Run Ricci
    os.makedirs('data/d2R/r0_'+r0)
    subprocess.run(['./coupling.sh', r0, lmax2])

if __name__== "__main__":
   main()
