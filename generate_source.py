#!/usr/bin/env python3

import sys
import os
import subprocess
import shutil
import psutil

def main():
    # Parse arguments
    if len(sys.argv) != 4:
        print('Usage:', sys.argv[0], '<r0> <lmaxret> <lmax2>')
        sys.exit(1)
    (r0, lmaxret, lmax2) = sys.argv[1:]
    
    # Create output directories
    griddir = 'data/grids'
    h1dir = 'data/h1/r0_'+r0
    h1retdir = h1dir + '/no-ddh/h1ret'
    evenstaticdir = h1dir + '/EvenStatic/h1ret'
    d2Rdir = 'data/d2R/r0_'+r0
    os.makedirs(h1retdir)
    os.makedirs(evenstaticdir)
    if not os.path.exists(griddir):
        os.makedirs(griddir)

    # Create radial grid
    grid = griddir + '/radial_grid_r'+r0+'.h5'
    mathcode = 'r0=' + r0 + '; gridFile="' + grid + '"; Get["h1Lorenz/notebooks/ComputeRadialGrid.wl"]; Quit[];'
    subprocess.run(['wolfram', '-run', mathcode])

    # Run h1Lorenz
    cpus = str(psutil.cpu_count(logical=False))
    subprocess.run(['mpirun', '-n', cpus, './h1Lorenz/h1Lorenz', r0, lmaxret, grid, h1retdir])

    # Run even static modes
    mathcode = 'r0=' + r0 + '; lminret = 2; lmaxret=' + lmaxret + '; gridFile="' + grid + '"; dataDir="' + evenstaticdir + '"; Get["FirstOrderMathematica/LorenzGaugeStatic/BarackLoustoEvenStaticModes.wl"]; Quit[];'
    subprocess.run(['wolfram', '-run', mathcode])

    # Process h1 data to generate ret, R and S fields
    mathcode = 'r0=' + r0 + '; lmaxret=' + lmaxret + '; h1dir="' + h1dir + '"; Get["Process-h1-data.wl"]; Quit[];'
    subprocess.run(['wolfram', '-run', mathcode])

    # Run Ricci
    os.makedirs('data/d2R/r0_'+r0)
    subprocess.run(['./coupling.sh', r0, lmax2], env=dict(os.environ, OMP_NUM_THREADS=cpus))

if __name__== "__main__":
   main()
