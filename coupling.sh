#!/bin/bash

r0=$1
lmax=$2
echo "Evaluating coupling for r0=${r0}, lmax=${lmax}"

cd "data/d2R/r0_"${r0}
mkdir d2R_RetRet d2R_SR d2R_RS d2R_RR d2R_SS-coupling

echo "======= RR ======"
cd d2R_RR/
../../../../SecondOrderRicci/Ricci ../../../h1/r0_${r0}/h1R ../../../h1/r0_${r0}/h1R 1000 $lmax
cd ..

echo "======= RS ======"
cd d2R_RS/
../../../../SecondOrderRicci/Ricci ../../../h1/r0_${r0}/h1R ../../../h1/r0_${r0}/h1S 1000 $lmax
cd ..

echo "======= SR ======"
cd d2R_SR/
../../../../SecondOrderRicci/Ricci ../../../h1/r0_${r0}/h1S ../../../h1/r0_${r0}/h1R 1000 $lmax
cd ..

echo "======= SS ======"
cd d2R_SS-coupling/
../../../../SecondOrderRicci/Ricci ../../../h1/r0_${r0}/h1S ../../../h1/r0_${r0}/h1S 1000 $lmax
cd ..

echo "======= RetRet ======"
cd d2R_RetRet/
../../../../SecondOrderRicci/Ricci ../../../h1/r0_${r0}/h1ret ../../../h1/r0_${r0}/h1ret 1000 $lmax
../../../../SecondOrderRicci/Ricci_M ../../../h1/r0_${r0}/h1ret $lmax
../../../../SecondOrderRicci/Ricci_S ../../../h1/r0_${r0}/h1ret $lmax
cd ..
