#!/bin/bash

r0=$1
lmax=$2
echo "Evaluating coupling for r0=${r0}, lmax=${lmax}"

cd "data/d2R/r0_"${r0}
mkdir d2R_RetRet

echo "======= RetRet ======"
cd d2R_RetRet/
../../../../SecondOrderRicci/Ricci ../../../h1/r0_${r0}/h1ret ../../../h1/r0_${r0}/h1ret 1000 $lmax
../../../../SecondOrderRicci/Ricci_M ../../../h1/r0_${r0}/h1ret $lmax
../../../../SecondOrderRicci/Ricci_S ../../../h1/r0_${r0}/h1ret $lmax
cd ..
