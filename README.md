# Clock Glitch Generator
This repository contains the VHDL code of the Paper "Glitch Happens: Attacking the AMD-Xilinx PLL with a Clock Glitch Generator" as presented at DSD 2025.
[![DOI](https://zenodo.org/badge/1012590401.svg)](https://doi.org/10.5281/zenodo.15793033)

## VHDL
The VHDL code of the Glitch Generator is provided in [vhdl/core.vhd](vhdl/core.vhd). The Glitch Generator was implemented on a Zedboard from Digilent which deploys the Zynq-7000.

## Calibration
The repository does not contain the VHDL code used for the calibration. To implement a calibration, a TDC is required. The TDC used in the paper [An FPGA-based 7-ENOB 600 MSample/s ADC without any External Components](https://doi.org/10.5281/zenodo.4016001) is suited for this, but needs to be changed to use the CARRY4 Elements instead of the CARRY8. 