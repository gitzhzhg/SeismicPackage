#! /bin/sh
# Clean.sh - Remove files created by SUWAVEFORM demos, i.e. by the scripts
#            Xsuwaveform1, Xsuwaveform2, PSsuwaveform1, and PSsuwaveform2
# Nils Maercklin, 2006

# Remove data files:
rm -f akb_wavelet.su ricker_wavelet.su \
      berlage_wavelet1.su berlage_wavelet2.su 

# Remove PS files:
rm -f akb_wavelet.ps ricker_wavelet.ps \
      berlage_wavelet1.ps berlage_wavelet2.ps \
      wavelets.ps berlage_wavelets.ps


exit 0
