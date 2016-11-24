#! /bin/sh
# Clean.sh - Remove files created by UTM demos
# Nils Maercklin, April 2007

# Remove files created by Utmconv.sh:
rm -f lonlat.txt lonlat1.txt utm1.txt utm2.txt

# Remove files created by Xsuutm.sh and PSsuutm.sh:
rm -f lldata.su utmdata.su coords.bin llplot.bin utmplot.bin
rm -f llplot.ps utmplot.ps

exit 0
