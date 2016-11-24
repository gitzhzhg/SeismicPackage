#!/bin/sh
# $Id: ps2pdfwr,v 1.4 2002/04/23 11:58:36 easysw Exp $
# Convert PostScript to PDF without specifying CompatibilityLevel.

OPTIONS="-dSAFER"
while true
do
	case "$1" in
	-?*) OPTIONS="$OPTIONS $1" ;;
	*)  break ;;
	esac
	shift
done

if [ $# -lt 1 -o $# -gt 2 ]; then
	echo "Usage: `basename $0` [options...] (input.[e]ps|-) [output.pdf|-]" 1>&2
	exit 1
fi

infile="$1";

if [ $# -eq 1 ]
then
	case "${infile}" in
	  -)		outfile=- ;;
	  *.eps)	base=`basename "${infile}" .eps`; outfile="${base}_cmyk.pdf" ;;
	  *.ps)		base=`basename "${infile}" .ps`; outfile="${base}_cmyk.pdf" ;;
	  *)		base=`basename "${infile}"`; outfile="${base}_cmyk.pdf" ;;
	esac
else
	outfile="$2"
fi

# We have to include the options twice because -I only takes effect if it
# appears before other options.
exec gs  -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite "-sOutputFile=$outfile" -dProcessColorModel=/DeviceCMYK  -f "$infile"
#exec gs $OPTIONS -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite "-sOutputFile=$outfile" $OPTIONS -c .setpdfwrite -f "$infile"

