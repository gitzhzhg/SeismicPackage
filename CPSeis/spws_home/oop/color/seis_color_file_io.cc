#include "color/seis_color_file_io.hh"
/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/
#include "color/color_bar_builder_pop.hh"
#include "color/cbb_rgb_set.hh"
#include "color/cbb_levels_gui.hh"
#include "color/cbb_col_set_gui.hh"
#include "sp/seis_ctype.hh"

#include <stdlib.h>

SeisColorFileIO::SeisColorFileIO (SeisCtype *sct) :
  ColorFileIO (),
  _sct  (sct)
{
}

SeisColorFileIO::~SeisColorFileIO ()
{
}

void SeisColorFileIO::setFileOut (char *filename)
{
  _sct->setFileName (filename);
}

int SeisColorFileIO::readColorFile (ColorBarBuilderPop *cbb, char *filename)
{
  int retval = 0;

  int error;
  if (!cbb || !filename || !strlen(filename)) {
    error = 1;
  }
  else {
    error = 0;
  }

  if (!error) {
    FILE *file_in;
    if ((file_in = fopen(filename,"r")) != NULL) {
      // dummy read of cps hardcoded 999 at first of file
      float val;
      int stat = fscanf (file_in, "%10f", &val);
      long beginning;
      if (stat < 1) {
	error = 1;
      }
      else {
	beginning = ftell (file_in);
	if (beginning < 0) {
	  error = 1;
	}
      }

      if (!error) {
	// read to get the length of the RGBZ array
	float r, g, b, z;
	int k2 = 0, finished = 0;
	while (!finished) {
	  stat = fscanf (file_in, " %f %f %f %f ", &r, &g, &b, &z);
	  if (stat == EOF) {
	    retval = k2;
	    finished = 1;
	  }
	  else if (stat != 4) {
	    error = 1;
	    finished = 1;
	  }
	  else {
	    k2++;
	  }
	}

	if (!error) {
	  // allocate the RGBZ array
	  float *rgbz = (float *)calloc ((size_t)(4*retval), sizeof(float));

	  // read to get the RGBZ's beginning with the first data line
	  error = fseek (file_in, beginning, SEEK_SET);
	  if (!error) {
	    int k3 = 0;
	    if (cbb->maxLevels() < retval) retval = cbb->maxLevels ();
	    for (k2 = 0; k2 < retval; k2++) {
	      stat = fscanf (file_in, " %f %f %f %f ", &rgbz[k3], &rgbz[k3+1],
		&rgbz[k3+2], &rgbz[k3+3]);
	      if (stat != 4) {
		retval = 0;
		error = 1;
	      }
	      k3 += 4;
	    }

	    if (!error) {
	      cbb->levelsGui()->setLevels (retval);
	      // copy the RGBZ array elsewhere and free the internal array
	      cbb->colSetGui()->newRGBSet (rgbz);
	    }
	  }
	  free (rgbz);
	}
      }
      fclose (file_in);
    }
  }
  return retval;
}

int SeisColorFileIO::writeColorFile (class ColorBarBuilderPop *cbb,
  char * filename, class CBBRGBSet *rgb)
{
  int retval = 0;

  int error;
  if (!cbb || !filename || !strlen(filename) || !rgb) {
    error = 1;
  }
  else {
    error = 0;
  }

  if (!error) {
    FILE *file_out;
    if (file_out = fopen(filename,"w")) {

      int num_colors;
      if ((num_colors = rgb->getSize()) > 0) {

	float *rgbz = 0;
	if (rgbz = squishRGBSet(rgb)) {

	  // write out " 999" on first record of file
	  if (fprintf (file_out, " 999\n") > 0) {
  
	    // write out RGBZ
	    int quit = 0;
	    int k2, k3 = 0;
	    for (k2 = 0; k2 < num_colors && !quit && !error; k2++) {
	      if (fprintf (file_out, " %13.3f %13.3f %13.3f %13.3f\n",
		rgbz[k3], rgbz[k3+1], rgbz[k3+2], rgbz[k3+3]) <= 0) {
		error = 1;
	      }
	      k3 += 4;
	      quit = rgbz[k3] == -1;
	    }
	    if (!error) retval = num_colors;
	  }
	  free (rgbz);
	}
      }
      fclose(file_out);
    }
  }
  return retval;
}

float *SeisColorFileIO::squishRGBSet (CBBRGBSet *rgb)
{
// look at the colors in the CBBRGBSet and remove redundancies and return
//   an array of reds, greens, blues, & attributes.  In the case that there
//   was one or more redundancies, place a -1 after the last array element
//   as a flag that no more elements exist.  To avoid memory leaks, the
//   user of this returned array MUST free it when use is terminated.
  if (!rgb) return 0;

  int num_colors;
  if (!(num_colors = rgb->getSize ())) return 0;

  float *rgbz = 0;
  rgbz = (float *)malloc ((size_t)(4*num_colors+1)*sizeof(float));
  if (!rgbz) return 0;

  int k2, k3 = 0;
  for (k2 = 0; k2 < num_colors; k2++) {
    rgb->getOne (k2, &rgbz[k3], &rgbz[k3+1], &rgbz[k3+2], &rgbz[k3+3]);
    k3 += 4;
  }

  int prev_match = 0;
  int new_count =  0;
  k3 = 4;
  for (k2 = 1; k2 < num_colors; k2++) {
    if (rgbz[prev_match  ] != rgbz[k3  ] ||
        rgbz[prev_match+1] != rgbz[k3+1] ||
        rgbz[prev_match+2] != rgbz[k3+2]   ) {
// no match occurred, reset previous match
      prev_match += 4;
      if (new_count < k2) {
// copy the unique rgbz's over
        rgbz[prev_match  ] = rgbz[k3  ];
        rgbz[prev_match+1] = rgbz[k3+1];
        rgbz[prev_match+2] = rgbz[k3+2];
        rgbz[prev_match+3] = rgbz[k3+3];
      }
      new_count++;
    }
    else {
// a match was found, store the latest attribute
      rgbz[prev_match+3] = rgbz[k3+3];
    }
    k3 += 4;
  }
// mark the end of the array
  rgbz[prev_match+4] = -1;
  return rgbz;
}
