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

/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

/* $Id: dfufp2i.h,v 1.9 1996/03/27 23:15:22 acheng Exp $ */

#ifndef DFUFP2IM_H  /* avoid re-inclusion */
#define DFUFP2IM_H

/* Input structure */
typedef struct
  {
      int32       hdim;         /* horizontal dimension of input data */
      int32       vdim;         /* vertical dimension of input data */
      intn        is_pal;       /* flag to tell whether there is a palette */
      intn        is_vscale;    /* flag telling whether vertical scale included  */
      intn        is_hscale;    /* flag telling whether horizonatal scale included */
      intn        ct_method;    /* color transform method: EXPAND or INTERP */
      float32     max;          /* max value of data */
      float32     min;          /* min value of the data */
      float32    *hscale;       /* horizontal scale */
      float32    *vscale;       /* vertical scale */
      float32    *data;         /* floating point data */
  }
Input;

/* Output structure */
typedef struct
  {
      int32       hres;         /* horizontal resolution of image */
      int32       vres;         /* vertical resolution of image */
      intn        compress;     /* compression scheme */
      char        outfile[32];  /* output file name */
      uint8      *image;        /* Image */
      uint8      *palette;      /* Palette */
  }
Output;

/*----------------------------------------------------------------------------*/
/*                           Function Prototypes                              */

#if defined c_plusplus || defined __cplusplus
extern      "C"
{
#endif                          /* c_plusplus || __cplusplus */

/* prototypes for dfufp2im.c */

#ifdef OLD_WAY
    extern int  duif2i_(int32 *hdim, int32 *vdim, float32 *max, float32 *min,
                        float32 hscale[], float32 vscale[], float32 data[],
                  _fcd palette, _fcd outfile, int *ct_method, int32 *hres,
                        int32 *vres, int *compress, int *lenfn);
    extern int  DFUfptoimage(int32 hdim, int32 vdim, float32 max, float32 min,
                             float32 *hscale, float32 *vscale, float32 *data,
                             uint8 *palette, char *outfile, int ct_method,
                             int32 hres, int32 vres, int compress);
#endif
    extern int  process
                (Input * in, Output * out);
    extern int  generate_scale
                (int32 dim, float32 *scale);
    extern int  convert_interp
                (Input * in, Output * out);
    extern int  pixrep_scaled
                (Input * in, Output * out);
    extern int  compute_offsets
                (float32 *scale, int32 dim, int32 *offsets, int32 res);
    extern int  pixrep_simple
                (Input * in, Output * out);

#if defined c_plusplus || defined __cplusplus
}
#endif                          /* c_plusplus || __cplusplus */

#endif                          /* DFUFP2IM_H */
