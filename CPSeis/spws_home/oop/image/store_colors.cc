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
/*------------------------------------------------------------------
 *USER DOC
 *Name   : storeColors
 *Purpose: Re-index color array by compression and expansion.
 *         and vary color intensity.
 *         Stores new color definitions when requested.
 *         Also handles bit plane color definitions for overlaying images.
 *
 *Author : Michael L. Sherrill / Paul Hauge 
 *Date   : 02/92 (C++ version 4/97)
 *
 * Function Definition:
 * long storeColors( float *rgb, long numcolors,
 *                   long *compression, long *intensity,
 *                   struct COLOR_INFO *col, float rgb_out)
 *
 * rgb           in     RGB color array values.
 * numcolors     in     Number of colors to scale or compress.
 * compression   in     Control parameter for re-indexing.
 * intensity     in     Intensity rescale factor.
 * col           in     ID of color structure to rescale values in.
 * rgb_out       out    Re-populated rgb array reflecting current color mapping
 *
 *NOTES: To load colors without modification set compression = 0,
 *       and intensity = 10.
 *       Overlays have 2 extra colors added for black and white
 *       and the total number of colors is exponential to the number of
 *       planes used.
 *
 *Revisions:
 * DATE      WHO            DESCRIPTION
 * --------  --------       --------------------------------------------
 * 10-93     M.L. Sherrill  Added ability to overlay colors
 * 05-11-92  M.L. Sherrill  Added color intensity control
 *
 *END DOC
 *------------------------------------------------------------------*/
#include <math.h>
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <Xm/Xm.h>

#include "plot_image.hh"
#include "sl/colorset_collection.hh"
#include "sl/color_info_set.hh"



long PlotImage::storeColors(    float *rgb, long numcolors,
                                long *compression, long *intensity,
                                ColorInfo *col, float *rgb_out)

{
 Display *dpy;
 XColor cdef[256];
 long j, n, nn;
 long r, g, b;
 float cbar_ratio;
 float adjust ;
 long num_to_store;
 int    ncol, irang, icol_sw ;
 int    ii, ivs[256] ;
 int    jj, jj_beg, kk, kk_beg ;
 float  xtemp, scf, smin, smax ;
 float  ascf, xrang, xmid, fden, xx, xx_beg ;
 float  f2, f4, vs1, vs2, rat1, rat2, rat3 ;



  dpy = XtDisplay(_graphic);

  num_to_store = numcolors;
  ncol  =  (int)numcolors;

  if( (col->numplanes) && (ncol == col->cnum) )
    { 
     ncol -= 2; /*there are 2 extra in overlay*/
     _user->_num_cbar_cols = ncol;
    }

/*********************************************************************
 *    Define icol_sw to be 0 or 1 depending on whether number of
 *      colors in look-up table is even or odd.
 ********************************************************************/
  irang  =  ncol/2  ;
  icol_sw =  ncol - 2*irang ; 


/**********************************************************************
 *  Find scale factor by following formula :
 *
 *       scf  =  2 ** ( *compression / adjust )
 *            =  e ** ( ln(2) * (*compression) / adjust )
 *
 *     (*compression)  currently ranges from   -100 to + 100 , 
 *       which means that if adjust = 20,
 *         scf   will range from  1/32  to 32  (  32 = 2 ** 5 ) .
 *       and if adjust = 30,
 *         scf will range from about 1/10 to 10 (  actually 2 ** 10/3 )
 ***********************************************************************/
  adjust = 30.0 ;
  xtemp = (log(2.0))*(*compression)/adjust ;
  scf =  exp( xtemp ) ;
  smax =  32.0;
  smin =  1.0 / smax ;
  if( scf < smin ) scf = smin ;
  if( scf > smax ) scf = smax ;



/**********************************************************************
 *     Now loop through to define ivs array, which ranges 
 *       from 0 to ncol - 1.  Because of symmetry, we only
 *       need to loop through upper half of this array.
 **********************************************************************/
  if( icol_sw == 1 )
     {  
     jj_beg  =  irang + 1 ;
     kk_beg  =  irang - 1 ;
     xrang =  (float)(irang) ;
     fden  =  1.0 / xrang ;
     xx_beg  =  fden - 0.001 ;
     ivs[irang] = irang ;  
     }
  else
     {  
     jj_beg  =  irang ;
     kk_beg  =  irang - 1 ; 
     xrang =  (float)(irang) - 0.5 ;
     fden  =  1.0 / xrang ;
     xx_beg  =  0.5 * ( fden - 0.001 ) ;  
     }
  xmid  =  xrang ;
  ascf  =  scf ;
  if( ascf < 0.0 )  ascf = -ascf ;



/*********************************************************************
 *   Define certain factors for both polarities
 *********************************************************************/
  if( scf > 0.0 ) 
     {  
     f2  =  xrang ;
     f4  = -xrang ;  
     }
  else
     {  
     f2  = -xrang ;
     f4  =  xrang ;  
     }
  jj = jj_beg ;
  kk = kk_beg ;
  xx = xx_beg ;
  for( ii = 0 ; ii < irang ; ii++ )
     { 
     rat1 =  (1.0 - xx) / (1.0 + xx ) ;
     rat2 =  pow( rat1, ascf ) ;
     rat3 =  (1.0 - rat2) / (1.0 + rat2) ;
     vs1  =  xmid + f2*rat3 ;
     vs2  =  xmid + f4*rat3 ;
     ivs[jj] = (int)(vs1 + 0.5) ;
     ivs[kk] = (int)(vs2 + 0.5) ;
     jj = jj + 1 ;
     kk = kk - 1 ; 
     xx = xx + fden ;
     }


/************************************************************************
 *      final loop
 ***********************************************************************/
  int ncol_ro;
  j = 0;
  cbar_ratio = (*intensity  * .10) * 65535.0;
  for( ii = 0 ; ii < ncol ; ii++ )
     {
     jj  =  ivs[ii] ;     /*  New scaled index  */
     r   =  4 * jj ;
     g   =  r + 1 ;
     b   =  g + 1 ; 
     cdef[j].pixel = col->pix[j];
     cdef[j].red   = (unsigned short)(min(rgb[r] * cbar_ratio , 65535));
     cdef[j].green = (unsigned short)(min(rgb[g] * cbar_ratio , 65535));
     cdef[j].blue  = (unsigned short)(min(rgb[b] * cbar_ratio , 65535));
     cdef[j].flags = DoRed|DoBlue|DoGreen;
     j++;
     }
  ncol_ro = j;



/*************************************************************************
 *        now take care of overlay plane masking
 **************************************************************************/
     if(col->numplanes)
        {
            /*add a white color*/
        cdef[j].pixel = col->pix[ii];
        cdef[j].red   = 65535;
        cdef[j].green = 65535;
        cdef[j].blue  = 65535;
        cdef[j].flags = DoRed|DoBlue|DoGreen;
        _white_pixel = cdef[j].pixel;
        j++; ii++;
             /*add a black color*/
        cdef[j].pixel = col->pix[ii];
        cdef[j].red   = 0;
        cdef[j].green = 0;
        cdef[j].blue  = 0;
        cdef[j].flags = DoRed|DoBlue|DoGreen;
        _black_pixel = cdef[j].pixel;
        j++;
	ncol_ro = j;
            /*Make overlay plane for all colors.
            May have to repeat this loop someday with rgbs at 65535 which
            will mean allocating regular number of colors * 3 instead of 2.
            Documentation is vague on overlay planes and I am not
            sure exactly why this works... (so far).*/
        for( ii = 0 ; ii < ncol + 2; ii++ )
           {
           cdef[j].pixel = col->pix[ii] | col->pmsk[0];
           cdef[j].red   = 0;
           cdef[j].green = 0;
           cdef[j].blue  = 0;
           cdef[j].flags = DoRed|DoBlue|DoGreen;
           j++;
           }
            /*assign overlay pixel to last masked pixel*/
        _overlay_pixel = cdef[j-1].pixel;
        num_to_store = col->cnum * (long)(pow(2.0, (float)col->numplanes));
        num_to_store = min( ((ncol+2) * 2), num_to_store);
        }
 


  if(col->colorsafe) 
    {

/*
     printf("\n");
     printf("******** PlotImage::storeColors test code below ********\n");
     printf("******** PlotImage::storeColors test code below ********\n");
     printf("******** PlotImage::storeColors test code below ********\n");
     printf("******** PlotImage::storeColors test code below ********\n");
     printf("\n");
*/

/////// Question: Why is this still getting the original colormap
/////// even if it was replaced with a private colormap by calling
/////// newcmap_andcpy or paintset->remakePrivate() from SLApp?

          /** X Error of failed request:                            **/
          /** BadAccess (attempt to access private resource denied) **/
          /**   Major opcode of failed request:  89 (X_StoreColors) **/
          /**   Serial number of failed request:  6265              **/
          /**   Current serial number in output stream:  6488       **/
/*
          int iii;
          XColor cdef2[256];
            for(iii = 0; iii < num_to_store; iii++)
              {
                    cdef2[iii].pixel = cdef[iii].pixel;
                    cdef2[iii].red   = 0;
                    cdef2[iii].green = 0;
                    cdef2[iii].blue  = 0;
                    cdef2[iii].flags = DoRed | DoGreen | DoBlue;
              }
          XQueryColors(dpy, col->cmap, cdef2, (int)num_to_store);
*/
/*
            printf("am in store_colors.cc  colormap = %08lx\n",
              (long)col->cmap);
            printf("num_to_store = %d\n", (int)num_to_store);
            for(iii = 0; iii < num_to_store; iii++)
              {
              int r  = cdef [iii].red;
              int g  = cdef [iii].green;
              int b  = cdef [iii].blue;
              int r2 = cdef2[iii].red;
              int g2 = cdef2[iii].green;
              int b2 = cdef2[iii].blue;

              printf("%3d  pixel = %08lx", iii, cdef[iii].pixel);
              printf("   rgb = %5d %5d %5d", r, g, b);
              printf("   rgb2 = %5d %5d %5d", r2, g2, b2);
              printf("   diff = %4d %4d %4d\n", r2 - r, g2 - g, b2 - b);
              }

            static int kount = 0;
            kount++;
*/
/***
            if(kount == 1)     // this works.
            if(kount == 2)     // this causes X error listed above.

            // The reason kount == 2 caused the X error was that Paintset
            // was converting to a private colormap in remakePrivate() by
            // calling XCopyColormapAndFree, but the original colormap
            // (not the new one) is still being used by this code here.

            // When Paintset was changed to do the same thing as
            // newcmap_andcpy (that is, to make a new colormap but not
            // free the colors in the old one), this code now works.

            // It almost seems that the new colormap is not being used.
            // Certainly it is not being used at this point in this routine.
            // I do not know whether this is the intention.
            // If not, this would be a long-standing cbyt bug (or a bug
            // in sl or sp or image).  I do not feel like trying to
            // investigate this issue at this time.  I am frustrated
            // because doing things in a simple way (as in the DEMO9
            // test program) would make the color issues go away,
            // whereas this 14-year-old code is complicated.

     printf("\n");
     printf("******** PlotImage::storeColors test code above ********\n");
     printf("******** PlotImage::storeColors test code above ********\n");
     printf("******** PlotImage::storeColors test code above ********\n");
     printf("******** PlotImage::storeColors test code above ********\n");
     printf("\n");
***/

    if (ColorsetCollection::readOnly(col)) {
      ColorInfo *old_col = new ColorInfo;
      memcpy (old_col, col, sizeof(ColorInfo)); // remember the old ColorInfo
      col->cnum = num_to_store;    // set the number to store
      ColorsetCollection::store (col, cdef); // store the colors
      col->cnum = old_col->cnum;   // reset to the number originally allocated
      int k2, different;
      for (k2 = 0, different = FALSE; !different && k2 < num_to_store; k2++) {
	different = col->pix[k2] != old_col->pix[k2];
      }
      if (different) {
	ColorInfoCollection::updateEverywhere (col);
      }
      delete old_col;
    }
    else {
      int old_cnum = col->cnum;
      col->cnum = ncol;            // set the number to store (?ncol_ro?)
      ColorsetCollection::store (col, cdef); // store the colors
      col->cnum = old_cnum;        // reset to the number originally allocated
    }
    /*return the newly stored rgb equivalent values.*/
    if(rgb_out != NULL)
      {
      for(n = 0, nn = 0; n < ncol; n++)
        {
        rgb_out[nn]   = (float)cdef[n].red   / 65535.0;
        rgb_out[++nn] = (float)cdef[n].green / 65535.0;
        rgb_out[++nn] = (float)cdef[n].blue  / 65535.0;
        nn += 2;
        }
      }
    }


  return (1); /*may need to return a status at a later date*/

}
