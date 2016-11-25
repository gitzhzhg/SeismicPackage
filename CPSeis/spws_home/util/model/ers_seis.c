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
#include <stdlib.h>
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Xos.h>
#include <X11/cursorfont.h>
#include <Xm/DrawingA.h>
#include "image.h"
#include "pick.h"
#include "tfdefs.h"
#include "ers_seis.h"
 
/* Declaration of private actions */
static void ErsRepaintCursor();


/**********************************************************
 *   Convert from world to device coordinates.            ***
 * ... A linear transformation is assumed.              ***
 * ... Return True if in viewport, False if outside.    ***
 *********************************************************/
 Bool Erswcdc(Widget W, float *xw, float *yw, float *xd, float *yd)
{ ErsCoordInf      *T;
  Bool  stat;

  *xd= 999999;
  *yd= 999999;
  T  = (ErsCoordInf *) ErsGetCoordInf(W);
  if(T==NULL) return False;
  wctodc(T,xw,yw,xd,yd);
  stat = (Bool) Inside_Box(T,xd,yd);

 return stat;
}

/**********************************************************
 * Convert from device to world coordinates.            ***
 * ... A linear transformation is assumed.              ***
 * ... Return True if in viewport, False if outside.    ***
 *********************************************************/
 Bool Ersdcwc(Widget W, float *xd, float *yd, float *xw, float *yw)
{ ErsCoordInf      *T;
  Bool  stat;

  T  = (ErsCoordInf *) ErsGetCoordInf(W);
  if(T != NULL)
   { dctowc(T,xd,yd,xw,yw);
     stat = (Bool) Inside_Box(T,xd,yd);
     return stat;
   }
  else return False;
}

/*************************************************************************
 *
 * ErsPointToKeyTime()
 *
 *  Public function that converts a pair of (x, y) device-coordinates
 *  into a (wx, wy) and (pkey, skey, tkey, tn ) user-coordinates.
 *  (wx,wy), (pkey,skey,tkey,wy) & (tn,wy) are alternate ways of
 *   describing a point.
 *    wx   = User x-coordinate as computed from linear transformation
 *    wy   = User y-coordinate as computed from linear transformation
 *  IF THERE IS no SEISMIC DATA UNDERLYING THE PICKING.
 *    tn   = wx ( rounded to integer )
 *    pkey = UNDEFINED_VAL=skey=tkey
 *  IF THERE IS SEISMIC DATA UNDERLYING THE PICKING.
 *    tn   = Absolute trace number in the current file.
 *    pkey = Header value for header number Phdr on trace tn
 *    skey = Header value for header number Shdr on trace tn
 *    tkey = Header value for header number Thdr on trace tn
 *    The keys default to UNDEFINED_VAL if their header is UNDEFINED_KEY
 *
 ***********************************************************************/
Bool ErsPointToKeyTime(Widget widget,int x,int y,float *pkey,float *skey,
     float *tkey,long *tn,float *wx,float *wy)
{
  struct PlotImage *PI;
  int io,Phdr,Shdr,Thdr;
  int delta_trace;
  float *hd;
  float dx,dy,yscal;
  float x_width,x_offset;
  float distn;
  long  memtn, dsktn;
  Bool status = True;
  Bool usewc  = False;

/**********************************************************
 * Initialize the output variables.                     ***
 *********************************************************/
  *pkey= UNDEFINED_VAL;
  *skey= UNDEFINED_VAL;
  *tkey= UNDEFINED_VAL;
  *tn  = UNDEFINED_TN;
  *wx  = UNDEFINED_VAL;
  *wy  = UNDEFINED_VAL;
  dx   = x;
  dy   = y;
/**********************************************************
 * Determine the widget type first. Seismic or DA?      ***
 * If a DrawingArea assume CBYT behavior.               ***
 *********************************************************/
  if( !XtIsSubclass(widget,xmDrawingAreaWidgetClass) )
    { printf("seismic widget? not  a DA\n"); return False; }
/**********************************************************
 * Get data necessary for coordinate transformations.   ***
 *********************************************************/
  PI = (struct PlotImage *) ErsGetPlotInf(widget);
/**********************************************************
 * Determine if there is seismic data.                  ***
 *********************************************************/
 if(PI == NULL )
   usewc = True;
 else
  {if(I_sdathd(PI) == NULL)    usewc = True;
   if(I_sdatnhd(PI) < SEQHDR)  usewc = True;
   if(I_sdatTdelta(PI) <= 0)   usewc = True;
  }

 if(usewc)  /* No data, use global coordinates */
  { if(!Ersdcwc(widget, &dx, &dy, wx, wy)) return False;
    *pkey = *wx;
    *tn = *wx;
    return True;
  }
/**********************************************************
 * Determine if (x,y) is in image area                  ***
 *********************************************************/
 if(PI == NULL) return False;
 if(x < I_gphXorigin(PI) || x > I_sdatXwdth(PI) + I_gphXorigin(PI))
   return False;
 if(y < I_gphYorigin(PI)  || y > I_sdatXht(PI)  + I_gphYorigin(PI))
   return False;
/**********************************************************
 * Determine borders and width                          ***
 *********************************************************/
 if (I_sdatUM(PI) >= PlotGS)   /* gray scale or color */
   { x_offset = I_gphXorigin(PI) + I_sdatFTL(PI);
     x_width  = I_sdatXwdth(PI);
   }
 else                            /* wiggle type display */
   { x_offset = I_gphXorigin(PI) + I_sdatFTL(PI);
     x_width  = I_sdatNdisp(PI) * I_sdatTdelta(PI);
   }
/**********************************************************
 * Now determine trace number and the key values        ***
 * distn.....relative trace number in display.          ***
 * memtn.....relative trace number in memory.           ***
 *           There are cpixm frames in memory.          ***
 * dsktn.....absolute trace number on disk. (header 1)  ***
 *********************************************************/
  distn = (dx- x_offset)/I_sdatTdelta(PI) + 1.0;
  if(distn < 1.0) distn = 1.0;
  if(distn > I_sdatNdisp(PI)) distn = I_sdatNdisp(PI);
  if(!I_sdatRL(PI))
    {memtn = distn  + 
             (I_sdatFTinI(PI) - 1 + I_cPixmapIdx(PI)*I_sdatOT(PI));
    }
  else
    {memtn = (I_sdatNdisp(PI) + 1) - distn  + 
             (I_sdatFTinI(PI) - 1 + I_cPixmapIdx(PI)*I_sdatOT(PI));
    }
  hd = I_sdathd(PI);
  dsktn = hd[(memtn-1)*I_sdatnhd(PI) + SEQHDR -1];
  *tn   = dsktn;
  yscal = (I_sdatTmax(PI) - I_sdatTmin(PI))/(I_sdatXht(PI)-1);
  *wy  = I_sdatTmin(PI) + yscal*(y - I_gphYorigin(PI));
/**********************************************************
 * The header values for keywords Phdr,Shdr,Thdr        ***
 *********************************************************/
   PR_GetHkeys(widget, &Phdr, &Shdr, &Thdr);
   io   = (memtn-1)*I_sdatnhd(PI);
   if( Phdr != UNDEFINED_KEY && Phdr <= I_sdatnhd(PI))
     { *pkey = hd[io+Phdr-1]; }
   if( Shdr != UNDEFINED_KEY && Shdr <= I_sdatnhd(PI))
     { *skey = hd[io+Shdr-1]; }
   if( Thdr != UNDEFINED_KEY && Thdr <= I_sdatnhd(PI))
     { *tkey = hd[io+Thdr-1]; }

  return True;
}

/************************************************************************
 *
 * ErsKeyTimeToPoint
 *  Public function that converts a (pkey, skey, tkey, time) into a
 *  pair of (x, y) coordinates. (For Displayed traces only)
 *  Note: 1. only valid if seismic data exists and PlotImage is defined.
 *
 ***********************************************************************/
Bool ErsKeyTimeToPoint(Widget widget, float pkey, float skey, float tkey,
     float wy, int *x, int *y)
{
  struct PlotImage *PI;
  float diff,epsp,epss,epst;
  float *hd;
  long io,memtn;
  int  Phdr,Shdr,Thdr;
  int  incr,distn, mtraces, n, screen,nhdrs;
  register int i;
  float ips,yscal;

  *x = 999999;
  *y = 999999;
/**********************************************************
 * Determine the widget type first. Seismic or DA?      ***
 * If a DrawingArea assume CBYT behavior.               ***
 *********************************************************/
  if( !XtIsSubclass(widget,xmDrawingAreaWidgetClass) )
    { printf("seismic widget? not  a DA\n"); return False; }
/**********************************************************
 * Get data necessary for coordinate transformations.   ***
 *********************************************************/
  PR_GetHkeys(widget, &Phdr, &Shdr, &Thdr);
  PI = (struct PlotImage *) ErsGetPlotInf(widget);
  if(PI == NULL) return False;
  hd    = I_sdathd(PI);
  nhdrs = I_sdatnhd(PI);
  if(hd == NULL) return False;
  if(nhdrs < 1 ) return False;

  if(I_sdatNdisp(PI)<= 0 ) return False;

  memtn =  I_sdatFTinI(PI) + I_cPixmapIdx(PI)*I_sdatOT(PI);
  incr  =  1;
  if(I_sdatRL(PI))
    {incr = -1;
     memtn=  I_sdatNdisp(PI) - 1 + I_sdatFTinI(PI) +
             I_cPixmapIdx(PI)*I_sdatOT(PI); }
  epsp = 0.0002*pkey; /* tolerance factor for testing */
  epss = 0.0002*skey; /* tolerance factor for testing */
  epst = 0.0002*tkey; /* tolerance factor for testing */
  for (i=0; i<I_sdatNdisp(PI); i ++)
    { distn = i+1;
      io = (memtn-1)*nhdrs;
      diff = hd[io+Phdr-1] - pkey;
      if((float) fabs((double) diff) < epsp)
        { diff = hd[io+Shdr-1] - skey;
          if((float) fabs((double) diff) < epss)
            { diff = hd[io+Thdr-1] - tkey;
              if((float) fabs((double) diff) < epst) break; }
        } 
      memtn += incr; 
    }
  if(i == I_sdatNdisp(PI)) return False;

  *x = I_gphXorigin(PI) + I_sdatFTL(PI) + 
       I_sdatTdelta(PI)*(distn -1);


  if(I_sdatTmin(PI) == I_sdatTmax(PI)) return False;
  yscal = I_sdatXht(PI)/(I_sdatTmax(PI) - I_sdatTmin(PI));
  *y    = I_gphYorigin(PI) + yscal*(wy - I_sdatTmin(PI));

 
  return(True);
}

/************************************************************************
 *
 * ErsPKeyTimeToPoint
 *  Public function that converts a (pkey, time) into a
 *  pair of (x, y) coordinates. (For Displayed traces only)
 *  Note: 1. only valid if seismic data exists and PlotImage is defined.
 *
 ***********************************************************************/
Bool ErsPKeyTimeToPoint(Widget widget, float pkey, float wy, int *x, int *y)
{
 struct PlotImage *PI;
 Bool  stat;
 Bool usewc  = False;
 float diff;
 float *hd;
 long io,memtn;
 int  Phdr,Shdr,Thdr;
 int  incr,distn, mtraces, n, screen, nhdrs, ndisp;
 float p1,p2,pl,pr,xdistn,interp;
 float tmin,tmax;
 float ux,uy,dx,dy;
 int   tr1,tr2;
 register int i;
 float yscal;

 *x = 999999;
 *y = 999999;
/**********************************************************
 * Determine the widget type first. Seismic or DA?      ***
 * If a DrawingArea assume CBYT behavior.               ***
 *********************************************************/
 if( !XtIsSubclass(widget,xmDrawingAreaWidgetClass) )
  { printf("seismic widget? not  a DA\n"); return False; }
/**********************************************************
 * Get data necessary for coordinate transformations.   ***
 *********************************************************/
 PI = (struct PlotImage *) ErsGetPlotInf(widget);
 if(PI == NULL )
   usewc = True;
 else
  {if(I_sdathd(PI) == NULL)    usewc = True;
   if(I_sdatnhd(PI) < SEQHDR)  usewc = True;
   if(I_sdatTdelta(PI) <= 0)   usewc = True;
  }
 if(usewc)
  {/* No data, use global coordinates */
   dx = *x;
   dy = *y;
   ux = pkey; uy = wy;
   stat = Erswcdc(widget, &ux, &uy, &dx, &dy);
   *x = dx;
   *y = dy;
   return stat;
  }

  hd    = I_sdathd(PI);
  nhdrs = I_sdatnhd(PI);
  ndisp = I_sdatNdisp(PI);
  tmax  = I_sdatTmax(PI);
  tmin  = I_sdatTmin(PI);
  PR_GetHkeys(widget, &Phdr, &Shdr, &Thdr);
  if(hd == NULL) return False;
  if(nhdrs < 1 ) return False;
  if(ndisp <= 0 ) return False;

  xdistn=  1.0;
  distn =  xdistn +0.1;
  memtn =  I_sdatFTinI(PI) + I_cPixmapIdx(PI)*I_sdatOT(PI);
  incr  =  1;
  if(I_sdatRL(PI))
    {incr = -1;
     memtn=  ndisp - 1 + I_sdatFTinI(PI) +
             I_cPixmapIdx(PI)*I_sdatOT(PI); }

  if(ndisp == 1) goto jump;
  pl = hd[(memtn-1)*nhdrs+Phdr-1];
  pr = hd[(memtn-1 + (ndisp-1)*incr)*nhdrs+Phdr-1];
  if(pl < pr)           /* increasing to right */
   { interp = (pkey-pl)/(pr-pl);
     if( pkey <= pl || pkey >= pr )    /* outside the box */
       { xdistn = 1. + (ndisp-1)*interp;
         goto jump;
       }
   }
  else if(pr < pl)      /* increasing to left */
   { interp = (pkey-pl)/(pr-pl);
     if( pkey <= pr || pkey >= pl )    /* outside the box */
       { xdistn = 1. + (ndisp-1)*interp;
         goto jump;
       }
   }
  else                  /* invalid transform data */
   { return False;
   }

/*
  interp = (pkey-pl)/(pr-pl);
  xdistn = 1. + (ndisp-1)*interp;
*/
  p1 = pl;
  tr1= 1;
  memtn += incr;
  for (i=1; i<ndisp; i ++)
    { xdistn = i+1;
      tr2= i+1;
      p2 = hd[(memtn-1)*nhdrs+Phdr-1];
      if((p1-pkey)*(p2-pkey) <= 0.)
        { xdistn = tr1 + ((pkey-p1)/(p2-p1))*(tr2-tr1);
          goto jump;
        }
      p1 = p2;
      tr1= tr2;
      memtn += incr; 
    }
  if(i == ndisp) return False;

 jump:
  *x = I_gphXorigin(PI) + I_sdatFTL(PI) + 
       I_sdatTdelta(PI)*(xdistn -1);

  if(tmin == tmax)
    { *y = I_gphYorigin(PI);}
  else
    { yscal = I_sdatXht(PI)/(tmax - tmin);
      *y    = I_gphYorigin(PI) + yscal*(wy - tmin);
    }
  /* Is point in the legal display? */
  distn = xdistn + 0.01;
  if( distn < 1 || distn > ndisp) return False ;
  if( wy > tmax || wy < tmin) return False;
  return True;
}

/************************************************************************
 *
 * ErsTraceTimeToPoint()   See also ErsPointToKeyTime()
 * Public function that converts a (tn, wy ) pair into a pair of
 * mouse (x, y) device-coordinates.
 *
 * tn......Absolute trace number from header when there is data). (input)
 *   ......User x-coordinate when there is no data.
 * wy......User y-coordinate                                      (input)
 * x.......output device x-coordinate
 * y.......output device y-coordinate
 *
 * Returns False if tn is not in the display
 **********************************************************************/
Bool ErsTraceTimeToPoint(Widget widget, long tn, float wy, int *x,int *y)
{
  struct PlotImage *PI;
  int    n, incr;
  long   distn, memtn, io, itr, tngap, tnnear;
  Bool usewc;
  float  yscal, dx,dy,ux,uy;
  float  *hd;
  register int i;
  if(widget == NULL) return False;
/**********************************************************
 * Initialize : output device-coordinates & usewc flag  ***
 *********************************************************/
  *x = 999999;
  *y = 999999;
  usewc = False;
/**********************************************************
 * Determine the widget type first. Seismic or DA?      ***
 * If a DrawingArea assume CBYT behavior.               ***
 *********************************************************/
  if( !XtIsSubclass(widget,xmDrawingAreaWidgetClass) )
    { printf("seismic widget? not  a DA\n"); return False; }
/**********************************************************
 * Get data necessary for coordinate transformations.   ***
 *********************************************************/
  PI = (struct PlotImage *) ErsGetPlotInf(widget);
/**********************************************************
 * Determine whether there is seismic data or not.      ***
 *********************************************************/
 if(PI == NULL )
   usewc = True;
 else
  {if(I_sdathd(PI) == NULL)    usewc = True;
   if(I_sdatnhd(PI) < SEQHDR)  usewc = True;
   if(I_sdatTdelta(PI) <= 0)   usewc = True;
  }
 if(usewc)  /* No data, use global coordinates */
  { ux = tn;
    uy = wy;
    if(!Erswcdc(widget, &ux, &uy, &dx, &dy) ) return False;
    *x = dx;
    *y = dy;
    return True;
  }

/**********************************************************
 * Check that tn is within the display.                 ***
 * Match tn with a header of a displayed trace.         ***
 * Convert to device coordinates                        ***
 *********************************************************/
  hd = I_sdathd(PI);
  memtn = I_sdatFTinI(PI) + I_cPixmapIdx(PI)*I_sdatOT(PI);
  io    = (memtn-1)*I_sdatnhd(PI) + SEQHDR - 1;
  if(hd[io] > tn ) return False;
  io    = io + (I_sdatNdisp(PI)-1)*I_sdatnhd(PI);
  if(hd[io] < tn ) return False;

  tngap = 9000;
  tnnear= 1;
  memtn =  I_sdatFTinI(PI) + I_cPixmapIdx(PI)*I_sdatOT(PI);
  incr  =  1;
  if(I_sdatRL(PI))
    {incr = -1;
     memtn=  I_sdatNdisp(PI) - 1 + I_sdatFTinI(PI) +
             I_cPixmapIdx(PI)*I_sdatOT(PI); }
  if(I_sdatRL(PI)) {incr = -1; }
  for (i=0; i<I_sdatNdisp(PI); i ++)
    { distn = i+1;
      io = (memtn-1)*I_sdatnhd(PI);
      itr= (hd[io + SEQHDR -1] + 0.00001);
      if(labs(itr - tn) < tngap)
        { tngap = labs(itr-tn);
          tnnear= distn;
        }
      else
        {  }
      if(tn == itr) break;
      memtn += incr; 
    }
  if(i == I_sdatNdisp(PI)) return False;
 
  if(tngap == 0) /* we found an exact match */
   { *x = I_gphXorigin(PI) + I_sdatFTL(PI) +
       (distn -1)*I_sdatTdelta(PI);
   }
  else           /* snap to the nearest? trace */
   {*x = I_gphXorigin(PI) + I_sdatFTL(PI) +
       (tnnear -1)*I_sdatTdelta(PI);
   }
/*  if(I_sdatUM(PI) < PlotGS) *x -= I_sdatTdelta(PI)/2; */

  if(I_sdatTmin(PI) == I_sdatTmax(PI)) return False;
  yscal = (I_sdatXht(PI)-1)/(I_sdatTmax(PI) - I_sdatTmin(PI));
  *y    = I_gphYorigin(PI) + yscal*(wy - I_sdatTmin(PI));

  return(True);
}

/************************************************************************
 *
 * ErsTraceToKey()    ( NOT USED )
 *  Public function that converts a tn value into (pkey, skey, tkey)
 *  tn....absolute trace number from disk
 *  pkey,skey,tkey are header values for trace tn for header words
 *  Phdr,Shdr,Thdr.
 *  Note: Searches thru the traces currently in memory!
 ************************************************************************/
Bool ErsTraceToKey(Widget widget,long tn,float *pkey,float *skey,float *tkey)
{
  PR_ *pikrec;
  struct PlotImage *PI;
  long n, mtraces;
  float *hd;
  register int i, io;
  register long itr;
/**********************************************************
 * Initialize the output variables                      ***
 *********************************************************/
  *pkey = UNDEFINED_VAL;
  *skey = UNDEFINED_VAL;
  *tkey = UNDEFINED_VAL;
/**********************************************************
 * Get data necessary for coordinate transformations.   ***
 *********************************************************/
  pikrec = (PR_ *) ErsGetPickingRecord(widget);
  if(pikrec == NULL) return False;
  PI = (struct PlotImage *) ErsGetPlotInf(widget);
  if(PI == NULL) return False;
  if(I_sdatUser(PI) == NULL) return False;
  hd = I_sdathd(PI);
  if(hd == NULL) return False;
  if(I_sdatnhd(PI) < 1 ) return False;
/**********************************************************
 * Check that tn is within the limits of the file       ***
 * Compute number of traces and headers in memory.      ***
 *********************************************************/
  mtraces = I_sdatFrames(PI) * I_sdatOT(PI);
  if(mtraces <= 0) return False;
  if(tn < 1) return False;
  if(tn > mtraces) return False;
/**********************************************************
 * find the trace in memory that matches tn.            ***
 * Set the key values for keywords Phdr,Shdr,Thdr       ***
 *********************************************************/
  for (i=0; i<mtraces; i++)
    { io = i*I_sdatnhd(PI);
      itr= (hd[io + SEQHDR -1] + 0.00001);
      if(tn == itr) break;
    }
  if(i == mtraces) return False;
  if( pikrec->Phdr > 0 && pikrec->Phdr <= I_sdatnhd(PI))
    { *pkey = hd[io+pikrec->Phdr-1]; }
  if( pikrec->Shdr > 0 && pikrec->Shdr <= I_sdatnhd(PI))
    { *skey = hd[io+pikrec->Shdr-1]; }
  if( pikrec->Thdr > 0 && pikrec->Thdr <= I_sdatnhd(PI))
    { *tkey = hd[io+pikrec->Thdr-1]; }

  return(True);
}


/************************************************************************
 *
 * ErsPKeyToTrace()
 *  Public function that converts a pkey value into tn
 *  tn....absolute trace number from disk
 *  pkey is a header value from the trace for header words Phdr.
 *  Note: Searches thru the traces currently in memory!
 *        It searches for the closest match to pkey.
 ************************************************************************/
Bool ErsPKeyToTrace(Widget widget,float pkey,long Phdr, long *tn)
{
  struct PlotImage *PI;
  long n, mtraces, ndisp, nhdrs;
  float eps,diff;
  float *hd;
  float xdistn,pl,pr,p1,p2,interp;
  long  tr1,tr2,distn,memtn;
  long  incr;
  register int i, io;
  register long itr;
/**********************************************************
 * Initialize the output variables                      ***
 *********************************************************/
  *tn = UNDEFINED_TN;
/**********************************************************
 * Get data necessary for coordinate transformations.   ***
 *********************************************************/
  PI = (struct PlotImage *) ErsGetPlotInf(widget);
  if(PI == NULL) return False;
  if(I_sdatUser(PI) == NULL) return False;
  hd    = I_sdathd(PI);
  nhdrs = I_sdatnhd(PI);
  ndisp = I_sdatNdisp(PI);
  if(hd == NULL) return False;
  if(nhdrs < 1) return False;
  if(hd == NULL) return False;
  if(ndisp <= 0 ) return False;
/**********************************************************
 * find the trace in memory that is closest to pkey.    ***
 *********************************************************/
  if(Phdr > nhdrs  || Phdr < 1)
    { printf(" Primary key out of range\n");
      return False; }

  xdistn=  1.0;
  distn =  xdistn +0.1;
  memtn =  I_sdatFTinI(PI) + I_cPixmapIdx(PI)*I_sdatOT(PI);
  incr  =  1;
  if(I_sdatRL(PI))
    {incr = -1;
     memtn=  ndisp - 1 + I_sdatFTinI(PI) +
             I_cPixmapIdx(PI)*I_sdatOT(PI); }

  if(ndisp == 1) goto jump;
  pl = hd[(memtn-1)*nhdrs+ Phdr-1];
  pr = hd[(memtn-1 + (ndisp-1)*incr)*nhdrs+ Phdr-1];
  if(pl < pr)           /* increasing to right */
   { interp = (pkey-pl)/(pr-pl);
     if( pkey <= pl || pkey >= pr)
       { xdistn = 1. + (ndisp-1)*interp;
         goto jump;
       }
   }
  else if(pr < pl)      /* increasing to left */
   { interp = (pkey-pl)/(pr-pl);
     if( pkey <= pr || pkey >= pl )    /* outside the box */
       { xdistn = 1. + (ndisp-1)*interp;
         goto jump;
       }
   }
  else                  /* invalid transform data */
   { return False;
   }

  p1 = pl;
  tr1= 1;
  memtn += incr;
  for (i=1; i<ndisp; i ++)
    { xdistn = i+1;
      tr2= i+1;
      p2 = hd[(memtn-1)*nhdrs+ Phdr-1];
      if((p1-pkey)*(p2-pkey) <= 0.)
        { xdistn = tr1 + ((pkey-p1)/(p2-p1))*(tr2-tr1);
          goto jump;
        }
      p1 = p2;
      tr1= tr2;
      memtn += incr; 
    }
  if(i == ndisp) return False;

 jump:
  distn = xdistn + 0.01;
  if( distn < 1 || distn > ndisp) return False ;
  *tn = hd[(memtn-1)*nhdrs+ SEQHDR - 1];
 
  return True;
}

/************************************************************************
 *
 *  struct PlotImage *ErsGetPlotInf()
 *  Public method to get the PlotImage structure from the picking record
 *  Bool ErsSetPlotInf()
 *  Public method to set the PlotImage structure in the picking record
 *
 **********************************************************************/
struct PlotImage *ErsGetPlotInf(Widget W)
{PR_ *pikrec;
 int n;

 pikrec = ErsGetPickingRecord(W);
 if(pikrec == NULL) return NULL;
 if(pikrec->widget_count == 0) return NULL;

 for(n=0;n<pikrec->widget_count;n++)
   { if(W == pikrec->widget_rec[n].widget) break; }
 if(n == pikrec->widget_count) return NULL;
 return (struct PlotImage *) pikrec->widget_rec[n].PlotI;
}

Bool ErsSetPlotInf(Widget W, caddr_t PI )
{ PR_ *pikrec;
  ErsWidgetRecord  *wrec;
  int n;

  pikrec = (PR_ *) ErsGetPickingRecord(W);
  if(pikrec == NULL) return False;
  if(pikrec->widget_count == 0)
    { printf("ErsSetPlotInf: ERROR\n");
      printf("The Picking Record and widget are not connected\n");
      return False; }
/* Find the correct widget record */
  for(n=0;n<pikrec->widget_count;n++)
   { if(W == pikrec->widget_rec[n].widget)  break; }
  if(n == pikrec->widget_count) return False;

  wrec = &pikrec->widget_rec[n];
  wrec->PlotI = PI;
 return True;
}

/************************************************
 * Bool ErsSeismicSetCoordInf()               ***
 * Reset the default coordinate map using the ***
 * information in the PlotImage structure.    ***
 * For User <-> Device mapping.               ***
 * Call this routine when user coords. change ***
 ***********************************************/
Bool ErsSeismicSetCoordInf(Widget W)
{struct PlotImage *PI;
 float xd1,yd1,xd2,yd2,xw1,yw1,xw2,yw2;
 float *hd;
 long mem1,mem2;
 int n;

 PI = ErsGetPlotInf(W);
 if(PI == NULL)     return False;

/* desperation defaults */
 xd1= I_gphXorigin(PI);
 yd1= I_gphYorigin(PI);
 xd2= xd1 + I_sdatXwdth(PI) -1.0;
 yd2= yd1 + I_sdatXht(PI) -1.0;
 yw1= 0.0;
 xw1= 0.0;
 xw2= I_sdatXwdth(PI) -1.0;
 yw2= I_sdatXht(PI) -1.0;
 ErsSetCoordInf(W,&xd1,&yd1,&xw1,&yw1,
                        &xd2,&yd2,&xw2,&yw2);

/* Values when seismic data has been read in already */
 hd = I_sdathd(PI);
 if(hd == NULL) return False;
  if(I_sdatXdepth(PI)!=1)
   { xd1 = I_gphXorigin(PI) + I_sdatFTL(PI);
     xd2 = I_gphXorigin(PI) + I_sdatXwdth(PI) -1.0;
   }
  else
   { xd1 = I_gphXorigin(PI) + I_sdatFTL(PI);
     xd2 = I_gphXorigin(PI) + I_sdatNdisp(PI)*I_sdatTdelta(PI);
   }
  yd1 = I_gphYorigin(PI);
  yd2 = yd1 + I_sdatXht(PI) -1.0;

  if(!I_sdatRL(PI)) /* normal left to right order */
   { mem1 = I_sdatFTinI(PI) + I_cPixmapIdx(PI)*I_sdatOT(PI);
     mem2 = mem1 + I_sdatNdisp(PI) - 1;
   }
  else
   { mem1 = I_sdatFTinI(PI) + I_sdatNdisp(PI) - 1 +
            I_cPixmapIdx(PI)*I_sdatOT(PI);
     mem2 = mem1 - I_sdatNdisp(PI) + 1;
   }
  xw1 = hd[(mem1-1)*I_sdatnhd(PI) + SEQHDR -1];
  xw2 = hd[(mem2-1)*I_sdatnhd(PI) + SEQHDR -1];
  yw1 = I_sdatTmin(PI);
  yw2 = I_sdatTmax(PI);

  ErsSetCoordInf(W,&xd1,&yd1,&xw1,&yw1,
                        &xd2,&yd2,&xw2,&yw2);

 return True;
}

/**************************************************************
 * Return absolute trace numbers of the left and right trace***
 * and min and max time in the visible display . Return True***
 * if plot is reversed and False if Not.                    ***
 *************************************************************/ 
void ErsSeismicLandRTrace(Widget W, long *left_tn, long *right_tn,
     float *tmin, float *tmax, Bool *invert)
{ struct PlotImage *PI;
  float  tn1,tn2,*hd;
  long   io,nhdrs;

  *left_tn = -1;
  *right_tn= -1;
  *invert  = False;
  *tmin = 0.;
  *tmax = 1.0;
  PI = (struct PlotImage *) ErsGetPlotInf(W);
  if(PI == NULL) return;
  hd = I_sdathd(PI);
  if(hd == NULL) return;
  nhdrs = I_sdatnhd(PI);
  io = (I_sdatFTinI(PI)-1) * nhdrs + SEQHDR - 1 +
       I_cPixmapIdx(PI)*I_sdatOT(PI)*nhdrs;
  tn1= hd[io]; /* first in memory in display*/
  io = (I_sdatFTinI(PI) + I_sdatNdisp(PI) -2) * nhdrs
       + SEQHDR - 1 + I_cPixmapIdx(PI)*I_sdatOT(PI)*nhdrs;
  tn2= hd[io]; /* last in memory in display*/
  if(I_sdatRL(PI))
    { *left_tn =tn2;
      *right_tn=tn1;
      *invert = True; }
  else
    { *left_tn =tn1;
      *right_tn=tn2;
      *invert = False;}
  *tmin = I_sdatTmin(PI);
  *tmax = I_sdatTmax(PI);
 return;
}
/**************************************************************
 * Return primary key values of the left and right trace    ***
 * and min and max time in the visible display . Return True***
 * if plot is reversed and False if Not.                    ***
 *************************************************************/ 
void ErsSeismicLandRPKey(Widget W, float *left_key, float *right_key,
     float *tmin, float *tmax, long Phdr, Bool *invert)
{struct PlotImage *PI;
 ErsCoordInf      *T;
 float  key1,key2,*hd,uc[4];
 long   io,nhdrs;

 *left_key = -1;
 *right_key= -1;
 *invert  = False;
 *tmin = 0.;
 *tmax = 1.0;
 PI = (struct PlotImage *) ErsGetPlotInf(W);
 hd = NULL;
 if(PI != NULL)  hd = I_sdathd(PI);
 if(PI == NULL || hd == NULL)
  {/* No seismic, use world coordinate system */
   T  = (ErsCoordInf *) ErsGetCoordInf(W);
   if(T==NULL) return;
   Get_uc(T,uc);
   *left_key = uc[0]; *right_key=uc[1];
   *tmin = uc[2];     *tmax = uc[3];
   *invert = False;
   return;
  }

 nhdrs = I_sdatnhd(PI);
 io = (I_sdatFTinI(PI)-1) * nhdrs + Phdr - 1 +
      I_cPixmapIdx(PI)*I_sdatOT(PI)*nhdrs;
 key1= hd[io]; /* first in memory in display*/
 io = (I_sdatFTinI(PI) + I_sdatNdisp(PI) -2) * nhdrs
      + Phdr - 1 + I_cPixmapIdx(PI)*I_sdatOT(PI)*nhdrs;
 key2= hd[io]; /* last in memory in display*/
 if(I_sdatRL(PI))
  { *left_key =key2;
    *right_key=key1;
    *invert = True; }
 else
  { *left_key =key1;
    *right_key=key2;
    *invert = False;}
 *tmin = I_sdatTmin(PI);
 *tmax = I_sdatTmax(PI);
 return;
}
/******************************************************************
 * Get the limits for trace number, primary key, secondary key, ***
 * tertiary key, and time.Also return the frame number displayed***
 *****************************************************************/
void ErsSeismicGetLimits(Widget W, int *frame, float *tr1, float *tr2,
     float *p1, float *p2, float *s1, float *s2,float *t1, float *t2,
     float *time1, float *time2)
{ struct PlotImage *PI;
  struct ImageInput *user;
  ErsCoordInf *T;
  float  temp, *hd;
  long   io,nhdrs,memtn;
  int    phdr,shdr,thdr;
  Bool   RtoL;

  *frame = 0;
  *tr1= UNDEFINED_VAL;
  *tr2= UNDEFINED_VAL;
  *p1 = UNDEFINED_VAL;
  *p2 = UNDEFINED_VAL;
  *s1 = UNDEFINED_VAL;
  *s2 = UNDEFINED_VAL;
  *t1 = UNDEFINED_VAL;
  *t2 = UNDEFINED_VAL;
  *time1=UNDEFINED_VAL;
  *time2=UNDEFINED_VAL;

  PI   = ErsGetPlotInf(W);
  hd   = NULL;
  RtoL = False;
  user = NULL;
  if(PI == NULL) return;
  if(PI != NULL)
   { hd   = I_sdathd(PI);
     user = I_sdatUser(PI);
     nhdrs= I_sdatnhd(PI);
   }
  
  if(hd == NULL || nhdrs < SEQHDR)
   { float uc[4];
     T = ErsGetCoordInf(W);
     if(T == NULL) return;
     Get_uc(T,uc);
     *time1 = uc[2]; *time2 = uc[3];
     *tr1   = 0;     *tr2   = 0;
     *p1    = uc[0]; *p2    = uc[1];
     return;
   }

  if(user != NULL)
   { *time1 = user->tmin;
     *time2 = user->tmax;
     *frame = I_cPixmapIdx(PI);
     RtoL   = I_sdatRL(PI);
   }

  PR_GetHkeys( W, &phdr, &shdr, &thdr);

  memtn = I_cPixmapIdx(PI)*I_sdatOT(PI)+1;
  io = (memtn-1)*nhdrs;
  *tr1= hd[io+SEQHDR-1]; /* first in memory in display*/
  if(phdr != UNDEFINED_KEY && phdr <= nhdrs) *p1 = hd[io + phdr-1];
  if(shdr != UNDEFINED_KEY && phdr <= nhdrs) *s1 = hd[io + shdr-1];
  if(thdr != UNDEFINED_KEY && phdr <= nhdrs) *t1 = hd[io + thdr-1];
  memtn = memtn + I_sdatOT(PI)-1;
  io = (memtn-1)*nhdrs;
  *tr2= hd[io+SEQHDR]; /* last in memory in display*/
  if(phdr != UNDEFINED_KEY && phdr <= nhdrs) *p2 = hd[io + phdr-1];
  if(shdr != UNDEFINED_KEY && phdr <= nhdrs) *s2 = hd[io + shdr-1];
  if(thdr != UNDEFINED_KEY && phdr <= nhdrs) *t2 = hd[io + thdr-1];
  if(RtoL)
    { temp = *tr1; *tr1 = *tr2; *tr2 = temp;;
      temp = *p1; *p1 = *p2; *p2 = temp;
      temp = *s1; *s1 = *s2; *s2 = temp;
      temp = *t1; *t1 = *t2; *t2 = temp;
    }

 return;
}
void ErsSeismicGetSampleRate(Widget W, float *srval)
{struct PlotImage  *PI;
 ErsCoordInf       *T;
 struct ImageInput *user;
 float  uc[4],*hd;

 *srval = 0.004;
 PI = (struct PlotImage *) ErsGetPlotInf(W);
 user = NULL;
 hd   = NULL;
 if(PI != NULL) hd = I_sdathd(PI);
 if(PI != NULL) user = I_sdatUser(PI);
 if(hd == NULL || user == NULL)
  {/* No seismic, use world coordinate system */
   T  = (ErsCoordInf *) ErsGetCoordInf(W);
   if(T==NULL) return;
   Get_uc(T,uc);
   *srval = 0.001*(uc[3]-uc[2]);
   if(*srval < 0) *srval = - *srval;
  }
 else
  { user = I_sdatUser(PI);
   *srval = user->G.srval * user->tdec;
  }

 return;
}

void ErsSeismicGethdDat(Widget W, float **hd, int *nhdrs, int *numv)
{ struct PlotImage *PI;

  *hd    = NULL;
  *nhdrs = 0;
  *numv  = 0;
  PI = (struct PlotImage *) ErsGetPlotInf(W);
  if(PI == NULL) return;
  *hd    = I_sdathd(PI);
  *nhdrs = I_sdatnhd(PI);
  *numv  = I_sdatNdisp(PI);

 return;
}


void ErsSeismicRtoL(Widget W, Bool *invert)
{ struct PlotImage *PI;

  *invert = False;
  PI = (struct PlotImage *) ErsGetPlotInf(W);
  if(PI == NULL) return;
  if(I_sdatRL(PI))
    { *invert = True; }
  else
    { *invert = False;}
 return;
}

int ErsSeismicGetDataStat(Widget W)
{ struct PlotImage *PI;
  float *hd;

  PI = (struct PlotImage *) ErsGetPlotInf(W);
  if(PI == NULL) return -1;
  hd = I_sdathd(PI);
  if(hd == NULL ) return -1;
  if(I_sdatXdepth(PI) == 1)
    { if(I_sdatBtr(PI) == NULL) return -1; }
  else
    { if(I_sdatFtr(PI) == NULL) return -1; }
 return -1;
}
/************************************************************************
 *
 *  ErsSeismicTimeToSample
 *    Function that is given a time value and returns the corresponding
 *    sample number (starting at 0) in the original data.
 *    This function adjusts the time if it is smaller than the starting
 *    time of the dataset or greater than the largest time.
 *    If there is no data then index is returned as a device coordinate?
 *
 ***********************************************************************/
void ErsSeismicTimeToSample(Widget widget, float *time, int *index)
{ struct PlotImage  *PI;
  struct ImageInput *user;
  float  *hd;

  *index = -1;
  PI = (struct PlotImage *) ErsGetPlotInf(widget);
  if(PI == NULL) return;
  hd = NULL;
  hd = I_sdathd(PI);
  if(hd == NULL) return;

  user = I_sdatUser(PI);
  if (*time < I_sdatTmin(PI)) *time = user->tmin;
  if (*time > I_sdatTmax(PI)) *time = user->tmax;

  *index = (*time - user->tmin) /(user->G.srval*user->tdec) + 0.5;
}

/************************************************************************
 *  ErsSeismicTellSelection
 *    This function returns the first and the last index of the traces
 *    in memory that are selected. Also counts traces between the limits.
 *    tn_start & tn_end are the selected trace numbers.
 *    Converts header trace number to memory storage index.
 ***********************************************************************/
Bool ErsSeismicTellSelection(Widget widget, long tn_start, long tn_end,
     long *first,long *last, long *count)
{
  struct PlotImage *PI;
  long   itr,io,memtn;
  long   trmin,trmax;
  float  *hd;
  int    incr;
  register int i;

  *first = -1;
  *last  = -1;

/**********************************************************
 * Find whether there is data to work with.             ***
 *********************************************************/
  PI = (struct PlotImage *) ErsGetPlotInf(widget);
  if(PI == NULL) return False;
  if(I_sdatXdepth(PI) == 1)
    { if(I_sdatBtr(PI) == NULL) return  False; }
  else
    { if(I_sdatFtr(PI) == NULL) return  False; }
/**********************************************************
 * Locate the selected trace limits. A monotonic sequence**
 * is assumed for the trace numbers.                    ***
 *********************************************************/
  trmin =  tn_start;
  trmax =  tn_end;
  if(trmin > trmax)
    { long temp;
      temp = trmin;
      trmin=trmax;
      trmax = temp; }
  memtn =  I_sdatFTinI(PI) + I_cPixmapIdx(PI)*I_sdatOT(PI);
  incr  =  1;
  if(I_sdatRL(PI))
    {incr = -1;
     memtn=  I_sdatNdisp(PI) - 1 + I_sdatFTinI(PI) +
             I_cPixmapIdx(PI)*I_sdatOT(PI); }
  hd = I_sdathd(PI);
  *count = 0;
  for (i=0; i<I_sdatNdisp(PI); i ++)
    { io = (memtn-1)*I_sdatnhd(PI);
      itr= (hd[io + SEQHDR -1] + 0.00001);
      if(tn_start == itr) *first= memtn-1;
      if(tn_end   == itr) *last = memtn-1;
      if( itr>=trmin && itr <= trmax) *count += 1;
      memtn += incr; 
    }

  if (*first == -1 || *last == -1) return False;

  if (*first > *last)
    { int temp;
      temp = *first;
      *first = *last;
      *last = temp;
    }
  return True;
}

/************************************************************************
 *  ErsSeismicTellSelectionP
 *    This function returns the memory index of the traces closest
 *    to the primary key values pkey_start & pkey_end.
 *    Also counts traces between the limits.
 *    Converts primary header value to memory storage index.
 *    first & last are indexed relative to 0.
 ***********************************************************************/
Bool ErsSeismicTellSelectionP(Widget widget, float pkey_start, float pkey_end,
     int Phdr, long *first,long *last)
{
  struct PlotImage *PI;
  long   itr,io,memtn;
  float  *hd,pkey,diff1,diff2,diff;
  int    incr,nhdrs;
  register int i;

  *first = -1;
  *last  = -1;

/**********************************************************
 * Find whether there is data to work with.             ***
 *********************************************************/
  PI = (struct PlotImage *) ErsGetPlotInf(widget);
  if(PI == NULL) return False;
  if(I_sdatXdepth(PI) == 1)
    { if(I_sdatBtr(PI) == NULL) return  False; }
  else
    { if(I_sdatFtr(PI) == NULL) return  False; }
  nhdrs = I_sdatnhd(PI);
  if(nhdrs < 1 || nhdrs < Phdr) return False;
  hd = I_sdathd(PI);
  if(hd == NULL) return False;
/**********************************************************
 * Locate traces with pkey values closest to pkey_start ***
 * and pkey_end                                         ***
 *********************************************************/
  incr  =  1;
  memtn =  I_sdatFTinI(PI) + I_cPixmapIdx(PI)*I_sdatOT(PI);
  if(I_sdatRL(PI))
    {incr = -1;
     memtn=  memtn + I_sdatNdisp(PI) - 1 ;
    }
  
  io = (memtn-1)*nhdrs;
  diff1 = pkey_start - hd[io + Phdr -1];
  diff2 = pkey_end   - hd[io + Phdr -1];
  diff1 = (float) fabs( (double) diff1);
  diff2 = (float) fabs( (double) diff2);
  for (i=0; i<I_sdatNdisp(PI); i ++)
    { io = (memtn-1)*nhdrs;
      pkey= hd[io + Phdr -1];
      diff = pkey_start - pkey;
      diff = (float) fabs( (double) diff);
      if(diff < diff1)
        { diff1 = diff; *first = memtn-1; }
      diff = pkey_end - pkey;
      diff = (float) fabs( (double) diff);
      if(diff < diff2)
        { diff2 = diff; *last = memtn-1; }
      memtn += incr; 
    }

  if (*first == -1 || *last == -1) return False;

  return True;
}


/************************************************************************
 *  ErsSeismicGetSamples
 *    public method to get the selected sample values.
 ***********************************************************************/
float * ErsSeismicGetSamples(Widget widget,
                             void  *Selection,
                             float **header, int *ntrace, int *nsample,
                             float *sample_rate)
{
  struct PlotImage  *PI;
  struct ImageInput *user;
  ErsSeismicAreaSelectionCallbackStruct * selection;
  float *trace, maxv;
  float *hd;
  unsigned char *btrace;
  long   first_trace, last_trace,j,k,count;
  int    first_sample, last_sample;
  Bool status;
  register int i;

/**********************************************************
 * Find whether there is data to work with.             ***
 *********************************************************/
  selection = ( ErsSeismicAreaSelectionCallbackStruct *) Selection;
  *header = NULL;
  PI = (struct PlotImage *) ErsGetPlotInf(widget);
  if(PI == NULL) return NULL;
  user= I_sdatUser(PI);
  hd  = I_sdathd(PI);
  if(hd == NULL) return NULL;
  if(I_sdatXdepth(PI) == 1)
    { if(I_sdatBtr(PI) == NULL) return NULL; }
  else
    { if(I_sdatFtr(PI) == NULL) return NULL; }

  *sample_rate  = user->G.srval*user->tdec;

  /* find which traces are involved in selection */
  if (!ErsSeismicTellSelection(widget, selection->tn_start,
      selection->tn_end, &first_trace, &last_trace,&count))
      return(NULL);

  /* find exact number of traces (we don't count gaps) */
  count = last_trace - first_trace + 1;
  *ntrace = count;

  /* find which samples are involved in the selection */
  ErsSeismicTimeToSample(widget, &(selection->time_start), &first_sample);
  ErsSeismicTimeToSample(widget, &(selection->time_end), &last_sample);
  *nsample = last_sample - first_sample + 1;

  if (*nsample == 0 || *ntrace == 0) return(NULL);

  /* allocate memory for trace */
  trace = (float *) malloc(sizeof(float) * *nsample * *ntrace);

  /* return a pointer to the requested seismic data */
  for(i=first_trace;i<last_trace+1;i++)
    {j = i-first_trace;
     k = I_cPixmapIdx(PI)*I_sdatOT(PI)*I_sdatOS(PI) +
         i*I_sdatOS(PI) + first_sample;
     if(I_sdatXdepth(PI) == 1)
       {btrace = I_sdatBtr(PI) + k;
        maxv = 1.0;
        byte_to_float_(btrace, nsample, nsample,
        &maxv, trace +j**nsample);
       }
     else
       memcpy(trace+j**nsample,I_sdatFtr(PI)+k,*nsample*sizeof(float));
    } 

  /* Set the pointer to the header data for the traces */
  *header = hd + I_sdatnhd(PI)*I_cPixmapIdx(PI)*I_sdatOT(PI) +
            (first_trace-1)*I_sdatnhd(PI);

  /* return in selection actual start and end trace and time values */
  selection->time_start = user->tmin + first_sample*(*sample_rate);
  selection->time_end   = user->tmin + last_sample*(*sample_rate);
  selection->tn_end     = selection->tn_start + *ntrace;

  return(trace);
}

/************************************************************************
 *  ErsSeismicGetTrTiWin
 *    Public method to get the selected sample values.
 *    first_trace & last_trace are indexed from 0.
 *    Data is returned as floats, even if traces are in byte format.
 ***********************************************************************/
float * ErsSeismicGetTrTiWin(Widget widget, long *mems, long *meme,
                             float *time_start, float *time_end,
                             float **header, int *ntrace, int *nsample,
                             float *sample_rate)
{
  struct PlotImage  *PI;
  struct ImageInput *user;
  float *trace, maxv;
  float *hd;
  unsigned char *btrace;
  long   first_trace, last_trace,j,k;
  int    first_sample, last_sample;
  register int i;

/**********************************************************
 * Find whether there is data to work with.             ***
 *********************************************************/
  *header = NULL;
  PI = (struct PlotImage *) ErsGetPlotInf(widget);
  if(PI == NULL) return NULL;
  user= I_sdatUser(PI);
  hd  = I_sdathd(PI);
  if(hd == NULL) return NULL;
  if(I_sdatXdepth(PI) == 1)
    { if(I_sdatBtr(PI) == NULL) return NULL; }
  else
    { if(I_sdatFtr(PI) == NULL) return NULL; }

  *sample_rate  = user->G.srval*user->tdec;

  /* Check the data selection */
  first_trace = *mems;
  last_trace  = *meme;
  if(first_trace > last_trace)
   { printf("GetTrTiWin: Invalid trace range, 1st > last\n");
     return NULL;
   }
  if(first_trace < 0) first_trace=0;
  if(last_trace  > I_sdatFrames(PI)*I_sdatOT(PI)-1) 
    last_trace= I_sdatFrames(PI)*I_sdatOT(PI)-1;
  
  /* find exact number of traces (we don't count gaps) */
  *ntrace = last_trace - first_trace + 1;

  /* find which samples are involved in the selection */
  ErsSeismicTimeToSample(widget, time_start, &first_sample);
  ErsSeismicTimeToSample(widget, time_end, &last_sample);
  *nsample = last_sample - first_sample + 1;

  if (*nsample == 0 || *ntrace == 0) return(NULL);

  /* allocate memory for trace */
  trace = (float *) malloc(sizeof(float) * *nsample * *ntrace);

  /* return a pointer to the requested seismic data */
  for(i=first_trace;i<last_trace+1;i++)
    {j = i-first_trace;
     k = I_cPixmapIdx(PI)*I_sdatOT(PI)*I_sdatOS(PI) +
         i*I_sdatOS(PI) + first_sample;
     if(I_sdatXdepth(PI) == 1)
       {btrace = I_sdatBtr(PI) + k;
        maxv = 1.0;
        byte_to_float_(btrace, nsample, nsample,
        &maxv, trace +j**nsample);
       }
     else
       memcpy(trace+j**nsample,I_sdatFtr(PI)+k,*nsample*sizeof(float));
    } 

  /* Set the pointer to the header data for the traces */
  *header = hd + I_sdatnhd(PI)*I_cPixmapIdx(PI)*I_sdatOT(PI) +
            first_trace*I_sdatnhd(PI);

  /* return in selection actual start and end trace and time values */
  *time_start = user->tmin + first_sample*(*sample_rate);
  *time_end   = user->tmin + last_sample*(*sample_rate);
  *mems = first_trace;
  *meme = last_trace;
  return(trace);
}

/************************************************************************
 *
 *  ErsSeismicGetData
 *    public method to get the selected sample values.
 *    This is similar to ErsSeismicGetTraces, but does not return key
 *    info and has no limitation on the number of traces.
 *    Only considers the traces currently being displayed.
 *
 ***********************************************************************/
float *ErsSeismicGetData(Widget widget, long tn_start, long tn_end,
       float *time_start, float *time_end,int *ntrace,int *nsample)
{
  struct PlotImage  *PI;
  struct ImageInput *user;
  unsigned char *btrace;
  float *trace, srval, maxv;
  long j,k,count;
  long first_trace, last_trace; /* in memory */
  int first_sample, last_sample;
  Bool status;
  register int i;

/**********************************************************
 * Find whether there is data to work with.             ***
 *********************************************************/
  trace = NULL;
  PI = (struct PlotImage *) ErsGetPlotInf(widget);
  if(PI == NULL) return trace;
  user = I_sdatUser(PI);
  if(I_sdatXdepth(PI) == 1)
    { if(I_sdatBtr(PI) == NULL) return NULL; }
  else
    { if(I_sdatFtr(PI) == NULL) return NULL; }
/**********************************************************
 * Find which traces are involved in selection          ***
 *********************************************************/
  if (!ErsSeismicTellSelection(widget, tn_start, tn_end,
                              &first_trace, &last_trace, &count))
    return(NULL);

  /* find exact number of traces (we don't count gaps) */
  *ntrace = count;
  /* *ntrace = last_trace - first_trace + 1; */

  /* find which samples are involved in the selection */
  ErsSeismicTimeToSample(widget, time_start, &first_sample);
  ErsSeismicTimeToSample(widget, time_end, &last_sample);
  *nsample = last_sample - first_sample + 1;

  if (*nsample == 0 || *ntrace == 0) return(NULL);
/**********************************************************
 * Allocate memory for trace.                           ***
 * Retrive the requested data from memory.             ***
 *********************************************************/
  trace = (float *) malloc(sizeof(float) * *nsample * *ntrace);

  for(i=first_trace;i<last_trace+1;i++)
    {k = I_cPixmapIdx(PI)*I_sdatOT(PI)*I_sdatOS(PI) +
         i*I_sdatOS(PI) + first_sample;
     j = i-first_trace;
     if(I_sdatXdepth(PI) == 1)
       {btrace = I_sdatBtr(PI) + k;
        maxv = 1.0;
        byte_to_float_(btrace, nsample, nsample,
        &maxv, trace +j**nsample);
       }
     else
       memcpy(trace+j**nsample,I_sdatFtr(PI)+k,*nsample*sizeof(float));
    } 

  /* adjust start and end time */
  srval = user->G.srval * user->tdec;
  *time_start = user->tmin + first_sample*srval;
  *time_end = *time_start + (*nsample - 1) * srval;

  return trace;
}

/************************************************************************
 *
 *  void ErsSeismicDraw() 
 *    Public function to copy the pixmap or image containing the
 *    seismic data into the widget window and draws the annotations.
 *    This function will call the exposeCallbacks attached to the widget 
 *    if flag expose_callbacks_flag is True.
 *
 ***********************************************************************/

void ErsSeismicDraw(Widget widget,int x,int y,int width,int height,
     Bool expose_callbacks_flag)
{ struct PlotImage *PI;
  Pixmap *pixmary;
  ErsCoordInf      *T;
  Bool image_mode = True;
  static char *msg1 = "Widget plot failed. Change";
  static char *msg2 = "or destroy this widget.";
  static  ErsSeismicCallbackStruct cb;
  static XEvent event;

  if(widget == NULL) return;
  if (!XtIsRealized(widget)) return;
/**********************************************************
 * Get data necessary for continuing.                   ***
 *********************************************************/
  PI = (struct PlotImage *) ErsGetPlotInf(widget);
  if(PI == NULL) return;
/**********************************************************
 * set cursor for window first time we enter this routine**
  if (!widget->seismic.cursor_set) {
    if (widget->seismic.error_flag)
      XDefineCursor(XtDisplay(widget), XtWindow(widget),
                    widget->seismic.X_cursor);
    else
      XDefineCursor(XtDisplay(widget), XtWindow(widget),
                    widget->seismic.current_cursor);
    widget->seismic.cursor_set = True;
  }
 *********************************************************/
/**********************************************************
 * Check the pointer position and box limits.           ***
 * (x,y) must lie within the drawable area.             ***
 *********************************************************/
  if (x > I_gphWid(PI) || y > I_gphHght(PI)) return;
  if (x + width > I_gphWid(PI)) width = I_gphWid(PI) - x;
  if (y + height > I_gphHght(PI)) height =I_gphHght(PI) - y;
/**********************************************************
 * Are we drawing from an image or a pixmap.            ***
 *********************************************************/
   /* draw from pixmap case */
 pixmary = I_PixmapA(PI);
 if (pixmary[I_cPixmapIdx(PI)] != 0)
     refresh( PI, x, y, width, height );
 else
  {/* error message */
   XDrawString(XtDisplay(widget), XtWindow(widget),
                   I_sdatGC2(PI), 20, 40, msg1, strlen(msg1));
   XDrawString(XtDisplay(widget), XtWindow(widget),
                   I_sdatGC2(PI), 20, 70, msg2, strlen(msg2));
  }

  /* take care of cursor */
  ErsRepaintCursor(widget, x, y, width, height);

  /* redraw highlight */
  /*
  if (widget->primitive.highlighted)
    _XmHighlightBorder(widget);
  else if (_XmDifferentBackground(widget, XtParent(widget)))
    _XmUnhighlightBorder(widget);
  */

/* call expose callbacks attached to the widget */
  if (expose_callbacks_flag)
   {event.xexpose.x = x;
    event.xexpose.y = y;
    event.xexpose.width = width;
    event.xexpose.height = height;
    cb.reason = XmCR_EXPOSE;
    cb.event = &event;
    cb.window = XtWindow(widget);
    XtCallCallbacks(widget, XmNexposeCallback, &cb);
   }
}

/************************************************************************
 *
 * ErsRepaintCursor
 * when an area of the plot has been repainted that crosses the cursor
 * we need to repaint the cursor in that area.
 *
 ***********************************************************************/
static void ErsRepaintCursor(Widget widget,int xs,int ys,int width,int height)
{
  Window wroot,wchild;
  PR_ *pikrec;
  struct PlotImage *PI;
  int rx,ry,wx,wy;
  unsigned int keys;
  int ox, oy;
  int xe, ye;

/**********************************************************
 * Get data necessary for continuing.                   ***
 *********************************************************/
  if(widget != NULL ) return; /* temporary patch */
  pikrec = (PR_ *) ErsGetPickingRecord(widget);
  if(pikrec == NULL) return;
  PI = (struct PlotImage *) ErsGetPlotInf(widget);
  if(PI == NULL) return;
  XQueryPointer(XtDisplay(widget),XtWindow(widget),
     &wroot, &wchild, &rx,&ry,&wx,&wy,&keys);
/*
  if (widget->seismic.cursor_type == XmX_CURSOR ||
      widget->seismic.error_flag) return;
*/

  ox = wx; /*widget->seismic.ox; */
  oy = wy; /*widget->seismic.oy; */
  xe = xs + width;
  ye = ys + height;

  if (xs < I_gphXorigin(PI) + I_sdatFTL(PI))
    xs = I_gphXorigin(PI) + I_sdatFTL(PI);
  if (ys < I_gphYorigin(PI) )
    ys = I_gphYorigin(PI);

  if (xe > I_gphXorigin(PI) + I_sdatXwdth(PI)) 
      xe = I_gphXorigin(PI) + I_sdatXwdth(PI);
  if (ye > I_gphYorigin(PI) + I_sdatXht(PI)) 
      ye = I_gphYorigin(PI) + I_sdatXht(PI);

  if (ox >= xs && ox <= xe)
    XDrawLine(XtDisplay(widget), XtWindow(widget),
              I_sdatGC1(PI),ox, ys, ox, ye);

  if (oy >= ys && oy <= ye)
    XDrawLine(XtDisplay(widget), XtWindow(widget),
              I_sdatGC1(PI), xs, oy, xe, oy);
}
