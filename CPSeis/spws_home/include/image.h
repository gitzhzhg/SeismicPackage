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
/*-------------------------------------------------------------------------
 *USER DOC
 * File        : image.h
 * Author      : Michael L. Sherrill 
 * Date        : 11/1/91
 *
 *
 *NOTES:
 * 1. Header file to include for seismic image generation.
 *
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *
 *END DOC
 * -------------------------------------------------------------------------*/

#ifndef __IMAGEDEF

#define __IMAGEDEF


#include "tfio.h"
#include "wproc.h"
#include "X11/Intrinsic.h"

#ifdef SUNOS
#include "spws_sunos.h"
#endif

/* maximum number of pixmaps and zooms*/
#define MAX_PIXMAP 1000
#define MAX_ZOOM   200
#define NO_ZOOM    100

/*mouse readout types*/
#define MOUSE_AMP 0
#define MOUSE_VEL 1
#define MOUSE_AUX 2

/*   Setup Plot Status Returns */
#define ReadFail       -1
#define ResourceFail   -2
#define EofFail        -3
#define UserAbort      -4
#define PlotSuccess     1
#define PlotWarning     2

/*byte defaults status returns*/
#define DEFS_OK   0
#define DEFS_FAIL 1

/* application definitions */
#define App_CBYT   1

/* seismic image plot types */
#define PlotWONLY   1
#define PlotWFILL   2
#define PlotGS      3
#define PlotCOLOR   3          /* used to call it PlotGS */
#define PlotCONTOUR 4
#define PlotSEMB    5
#define PlotGRID    6
#define PlotISO     7
#define PlotARRAY   7          /* used to call it PlotISO */
#define PlotHEADER  8

/* color map color schemes used by defined_color.c*/
#define STANDARD    1
#define STANDARD2   2
#define WHITERED    3
#define BLUEYELLOW1 4
#define BLUEYELLOW2 5
#define SEMBLANCE   6
#define SEMBLANCE2  7
#define CONTOUR     8
#define GRAY        9
#define ALLISTAIR1 10
#define ALLISTAIR2 11
#define ALLISTAIR3 12
#define ALLISTAIR4 13
#define ALLISTAIR5 14
#define ALLISTAIR6 15
#define ALLISTAIR7 16
#define ALLISTAIR8 17
#define SECTOR     18
#define MEDRAM     19
#define MAXNAMEDCOLORS 19


/* image offset (border width in pixels) */
#define BORDER 50

/*headers to use for lav*/
#define OURLAVHDR 31
#define CPSLAVHDR 24

/*currently set maximum headers equal to cps*/
#define MAXHDRS 64

/*zoom type control constants*/
#define Z_ORIGINAL 0
#define Z_UP       1
#define Z_DOWN     2

/*scan constants*/
#define ScanLeft  1
#define ScanRight 2


#define ImageAll -1

enum outtype { XYOFF, XYAUTO, XYNOAUTO };

enum  {PANELNORM,NORM,FILENORM};

/* macros for determining pixels in an inch */
#define HorzPixelsPerInch(dsp, scrn) (int)(((float) DisplayWidth(dsp,scrn)) / \
                                 (0.0394 * DisplayWidthMM(dsp, scrn)))

#define VertPixelsPerInch(dsp, scrn) (int)(((float) DisplayHeight(dsp,scrn)) / \
                                 (0.0394 * DisplayHeightMM(dsp, scrn)))


/* macros that return number of pixels within a requested millimeter size*/
#define HorzPixelsFromMM(dsp, scrn, mm)\
(int)(mm * ((float)DisplayWidth(dsp,scrn) / (float)DisplayWidthMM(dsp, scrn)))

#define VertPixelsFromMM(dsp, scrn, mm)\
(int)(mm * ((float)DisplayHeight(dsp,scrn) / (float)DisplayHeightMM(dsp, scrn)))

/* macros for min and maximum value*/
#define max(a, b) (  (a) > (b) ? (a) : (b)  ) 
#define min(a, b) (  (a) < (b) ? (a) : (b)  )



/*------------------------------------------------------------------------
   Macros to reference PlotImage struct
  -----------------------------------------------------------------------*/

/*  I_gphWidget    = drawing area widget                             */
/*  I_gphCmap      = color map                                       */
/*  I_gphStatusW   = widget to contain status message                */
/*  I_gphXorigin   = pixel location of Xmin (min user X coord)       */
/*  I_gphYorigin   = pixel location of Ymin (min user Y coord)       */

/*  I_sdatnhd      = number of words in each trace header            */
/*  I_sdatNdisp    = number of traces in the display                 */
/*  I_sdatNsamp    = number of samples in each trace in the display  */
/*  I_sdatNsampMem = number of samples in each trace in memory       */
/*  I_sdatTminMem  = time for first trace sample in memory           */
/*  I_sdatTmaxMem  = time for  last trace sample in memory           */
/*  I_sdatTmin     = time for first displayed trace sample           */
/*  I_sdatTmax     = time for  last displayed trace sample           */
/*  I_sdatDT       = trace sample rate (seconds per trace sample)    */
/*  I_sdatYperpix  = y value per pixel (seconds per pixel)           */
/*  I_sdatTdelta   = pixel width of each trace                       */
/*  I_sdatXdest    = x offset of destination drawable to put image   */
/*  I_sdatYdest    = y offset of destination drawable to put image   */
/*  I_sdatBytefile = name of byte file                               */
/*  I_sdatHeaders  = pointer to first   header   in the display      */
/*  I_sdatTraces   = pointer to first byte trace in the display      */
/*  I_sdatIndex    = index of trace in memory which is the first     */
/*                        trace in the display                       */


                    /** image graphics info **/

#define image_status_wig(image, w)  ( (image)->statusw= (w) )
#define I_gphXOUT(image, w)      ( (image)->xydisp.xloc = (w) )
#define I_gphYOUT(image, w)      ( (image)->xydisp.yloc = (w) )
#define I_gphAOUT(image, w)      ( (image)->xydisp.aout = (w) )
#define I_gphAOUTTYPE(image,type) ( (image)->xydisp.mouse_readout_type = (type))
#define I_gphDisp(image)         ( (image)->displayed )
#define I_gphWidget(image)       ( (image)->graphic )
#define I_gphWin(image)          ( XtWindow( (image)->graphic ) )
#define I_gphWid(image)          ( (image)->graph_width )
#define I_gphHght(image)         ( (image)->graph_height )
#define I_gphCinfo(image)        ( (image)->col )
#define I_gphCmap(image)         ( (image)->col->cmap )
#define I_gphStatusW(image)      ( (image)->statusw )
#define I_SetCinfo(image,c)      ( (image)->col= (c) )
#define I_SetC2info(image,c)     ( (image)->col_two= (c) )
#define I_gphSetLeftBorder(image,b)   ( (image)->left_border= (b) )
#define I_gphSetRightBorder(image,b)  ( (image)->right_border= (b) )
#define I_gphSetTopBorder(image,b)    ( (image)->top_border= (b) )
#define I_gphSetBottomBorder(image,b) ( (image)->bottom_border= (b) )
#define I_gphLeftBorder(image)        ( (image)->left_border )
#define I_gphRightBorder(image)       ( (image)->right_border )
#define I_gphTopBorder(image)         ( (image)->top_border )
#define I_gphBottomBorder(image)      ( (image)->bottom_border )
#define I_gphOverLeftBorder(image)    ( (image)->over_image->left_border )
#define I_gphOverRightBorder(image)   ( (image)->over_image->right_border )
#define I_gphOverTopBorder(image)     ( (image)->over_image->top_border )
#define I_gphOverBottomBorder(image)  ( (image)->over_image->bottom_border )
#define I_gphPickGC(image)            ( (image)->pick_gc )
#define I_gphXorigin(image)  ( I_gphLeftBorder(image) + I_sdatXdest(image) )
#define I_gphYorigin(image)  ( I_gphTopBorder (image) + I_sdatYdest(image) )
#define I_gphOverXorigin(image)  ( I_gphOverLeftBorder(image) \
                                  + I_sdatOverXdest(image) )
#define I_gphOverYorigin(image)  ( I_gphOverTopBorder (image) \
                                  + I_sdatOverYdest(image) )
                     /** image pixmap info **/

#define I_cPixmap(image)        ( (image)->pixmary[(image)->cpixm] )
#define I_PixmapA(image)        ( (image)->pixmary )
#define I_SetcPixmap(image,i)   ( (image)->cpixm= (i) )
#define I_cPixmapIdx(image)     ( (image)->cpixm )

                    /** image seismic data info **/

#define I_sdatTmin(image)       ( (image)->tmin )
#define I_sdatTmax(image)       ( (image)->tmax )
#define I_sdatXmin(image)       ( (image)->grid_x1 )
#define I_sdatXmax(image)       ( (image)->grid_x2 )
#define I_sdatYmin(image)       ( (image)->grid_y1 )
#define I_sdatYmax(image)       ( (image)->grid_y2 )
#define I_sdatnhd(image)        ( (image)->nhdrs )
#define I_sdatActiveHdr(image)  ( (image)->user->active_header )
#define I_sdathd(image)         ( (image)->hd )
#define I_sdatXht(image)        ( (image)->ximage.height )
#define I_sdatOverXht(image)    ( (image)->over_image->ximage.height )
#define I_sdatXwdth(image)      ( (image)->ximage.width )
#define I_sdatOverXwdth(image)  ( (image)->over_image->ximage.width )
#define I_sdatXdepth(image)     ( (image)->ximage.depth )
#define I_sdatTdelta(image)     ( (image)->trace_delta )
#define I_sdatXdest(image)      ( (image)->dest_x )
#define I_sdatYdest(image)      ( (image)->dest_y )
#define I_sdatOverXdest(image)      ( (image)->over_image->dest_x )
#define I_sdatOverYdest(image)      ( (image)->over_image->dest_y )
#define I_sdatXperpix(image)    ( (image)->x_value_per_pixel )
#define I_sdatYperpix(image)    ( (image)->y_value_per_pixel )
#define I_sdatUM(image)         ( (image)->user->mode )
#define I_sdatFTL(image)        ( (image)->first_trace_location )
#define I_sdatFTinI(image)      ( (image)->FirstTraceInImage )
#define I_sdatNdisp(image)      ( (image)->DisplayedTraces )
#define I_sdatNsamp(image)      ( (image)->DisplayedSamples )
#define I_sdatRL(image)         ( (image)->user->RtoL )
#define I_sdatOT(image)         ( (image)->original_traces )
#define I_sdatOS(image)         ( (image)->original_samples )
#define I_sdatUser(image)       ( (image)->user )
#define I_sdatFrames(image)     ( (image)->user->frames )
#define I_sdatFtr(image)        ( (image)->float_array )
#define I_sdatBtr(image)        ( (image)->byte_array )
#define I_sdatGC1(image)        ( (image)->gc1 )
#define I_sdatGC2(image)        ( (image)->gc2 )
#define I_sdatPickGC(image)     ( (image)->pick_gc )
#define I_sdatTminMem(image)    ( (image)->user->G.tstrt +   \
                             ((image)->Cl.samp1 - 1) * (image)->user->G.srval )
#define I_sdatTmaxMem(image)    ( (image)->user->G.tstrt +   \
                             ((image)->Cl.samp1 - 1 +             \
                             (image)->Cl.nsamp - 1) * (image)->user->G.srval )
#define I_sdatNsampMem(image)   ( (image)->Cl.nsamp )
#define I_sdatNtotMem(image)    ( (image)->Cl.ntot  )
#define I_sdatDT(image)         ( (image)->user->G.srval * (image)->Cl.sdec )
#define I_sdatBytefile(image)   ( (image)->user->filename )
#define I_sdatIndex(image)  \
   ( I_cPixmapIdx(image) * I_sdatNtotMem(image) + I_sdatFTinI(image) - 1)



#define DisplayedHeaderData(image) \
   ( &((image)->hd[I_sdatIndex(image) * I_sdatnhd(image)]))

#define DisplayedByteTraceData(image)   \
   ( &((image)->byte_array[I_sdatIndex(image) * I_sdatNsampMem(image) + \
     (int)(((image)->zoomyary[(image)->zindex][0]-I_sdatTminMem(image))   / \
      ((image)->user->G.srval * (image)->user->tdec) + 0.5)] ))

#define DisplayedFloatTraceData(image)   \
   ( &((image)->float_array[I_sdatIndex(image) * I_sdatNsampMem(image)+ \
     (int)(((image)->zoomyary[(image)->zindex][0]-I_sdatTminMem(image))   / \
      ((image)->user->G.srval * (image)->user->tdec) + 0.5)] ))

#define MemByteTraceData(image) \
   ( &((image)->byte_array[I_cPixmapIdx(image) * I_sdatNtotMem(image)\
                           * I_sdatNsampMem(image)]))

#define MemFloatTraceData(image) \
   ( &((image)->float_array[I_cPixmapIdx(image) * I_sdatNtotMem(image)\
                             * I_sdatNsampMem(image)]))

#define MemHeaderData(image) \
   ( &((image)->hd[I_cPixmapIdx(image) * I_sdatNtotMem(image) \
                   * I_sdatnhd(image)]))

#define I_sdatHeaders(image) ( DisplayedHeaderData(image) )

#define I_sdatTraces(image)  ( DisplayedByteTraceData(image) )


/* Tom may use later
#define I_sdatHeaders(image)  \
   ( &((image)->hd[((image)->FirstTraceInImage - 1) *
I_sdatnhd(image)]))
#define I_sdatTraces(image)   \
   ( &((image)->byte_array[((image)->FirstTraceInImage - 1)  \
    I_sdatNsampMem(image)]))
*/



/*------------------------------------------------------------------------
                  transformation macros 
  -----------------------------------------------------------------------*/

/* trace_number = 1 thru I_sdatNdisp        (in current display)     */
/* sample_index = 0 thru I_sdatNsampMem - 1 (entire trace in memory) */
/* transformations between pixels and time refer to current display  */
/* transformations between sample index and time refer to traces in memory */
/* transformations between xpixels and x refer to current display  */
/* transformations between ypixels and y refer to current display  */
/* actually, the variables time and y are equal */

#define I_get_xpixel_from_x(image,x)  \
( (int)( I_gphXorigin(image) + 0.5 + I_sdatFTL(image) +\
         ((x) - I_sdatXmin(image)) / I_sdatXperpix(image) ) )

#define M_get_xpixel_from_x(image,x)  \
( (int)( I_gphXorigin(image) + 0.5 + I_sdatFTL(image) +\
         ((x) - (image)->manual_grid_x1) / (image)->manual_x_value_per_pixel ) )

#define I_get_x_from_xpixel(image,xpixel)  \
( (float)( I_sdatXmin(image) + (float)((xpixel - I_sdatFTL(image)) -\
                                 I_gphXorigin(image)) * I_sdatXperpix(image)) )

#define M_get_x_from_xpixel(image,xpixel)  \
( (float)( (image)->manual_grid_x1 + (float)((xpixel) - I_gphXorigin(image))  \
                                         * (image)->manual_x_value_per_pixel) )

#define I_get_ypixel_from_y(image,y)  \
( (int)( I_gphYorigin(image) + 0.5 +      \
         ((y) - I_sdatYmin(image)) / I_sdatYperpix(image) ) )


#define M_get_ypixel_from_y(image,y)  \
( (int)( I_gphYorigin(image) + 0.5 +      \
         ((y) - (image)->manual_grid_y1) / (image)->manual_y_value_per_pixel ) )


#define I_get_y_from_ypixel(image,ypixel)  \
( (float)( I_sdatYmin(image) + (float)((ypixel) - I_gphYorigin(image))    \
                                                * I_sdatYperpix(image)) )

#define M_get_y_from_ypixel(image,ypixel)  \
( (float)( (image)->manual_grid_y1 + (float)((ypixel) - I_gphYorigin(image))  \
                                          * (image)->manual_y_value_per_pixel) )

#define I_get_ypixel_from_time(image,time)  \
( (int)( I_gphYorigin(image) + 0.5 +      \
         ((time) - I_sdatTmin(image)) / I_sdatYperpix(image) ) )

#define I_get_time_from_ypixel(image,ypixel)  \
( (float)( I_sdatTmin(image) + (float)((ypixel) - I_gphYorigin(image))    \
                                                * I_sdatYperpix(image)) )

#define I_get_xpixel_from_trace_number(image,trace_number)  \
   ( (int)( I_gphXorigin(image) + 0.5 +                  \
                 ((float)(trace_number) - 0.5) * I_sdatTdelta(image) ) )

#define I_get_trace_number_from_xpixel(image,xpixel)  \
   ( (long)( I_get_x_from_xpixel(image,xpixel) + .5) )

#define I_get_sample_index_from_time(image,time)  \
   ( (int)( ((time) - I_sdatTminMem(image)) / I_sdatDT(image) + 0.5 ) )

#define I_get_time_from_sample_index(image,sample_index)  \
   ( (float)( I_sdatTminMem(image) + I_sdatDT(image) * (sample_index) ) )

#define I_get_header_value(image,trace,header) \
   ( (float) ((image)->hd[(trace - 1) * (image)->nhdrs + header - 1] ) ) 

#define I_set_x1(image, x1)      ( (image)->grid_x1 = (x1) )
#define I_set_x2(image, x2)      ( (image)->grid_x2 = (x2) )
#define I_set_y1(image, y1)      ( (image)->grid_y1 = (y1) )
#define I_set_y2(image, y2)      ( (image)->grid_y2 = (y2) )

typedef Boolean abort_function(void*);
typedef void    output_function(void*, int x, int y);
typedef void    externalFunction(void*);
/*------------------------------------------------------------------------
 This structure controls the xy output as the mouse moves
 -----------------------------------------------------------------------*/
struct XYdisp {         
                Widget           xloc;
                Widget           yloc;
                Widget           aout;
                char             *override_x_str;
                char             *override_y_str;
                enum   outtype   status;
                Boolean          init;
                long             mouse_readout_type;
                output_function  *outfunc;/* called with text widget write */
                void             *outdata;/* data for function */
                Boolean          interpolate_readout;
                float            not_defined_value;
              };

/*------------------------------------------------------------------------
 Structure to control overlay traces (not used anymore but need to keep
 it around until oldcbyt goes away)
 -----------------------------------------------------------------------*/
struct Overlay
       {
       char           filename[200];       /*trace data file*/
       long           iskp;                /*traces to initially skip*/
       long           ndo;                 /*traces to do between skip do*/
       long           nplt;                /*traces to display*/
       long           nskp;                /*traces to skip between skip do*/
       long           tdec;                /*sample decimation in the read*/
       long           norm;                /*normalize the data when read*/
       long           rp;                  /*reverse data polarity*/
       float          ct;                  /*channels per trace in display*/
       float          tmin;                /*starting time*/
       float          tmax;                /*ending time*/
       long           nhdrs;               /* # of headers in each trace*/
       long           overlay_hdr;         /*use this header to match data*/
       long           image_hdr;           /*use this header to match overlay*/
       struct GLBL    G;                   /*byte data global information*/
       float          *float_array;        /*to read in float data*/
       float          *hd;                 /*to hold header data*/
       unsigned char  *byte_array;         /*to hold byte file data*/
       struct Cntrl   Cl;                  /*data read control*/
       };

/*-------------------------------------------------------------------------
 This structure contains user input variables to control the reading of
 data, size the display, and annotate the image. This is the main structure
 to populate for image generation. 
 --------------------------------------------------------------------------*/
struct ImageInput 
       {
       long         do_color;         /*do cps type color processing*/
       long         do_median;        /*rescale to median of data*/
       long         do_percent;       /*calculate color percentages*/
       long         do_amps;          /*scale color to trace amplitudes*/
       long         pnc;              /*percent of negative to color*/
       long         ppc;              /*percent of positive to color*/ 
       float        color_ampmin;     /*min amplitude to color*/
       float        color_ampmax;     /*max amplitude to color*/ 
       float        cbar_rgb[1024];   /*cps color file rgb values*/
       int          use_external_amp; /*use external amplitude to scale by*/
       double       external_amplitude;/*scale multiple data sets with*/
       double       external_scaler;  /*scale multiple data sets with*/
       long         num_cbar_cols;    /*number of colors in cps color file*/
       long         overlay;          /*overlay trace increment*/
       long         mode;             /*wiggle trace or variable density*/
       long         movie;            /*boolean for movie option*/
       long         frames;           /*number of images to create*/
       long         skip_frames;      /*number of frames between to skip*/
       char         filename[200];    /*trace data file*/
       long         iskp;             /*traces to initially skip*/
       long         ndo;              /*traces to do between skip do*/
       long         nplt;             /*traces to display*/
       long         nskp;             /*traces to skip between skip do*/
       float        tmin;             /*first time to read*/
       float        tmax;             /*ending time to read*/
       long         tdec;             /*sample decimation in the read*/
       long         norm;             /*normalize the data when read*/
       Boolean      scale_to_file;    /*scale data to lav of entire file*/
       Boolean      scale_to_panel;   /*scale to lav of image data displayed*/
       long         gradev;           /*vertical variable density grade*/
       long         gradeh;           /*horizontal variable density grade*/
       long         rp;               /*reverse data polarity*/
       long         RtoL;             /*display data right to left*/
       long         invert_yaxis;     /*invert the y of the image*/
       float        ti;               /*traces per inch of display*/
       float        is;               /*inches per second of display*/
       long         metric;           /*plot in metric scale*/
       long         depth;            /*plot in depth instead of time*/
       float        ct;               /*channels per trace in display*/
       float        clipping_factor;  /*to be implemented later*/
       long         match_xheader;    /*x header to match different data sets*/
       long         match_yheader;    /*y header to match different data sets*/
       long         hdrOne;           /*primary header annotation*/
       long         hdrTwo;           /*secondary header annotation*/
       long         firstLbl;         /*trace to start annotation*/
       long         active_header;    /*general purpose header reference*/
       long         y_axis_header;    /*header to label y axis by*/
       long         num_y_labels;     /*number of y axis labels to annotate*/
       Boolean      draw_x_lines;     /*draw lines thru image at x label locs*/
       Boolean      draw_y_lines;     /*draw lines thru image at y label locs*/
       Boolean      label_by_headers; /*use header values to annotate by*/
       Boolean      symetrical_anno;  /*make x and y annotation symetrical*/
       int          annotate;         /*true or false to annotate the image*/
       long         LblInc;           /*increment of annotation*/
       double       ptl;              /*primary timing line to annotate*/
       double       stl;              /*secondary timing line to annotate*/
       char         plotLabel[200];   /*user plot label*/
       int          plotlabel_x;      /*x location for plot label*/
       int          plotlabel_y;      /*y location for plot label*/
       Boolean      plotlabel_only;   /*only annotate a label*/
       struct GLBL  G;                /*byte data global information*/
       long         color_data;       /*data is in color or gray*/
       float        minp;             /*minimum velocity percent to contour*/
       float        maxp;             /*maximum velocity percent to contour*/
       long         contours;         /*contour level to draw*/
       float        grid_x1;          /*user starting x grid coordinate*/
       float        grid_x2;          /*user ending   x grid coordinate*/
       float        grid_y1;          /*user starting y grid coordinate*/
       float        grid_y2;          /*user ending   y grid coordinate*/
       float        grid_xinc;        /*user x grid increment*/
       float        grid_yinc;        /*user y grid increment*/
       long         grid_width;       /*pixel width  of grid*/
       long         grid_height;      /*pixel height of grid*/
       float        vel_min;          /*minimum image velocity*/
       float        vel_max;          /*maximum image velocity*/
       long         vary_size;        /*size of array to store velocities*/
       };



/*---------------------------------------------------------------------------
 This structure contains all the variables required to produce the Xlib image.
 ---------------------------------------------------------------------------*/
struct PlotImage 
       {
       Boolean           filedata;             /*using data from a file*/
       long              zoomed;               /*the display is zoomed*/
       long              zoomdown;             /*zoom down the display*/
       long              zoomup;               /*zoom up the display*/
       long              zoom_scan;            /*zoomed and scanned*/
       double            zoom_factor;          /*zoom factor*/
       long              scanright;            /*getting next traces*/
       long              scanleft;             /*getting previous traces*/
       long              displayed;            /*for front end*/
       long              trace_delta;          /*pixel width of traces*/
       long              first_trace_location; /*in the display*/
       long              nhdrs;                /* # of headers in each trace*/
       long              frames;               /* current # displayed frames*/
       long              skip_frames;          /* skip between frames*/
       float             ti;                   /*the image's traces per inch*/
       float             is;                   /*the image's inches per sec*/
       long              original_samples;     /*before zoom*/
       long              original_traces;      /*before zoom*/
       double            original_is;          /*inches per second before zoom*/
       double            original_ti;          /*traces per inch before zoom*/
       float             original_velfactor;   /*original pixel to vel ratio*/
       long              use_old_arrays;       /*flag to reallocate arrays*/
       long              zindex;               /*zoom index 0-199, 100=no zoom*/
       double            zoomxary[MAX_ZOOM][2];/*x data in the zoom*/
       double            zoomyary[MAX_ZOOM][2];/*y data in the zoom*/
       long              neg_fill;             /*fill in troughs instead*/
       long              FirstTraceInImage;    /*1st trace in display*/
       long              DisplayedTraces;      /* # of traces in display*/
       long              DisplayedSamples;     /* # of samples in display*/
       long              ntot;                 /* # of traces to read */
       long              nsamp;                /* # of samples to read */
       long              tpnl[MAX_PIXMAP];     /* # traces per image */
       double            tmin;                 /*first time in display*/
       double            tmax;                 /*last time in display*/
       Boolean           manual_annotate;      /*use manual annotation*/
       float             manual_grid_y1;       /*value to over-ride data*/
       float             manual_grid_y2;       /*value to over-ride data*/
       float             manual_grid_x1;       /*value to over-ride data*/
       float             manual_grid_x2;       /*value to over-ride data*/
       double            manual_y_value_per_pixel;/*computed to over-ride data*/
       double            manual_x_value_per_pixel;/*computed to over-ride data*/
       double            manual_scaler;      /*ratio of over-ride to real data*/
       float             grid_x1;              /*the image's uc x1*/
       float             grid_x2;              /*the image's uc x2*/
       float             grid_y1;              /*the image's uc y1*/
       float             grid_y2;              /*the image's uc y2*/
       double            y_value_per_pixel;    /*for mouse y readout*/
       double            x_value_per_pixel;    /*for mouse x readout*/
       double            max_amplitude;        /*max in the data read in*/
       double            min_amplitude;        /*min in the data read in*/
       double            color_ratio;          /*index to color map*/
       double            display_scale;        /*to fit data into display*/
       double            aux_value;            /*external value for readout*/
       char              *aux_label;           /*external value for readout*/
       float             *float_array;         /*to read in float data*/
       float             *hd;                  /*to hold header data*/
       unsigned char     *byte_array;          /*to hold byte file data*/
       XSegment          *segments;            /*to hold xlib drawing of xys*/
       long              num_segs;             /*number of segments in array*/
       XImage            ximage;               /*Xlib image struct*/
       struct ImageInput *user;                /*user input struct*/
       long              graph_width;          /*width of pixmap */
       long              graph_height;         /*height of pixmap */
       long              left_border;          /*border pixels*/
       long              right_border;         /*border pixels*/
       long              top_border;           /*border pixels*/
       long              bottom_border;        /*border pixels*/
       long              dest_x;               /*drawable x destination*/
       long              dest_y;               /*drawable y destination*/
       long              orig_dest_x;          /*before any dragging*/
       long              orig_dest_y;          /*before any dragging*/
       Pixmap            pixmary[MAX_PIXMAP];  /*all pixmaps used*/
       Pixmap            bitmap_pixmap;        /*for single plane*/
       int               cpixm;                /*current pixmap displayed */
       GC                gc1;                  /*normal context for graph*/
       GC                gc2;                  /*bold context for annotation*/
       GC                pick_gc;              /*pick marking*/
       GC                bitmap_gc1;           /*for single plane*/
       GC                bitmap_gc2;           /*for single plane*/
       XFontStruct       *font_fixed;          /*fixed font structure*/
       XFontStruct       *font_bold;           /*bold font structure*/
       long              boldcharwidth;        /*pixel width of char in gc*/
       long              boldcharheight;       /*pixel height of char in gc*/
       long              fixedcharheight;      /*pixel height of char in gc*/
       long              fixedcharwidth;       /*pixel height of char in gc*/
       struct COLOR_INFO *col;                 /*data color context*/
       struct COLOR_INFO *col_two;             /*secondary data color context*/
       struct Cntrl      Cl;                   /*data read control*/
       unsigned char     MapColors[256];       /*color map index for the image*/
       char              graph_font[200];      /*graphic font to use*/
       Window            graph_window;         /*window for graph*/
       long              frame_buffer;         /*is device frame buffered*/
       Widget            graphic;              /*widget id for graph*/
       long              histogram[256];       /*amplitude history*/
       void              (*proc)();            /*cps processing function*/
       void              *procdata;            /*cps processing data*/
       externalFunction  *external_function;   /*function image will call*/
       void              *external_object;     /*data for extern_function*/
       abort_function    *abort_image;         /*abort image function */
       void              *abort_data;          /*abort data */
       struct XYdisp     xydisp;               /*automatic xy output */
       float             median_scale;         /*scaler for median data*/
       Widget            statusw;              /*show status - if defined */
       float             vel_min;              /*minimum image velocity*/
       float             vel_max;              /*maximum image velocity*/
       float             *vary;                /*velocity array*/
       float             velfactor;            /*pixel transform*/
       float             *xloc;                /*array of x locations*/
       float             *yloc;                /*array of y locations*/
       long              traces_per_group;     /*original cvst traces in group*/
       struct Overlay    over;                 /*overlay structure*/
       long              height_diff;          /*overlay and parent difference*/
       Boolean           can_overlay;          /*enough colors to overlay*/
       Boolean           underlay_only;        /*no overlay*/
       Boolean           point_to_data;        /*using someones data dont free*/
       Boolean           point_to_headers;     /*using someones headers*/
       Pixel             overlay_pixel;        /*overlay gc pixel*/
       Pixel             white_pixel;          /*image background*/
       Pixel             black_pixel;          /*image foreground*/
       Pixel             grid_pixel;           /*grid image foreground*/
       Pixel             background_pixel;     /*active background pixel*/
       Pixel             foreground_pixel;     /*active foreground pixel*/
       struct PlotImage  *chain_image;         /*another image to plot*/
       struct PlotImage  *over_image;          /*image that overlays this one*/
       };

typedef struct ImageInput *ImageInputPtr;
typedef struct PlotImage  *PlotImagePtr;






/*----------------------------------------------------------------------------
                         function prototypes                      
-----------------------------------------------------------------------------*/

#ifdef __cplusplus  
extern "C" {                          /* for C++ */
#endif



/*============================================================================
  Checks the size and memory requirements of image
=============================================================================*/
long check_size(               Widget            w,
                               struct PlotImage  *image,
                               char              *errstr);

/*============================================================================
  Initializes the image structure
=============================================================================*/
void init_image_struct(        struct PlotImage  *image,
                               struct ImageInput *user,
                               Widget            Graphic,
                               char              *graph_font,
                               char              *small_graph_font,
                               struct COLOR_INFO *col,
                               struct COLOR_INFO *col_two,
                               Boolean           frame_buff);

/*============================================================================
  Contains the rgbz values of color bars
=============================================================================*/
void defined_color(            float             *cbar,
                               long              barnumber,
                               long              *numcolors,
                               long              *tracevalues,
                               long              maxcolors);

/*============================================================================
  Dynamically compresses or expands the color map
=============================================================================*/
long color_compress(           Widget            w,  
                               float             *rgb, 
                               long              numcolors,
                               long              *compression, 
                               long              *intensity,
                               struct COLOR_INFO *col,
                               struct PlotImage  *image,
                               float             *rgb_out);

/*============================================================================
  Loads and stores the xlib colors
=============================================================================*/
void loadcolors(               Widget            w,
                               struct PlotImage  *image,
                               long              predef,
                               struct COLOR_INFO *col_cust,
                               struct COLOR_INFO *col_gs,
                               long              *numcolors,
                               float             *rgb,
                               long              *change_colors );

/*============================================================================
  Labels velocities
=============================================================================*/
void label_velocity(           float             *rgb,
                               float             vmin,
                               float             vmax,
                               long              numcolors);

/*============================================================================
  Draws a color bar on a widget
=============================================================================*/
void drawcbar(                 Widget            w, 
                               struct PlotImage  *image );

/*============================================================================
  Reads in a cps color bar file
=============================================================================*/
long get_cps_cbar(             char              *filename,
                               float             *rgb, 
                               long              *numcolors,
                               long              *tracevalues);

/*============================================================================
  Redraws a pixmap
=============================================================================*/
void refresh(                  struct PlotImage *image,
                               long              x,
                               long              y,
                               long              width,
                               long              height);

/*============================================================================
  Redraws a pixmap
=============================================================================*/
void refresh_main(             struct PlotImage  *image,
                               long              x,
                               long              y,
                               long              width,
                               long              height,
                               long              dest_x,
                               long              dest_y,
                               Boolean           border_only,
                               Widget            w);

/*============================================================================
  Redraws a subset of traces
=============================================================================*/
void refresh_traces(           struct PlotImage  *image,
                               long              first_trace, 
                               long              last_trace, 
                               float             first_time, 
                               float             last_time);

/*============================================================================
  Redraws picks on an image
=============================================================================*/
void refresh_picks(            struct PlotImage  *image, 
                               GC                gc, 
                               float             picks[],
                               float             missing_pick, 
                               float             zero_pick,
                               long              first_trace, 
                               long              last_trace,
                               long              x, 
                               long              y, 
                               long              width, 
                               long              height);

/*============================================================================
  Redraws points on an image
=============================================================================*/
void refresh_points(           struct PlotImage  *image, 
                               GC                gc,
                               float             xpoints[], 
                               float             ypoints[], 
                               long              npoints,
                               long              x, 
                               long              y, 
                               long              width, 
                               long              height);

/*============================================================================
  Controls drawing of movie loops
=============================================================================*/
void image_movie(              struct PlotImage  *image,
                               long              frame,
                               long              x,
                               long              y,
                               long              width,
                               long              height);

/*============================================================================
  Frees image structure resources
=============================================================================*/
void image_free (              struct PlotImage  *image, 
                               Display           *dpy );

/*============================================================================
  Frees x resources associated with an image
=============================================================================*/
void image_free_xresources (    struct PlotImage *image, 
                                Display          *dpy);

/*============================================================================
  Prepares image for plotting
=============================================================================*/
long setup_plot(                Widget           w,
                                struct PlotImage *image,
                                char             *errstr);

/*============================================================================
  Modifies an existing plot
=============================================================================*/
long modify_plot(               Widget            w, 
                                struct PlotImage  *image, 
                                char              *errstr);

/*============================================================================
  Processes a zoom
=============================================================================*/
int process_zoom(               Widget            w,
                                struct PlotImage  *image,
                                long              zoom_type,
                                long              x1,
                                long              y1,
                                long              x2,
                                long              y2,
                                long              pan,
                                char              *errstr );

/*============================================================================
  Restores a zoomed image
=============================================================================*/
int zoom_original(              struct PlotImage  *image,
                                char              *errstr);

/*============================================================================
  Zooms an image up
=============================================================================*/
int zoom_up(                    Widget w,
                                struct PlotImage *image,
                                long              x1,
                                long              y1,
                                long              x2,
                                long              y2,
                                long              pan,
                                char              *errstr);

/*============================================================================
  Zooms an image down
=============================================================================*/
int zoom_down(                  struct PlotImage *image,
                                char             *errstr);

/*============================================================================
  Sets the mouse for image value readout
=============================================================================*/
void set_xyout(                 struct PlotImage  *image,
                                Widget            xloc,
                                Widget            yloc,
                                Widget            aout,
                                enum outtype      newstat );

/*============================================================================
  Creates an image from another image
=============================================================================*/
struct PlotImage * spawn_image( struct PlotImage *sourceimage,
                                struct PlotImage *parentimage,
                                Widget            graphic,
                                long              width,
                                long              height,
                                float             tmin,
                                float             tmax,
                                float             ti,
                                float             is,
                                float             grid_x1,
                                float             grid_x2,
                                char              *errstr);

/*============================================================================
  Modifies a section of an image
=============================================================================*/
long image_sub (                Widget            w,
                                long              start_func,
                                long              end_func,
                                long              starting_sample,
                                long              ending_sample,
                                struct PlotImage  *image);

/*============================================================================
  Converts a device coordinate to a user coordinate
=============================================================================*/
void convert_vel(               int                yin,
                                long               top_border,
                                double             xval,   
                                double             *aval,
                                long               nhdrs,
                                long               samples,
                                long               num_vels,
                                float              *hd,
                                float              *vel_vals,
                                struct             PlotImage *image);

/*============================================================================
  Reads a next set of data image generation
=============================================================================*/
long image_scan(                Widget             w,
                                struct PlotImage   *image,
                                int                direction,
                                int                scan_original,
                                int                scan_screen,
                                char               *errstr);

/*============================================================================
  Sets plot parameters so that an image occupies all of the screen
=============================================================================*/
void maximize(                  Widget             w,
                                struct ImageInput  *user,
                                struct PlotImage   *image,
                                long               plot_mode);

/*============================================================================
  Creates an image under a bit map image
=============================================================================*/
long create_underlay(           struct PlotImage   *under,
                                struct PlotImage   *parent,
                                char               *errstr);

/*============================================================================
  Aligns over/under images
=============================================================================*/
void set_dest(                  struct PlotImage   *over,
                                struct PlotImage   *under);

/*============================================================================
  Decodes byte files for needed info
=============================================================================*/
long bytedefs (                 struct PlotImage   *image,
                                char               *filename,
                                float              *xloc,
                                float              *yloc,
                                long               *xloc_hdr,
                                long               *yloc_hdr,
                                long               *num_gathers);

/*============================================================================
  Decodes a byte file for velocity info
=============================================================================*/
long get_xys(                   struct PlotImage   *image);


/*============================================================================
  Annotates a grid type image
=============================================================================*/
void annotate_grid(             struct PlotImage   *image,
                                float              user_x1,
                                float              user_x2,
                                float              user_y1,
                                float              user_y2);

/*============================================================================
  Allocates an array to store velocities
=============================================================================*/
void allocate_vary(             struct PlotImage   *image);

/*============================================================================
  Creates a grid type image
=============================================================================*/
long image_grid(                Widget             w,
                                struct PlotImage   *image,
                                char               *errstr);

/*============================================================================
  Sets the size of a grid type image
=============================================================================*/
void setgrid_size(              struct PlotImage   *image);

/*============================================================================
  Returns a pixel from a header value
=============================================================================*/
int pixel_from_header(          struct PlotImage   *image,
                                float              header_val,
                                int                header_number);

/*============================================================================
  Re-annotates an image
=============================================================================*/
void re_annotate(               struct PlotImage   *image);

/*============================================================================
  Annotates the x axis of a grid plot
=============================================================================*/
void annotate_gridx(            struct PlotImage   *image);

/*============================================================================
  Reads file data for an image
=============================================================================*/
long image_io(                  struct PlotImage   *image,
                                long               AryOffset,
                                long               HdrOffset,
                                long               image_number,
                                char               *errstr);



#ifdef __cplusplus  
}
#endif

/*------------------------------------------------------------------------
               end of function prototypes                      
-----------------------------------------------------------------------*/


#endif
