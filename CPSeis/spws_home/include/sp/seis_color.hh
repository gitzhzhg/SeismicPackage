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
#ifndef SEISCOLOR_H
#define SEISCOLOR_H

#include "wproc.h"
#include "sp/seis_plot.hh"

class ColorInform;

class SeisColor {
  protected:
     float     _rgb[1024];
     float     _rgb_of_plot[1024];
     SeisPlot  *_sp;
     ColorInfo _col;
     int       _predef;
     int       _which_cols;
     long      _numcolors;
     Boolean   _colors_from_predef; // the color are from predefined cbar
     Boolean   _i_allocated_colors; // the color were alloc. by this class
     Boolean   _try_to_allocate;    // if color change try to alloc new col.
     Boolean   _first_time;
     Boolean   _always_update_from_sp; // get cbar info and predef # from sp
     long      _new_color;
     long      _inten;
     long      _compres;
     Boolean   _for_seisplot;  // create to SeisPlot internal use
     ColorInform *_cinform;
     Display     *_dpy;
     int         _role;

     class ColorInfoSegment *_col_segment;

     void colorChange(long num_allocated, long which_set);

  public:
     enum _cfile_status {CbarSucc, CbarTooMany, CbarInvalid};

     // ----------------------- CONSTRUCTORS -------------------------
     /*
      * Should be used only from SeisPlot
      */
     SeisColor(SeisPlot *sp, ColorInfoPtr col, 
               int which_cols =SeisPlot::COLOR);
     /*
      * Should be used only from other places
      */
     SeisColor(SeisPlot *sp, 
               Boolean   separate_colors =False,
               Boolean   always_update_from_sp= False,
               int which_cols =SeisPlot::COLOR);


     virtual ~SeisColor();
     virtual  int  readColorFile(char *fname);
     virtual  void setPredef(int predef);
     void setSeisPlot(SeisPlot *sp);
     long     predef() { return _predef;}
     void drawCbarOn (Widget w, float cell_width_factor=1.0);
     void drawReadOnlyCbarOn (Widget w, float cell_width_factor=1.0);
     void drawDynamicCbarOn (Widget w);
     void loadToSeisPlot(SeisPlot *sp =NULL);
     void prepareColors(SeisPlot *sp =NULL, Boolean update_sp =True);
     void setColor(ColorInfoPtr col);
     void setReadOnlyColor(ColorInfoPtr col);
     void setDynamicColor(ColorInfoPtr col);
     void allocColor();
     void allocReadOnlyColor();
     void allocDynamicColor();
     float minRGBAmp() {return _rgb[3];}//array elements are r,g,b,amplitude
     float maxRGBAmp() {return _rgb[(_numcolors * 4) - 1];}//see previous line

     void setIntensity(long i) { _inten=i; prepareColors(NULL,False); }
     void setCompression(long c) { _compres=c; prepareColors(NULL,False); }
     long intensity()   { return _inten;}
     long compression() { return _compres;}
     long numcolors() { return _numcolors; }
     long numplanes() { return _col.numplanes; }
     void updateAmps(SeisPlot *sp = NULL);
     void transferColors(SeisPlot *sp = NULL);
     void updateAmpsFromSP(SeisPlot *sp);
     void updatePredefInfoFromSP(SeisPlot *sp);
     void addSP(SeisPlot *sp); 
     void removeSP(SeisPlot *sp); 

     int getMaxNumColors(int *num_planes =NULL);
     int getNumColors();
     void setNumColors(int n);
     void getAColor(int idx, float *r, float *g, float *b, float *amp);
     void setAColor(int idx, float r, float g, float b, float amp);
     int getXlibPixel(int index) const { return _col.pix[index]; }

     float *rgbOfPlot();

     virtual void colorInfoChangedImmediately (ColorInfo *col);

     friend class ColorInform;
};

#endif
