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
#ifndef USERCTL_H
#define USERCTL_H

#define MAX_LINES 500

#include "image.h"
#include "va_const.h"

#define uoff(field)   XtOffsetOf(struct ImageInput, field)

class MoviePop;

class UserCtl {

  protected:
       struct ImageInput *_user[MAX_DRAW];
       struct PlotImage  *_image[MAX_DRAW];
       MoviePop          *_mp;
       unsigned long     _user_change_flags;
       float             _maxtime[MAX_DRAW];
       float             _mintime[MAX_DRAW];
       float             _minxgrid[MAX_DRAW];
       float             _maxxgrid[MAX_DRAW];
       float             _minygrid[MAX_DRAW];
       float             _maxygrid[MAX_DRAW];
       float             _minvel[MAX_DRAW];
       float             _maxvel[MAX_DRAW];
       long              _xselect_hdr[MAX_DRAW]; // x header word for select
       long              _yselect_hdr[MAX_DRAW]; // y header word for select
       long              _select_type;           // may be set to SEM, CMP, GVS
       long              _number_func[MAX_DRAW]; // number function returned
       long              _plot_vel_type;         // type of iso plot to do
       long              _pick_vel_type;         // type of picking to overlay
       long              _gvsft;                 // gvs first trace
       long              _cmpft;                 // gvs first trace
       float             _yline[MAX_LINES];      // y line request
       float             _xline[MAX_LINES];      // x line request
       float             _time[MAX_LINES];       // time or depth slice value
       long              _line_type;             // crossline/inline/slice
  public:
      UserCtl( struct PlotImage  *iso_image,
               struct ImageInput *iso_user,
               struct PlotImage  *gvs_image,
               struct ImageInput *gvs_user,
               struct PlotImage  *sem_image,
               struct ImageInput *sem_user,
               struct PlotImage  *cmp_image,
               struct ImageInput *cmp_user,
               struct PlotImage  *vgrid_image,
               struct ImageInput *vgrid_user );

      UserCtl( struct ImageInput *iso_image,  
               struct ImageInput *gvs_image,  
               struct ImageInput *sem_image,  
               struct ImageInput *cmp_image,
               struct ImageInput *vgrid_image );

      void set( long which, long field, float  value);
      void set( long which, long field, long   value);
      void set( long which, long field, double value);
      void set( long which, long field, char  *value);
      long   getl( long which, long field);
      float  getf( long which, long field);
      double getd( long which, long field);
      char  *gets( long which, long field);
      struct GLBL *getG( long which) { return &_user[which]->G; };
      struct PlotImage *getPI( long which) { return _image[which]; };

      // first trace routines
      void setFirstTrace(long which, long value);
      long getFirstTrace(long which);

      // change flag routines
      unsigned long getChangeFlags() { return _user_change_flags; };
      unsigned long getChangeFlagsReset();
      void  setChangeFlag(int i) { _user_change_flags |= (1<<i); };

      // tmin - tmax routines
      void retTimeMinMax(float *min, float *max);
      void dispTimeMinMax(long which, Widget w);
      void rangeStrTimeMinMax(long which, char *tminstr, char *tmaxstr);
      void setTimeMinMax(long which);

      // velocity and grid min and max routines
      void retXGridMinMax( float *min, float *max);
      void retYGridMinMax( float *min, float *max);
      void setVelMinMax( long which, float min, float max);
      void retVelMinMax( Widget w, float *min, float *max, long which);

      // other parameters
      long getXSelectHdr(long which)          { return _xselect_hdr[which]; }
      long getYSelectHdr(long which)          { return _yselect_hdr[which]; }
      void setXSelectHdr(long which,long hdr) {_xselect_hdr[which]= hdr;}
      void setYSelectHdr(long which,long hdr) {_yselect_hdr[which]= hdr;}
      long getSelectType()                   { return _select_type; }
      void setSelectType(long which)         {_select_type= which;}
      long getNumFuncs(long which)           { return _number_func[which]; }
      void setNumFuncs(long which,long val)  { _number_func[which]= val;
                                           set(which, uoff(iskp), (long)0);}
      void setMoviePtr(MoviePop *mp)         {_mp = mp;}
      void changeMovies(int which);
      // vel type routines 
      long  getPlotVelType()        { return _plot_vel_type; }
      void  setPlotVelType(long vt);
      long  getPickVelType()        { return _pick_vel_type; }
      void  setPickVelType(long vt) { _pick_vel_type= vt; }
      void  setYline(int which, float yline) {_yline[which] = yline;}
      float getYline(int which)              { return _yline[which];}
      void  setXline(int which, float xline) {_xline[which] = xline;}
      float getXline(int which)              { return _xline[which];}
      void  setTslice(int which,float time)  {_time[which] = time;}
      float getTslice(int which)             { return _time[which];}
      void  setLineType(int type)            { _line_type = type;}
      long  getLineType()                    { return _line_type; }
      // general 
      void paramPrep();
};



#endif

