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
#ifndef SEISSELECT_H
#define SEISSELECT_H

#include "wproc.h"
#include "file_choice.h"
#include "sl/sl_form_pop.hh"
#include "sp/seis_inform.hh"
#include <time.h>

class SeisPlot;
class SeisAnnoPop;
class SeisColorPop;
class SLTogBox;
class SLPushBox;
class SLRadioBox;
class SLTextBox;
class SLScale;
class SLSmartForm;
class SLForm;
class SLpOption;
class SLpFile;
class NetEnv;



class SeisSelect : public SLFPopSep, public SeisInform {

    public:
       SeisSelect( Widget    p,
                   char      *name,
                   HelpCtx   hctx,
                   SeisPlot  *sp,
                   SeisPlot  *info_sp =NULL,
                   Boolean   allow_selector = False);
       virtual ~SeisSelect();
       virtual Widget make(Widget p);
       virtual void manage();
       virtual void postScan(SeisPlot *, SeisPlot::ScanDir ) ;
       void    setUnits();
       virtual void setFromOther(SeisPlot *othersp =NULL);
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean);
       virtual void colorOpEnabled(Boolean enable =True);
       void unmangeIS();
       void unmangeMAX();
       void unmanagePTYPE();
       void unmanageCT();
       void unmanageScale();
       void setAnnoPop(SLFormPop  *pop);
       void setColorPop(SeisColorPop *pop);
       void setUnderPop(SLFormPop *pop);
       virtual Boolean notifyComplex(SLDelay*, int);
       virtual void unitChange(SeisPlot *sp, int units);
       virtual void notCurrentInWindow(SeisPlot *sp);
       virtual void applyParams();
       void getDataFrom(int where);
       virtual SeisPlot *getSP();
       virtual void resetDataSelectionParameters( long  ntot, long  iskp,
                                                  long  ndo,  long  nskp,
                                                  long  tdec, float tmin,
                                                  float tmax              );
       Boolean netConnectOk ();

       void useSelector (int   primary_header,
                         float primary_min,
                         float primary_max,
                         float primary_inc,
                         int   secondary_header,
                         float secondary_min,
                         float secondary_max,
                         float secondary_inc,
                         int   tertiary_header,
                         float tertiary_min,
                         float tertiary_max,
                         float tertiary_inc,
                         long  traces_found,
                         long  frames_found,
                         long  traces_in_last_frame,
                         long  secondary_search,
                         long  tertiary_search);

       void useSelector (class NDoTraceSelection *select,
                         class NDoTraceFind *results);

       void turnOffSelector();
       
       virtual void getTracePattern(long *nplt, long *iskp, long *ndo, long *nskp,
                            long *frames, long *domovie);
       virtual void setTracePattern(long nplt, long iskp, long ndo, long nskp,
                            long frames, long domovie);
       virtual void setNumTracesToPlot(long nplt);
       virtual void setNumMovieFrames(long num_frames);
       virtual void setMovie (Boolean domovie);

       enum { NTOT, ISKP, NDO, NSKP, TMIN, TMAX, TDEC, 
             TI, IS, CT, RP, RD, INVERT, MAX_QUEST,
             ANNO, MAXIM, VD, UNDERLAY, ADDSP, DELSP, INCSP, 
             NEGATIVE_FILL, LOCAL, CRAY_SP, DOSELECTOR }; 


    protected:
       void filein(long ident, char *oldvar, char *newvar);
       void closeJSFileIfChanged ();
       Boolean fileModified (time_t time_stamp);
       virtual void    DoAction();
       virtual void    UndoInput();
       Boolean selectorsMade ();
       Boolean updateSelectors ();
       virtual Boolean ValidInput();
       void pushAction(long ident);
       void addNewSeisPlot();
       void incrementSeisPlot();
       void setToExtAmp(Boolean set);
       void checkTracePattern();
       virtual Boolean checkForPatternChange();
       Boolean netConnectGood();
       void setNetEnvPtr(NetEnv **netenv);


       Boolean      _set_from_other_class;
       char         _bytfile[300];
       Widget       _dislab;
       Widget       _sellab;
       SLpFile      *_infile;
       Widget       _information;
       Widget       _bottom_tmp;
       SLTogBox     *_selector_tog;
       SLPushBox    *_selector_button;
       SLTogBox     *_psettings;
       SLRadioBox   *_ptype;
       SLSmartForm  *_norm_form;
       SLRadioBox   *_norm_type;
       SLTextBox    *_ext_amp;
       SLTextBox    *_data_sel1;
       SLTextBox    *_data_sel2;
       SLTextBox    *_traceparams;
       SLPushBox    *_but;
       SLPushBox    *_extra_sp;
       SLpOption    *_host_ops;
       SLScale      *_which_sp;
       SeisAnnoPop  *_annopop;
       SeisPlot     *_first_sp, *_sp, *_info_sp;
       SLFormPop    *_anno_pop;
       SeisColorPop *_color_pop;
       SLFormPop    *_under_pop;
       SLForm       *_mform;
       AltFileAccess _alt_access;
       NetEnv       *_netenv;
       class TraceSelectorPop *_selector_pop;
       class TraceSelectorNDoPop *_selector_ndo_pop;
       long _doselector;
       long _nplt;
       long _iskp;
       long _ndo;
       long _nskp;
       long _previous_nplt;
       long _previous_iskp;
       long _previous_ndo;
       long _previous_nskp;
       Boolean _previous_initialized;
       float _tmin;
       float _tmax;
       long _tdec;
       float _external_amp;
       Boolean _first_time;
       Boolean _docolor;
       Boolean _allow_selector;
       float _ti;
       float _is;
       float _ct;
       long _rp;
       long _rd;
       long _invert;
       long _frames, _sav_frames, _skip_frames;
       long _domovie;
       char *_infile_failstr;
       Boolean _infile_valid;

    private:
       static void fileCallback(void *data, long ident, char *oldvar,
         char *newvar);
       static void selectorChanged(void *data, long ident, Boolean set);
       float _wiggle_ti_max;
       float _color_ti_max;
       time_t _time_stamp;
       Boolean checkTracesPerInch();

    
};




#endif
