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

//************************ COPYRIGHT NOTICE ****************************
//      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
//       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
//************************ COPYRIGHT NOTICE ****************************



//************************************************************************
//***    Menu to select two files for generating a difference plot.    ***
//***    Author:Michael L. Sherrill 12/2001                            ***
//************************************************************************



#ifndef _SEIS_DIFFERENCE_POP_HH
#define _SEIS_DIFFERENCE_POP_HH

#include "sl/sl_dialog.hh"
#include "sl/sl_databox.hh"
#include "sl/sl_arrow_scale.hh"

class SeisMultiPlotControl;
class SeisPlot;

class SeisDifferencePop : public SLDialog
{
  public :
    SeisDifferencePop( Widget                     w,
                       char                       *name,
                       HelpCtx                    hctx,
                       SLArrowScale               *scale,
                       class SeisMultiPlotControl *control);

    virtual ~SeisDifferencePop();

    friend class SeisDifferenceTable;

    friend class SeisDifferencePlot;

    class SLTextBox            *_trace_size_box;
    class SLTextBox            *_ext_amp;
    class SLRadioBox           *_plot_mode_radios;
    class SLRadioBox           *_norm_type;
    virtual Widget make(Widget p);
    virtual long getNumFrames();
    virtual char *getFilename(long i); 
    virtual void clearSelections();
    virtual long frameSequenceNumber(int i);
    virtual Boolean notifyComplex(SLDelay *obj, int ident);
    virtual Boolean notify(SLPrim *gui);
    virtual void setFirstSeisPlot(int index);
    virtual void setSecondSeisPlot(int index);
    

    enum{TI, IS, CT, NEGATIVE_FILL = 77};


  protected:
    virtual int plot();  
    virtual void userRemovedPlot(class SeisDifferencePlot *sdp);    

  private:
    class SeisDifferenceTable  *_table; 
    class SeisMultiPlotControl *_control;
    SLArrowScale               *_arrow_scale;
    SLpPush                    *_clear_button;
    SLpPush                    *_ok_button;
    SLpPush                    *_apply_button;
    SLpPush                    *_cancel_button;
    SLpPush                    *_help_button;
    float                      _tmin;
    float                      _tmax;
    long                       _nplt;
    long                       _iskp;
    long                       _ndo;
    long                       _nskp;
    float                      _allowed_tmin;
    float                      _allowed_tmax;
    long                       _allowed_nplt;
    float                      _traces_per_inch;
    float                      _inches_per_second;
    float                      _ct;
    int                        _plot_type;
    float                      _external_amp;
    SeisPlot                   *_first_seisplot;
    SeisPlot                   *_second_seisplot;
    class SeisDifferencePlot   *_plot;
};


class SeisDifferenceTable  :  public SLDatabox
{

  public:
    SeisDifferenceTable( SLDelay           *slparent,
                         char              *name,
                         SeisDifferencePop *pop);
    virtual  ~SeisDifferenceTable();
    int      _frames_set[SLArrowScale::MAX_FRAMES];
    virtual  void clearSelections();
    virtual  long getNumFrames();
    virtual  int getNumSequencedFrames();
    virtual  char *getFilename(long i);
    virtual  long frameSequenceNumber(int i);
    int      _sequence;
    SeisDifferencePop *_pop;


  private:
    void              makeHelper();
    
};

#endif
