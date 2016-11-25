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
//========================= COPYRIGHT NOTICE ================================
//====  CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.      ========
//====   PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK       ========
//======================== COPYRIGHT NOTICE =================================


//===========================================================================
//========== Class that applies nmc to a trace file and outputs   ===========
//========== a new nmc corrected trace file                       ===========
//========== Michael L. Sherrill 11/97                            ===========
//===========================================================================

#ifndef VA_NMC_OUTPUT_H
#define VA_NMC_OUTPUT_H

#include "sl/sl_form_pop.hh"
#include "tfio.h"


enum{MAX_LOCS = 5000}; //Change this when VF class is more dynamic

class SLTextBox;
class SLFilePairPlus;
class VaCmpPlot;
class VaIsoPlot;
class VfManager;
class VfMoveout;
class SLTogBox;


class VaNmcOutput :  public SLFPopSep {

  private:
       SLFilePairPlus      *_file_pair;
       SLTextBox           *_text_box;
       struct Cntrl        _Cl;
       struct GLBL         _G; 
       struct GLBL         _Gout;
       struct GLBL         _glbls;
       VfManager           *_vf_manager;
       VfMoveout           *_vf_moveout;
       VaCmpPlot           *_cmp_plot;
       VaIsoPlot           *_iso_plot;
       float               _tolerance;
       int                 _first_time;
       char                *_input_filename;
       char                *_output_filename;
       char                _errmsg[256];
       float               *_missing_locations;
       char                *_missing_location_string;
       float               *_floats;
       unsigned char       *_bytes;
       float               *_tasf;
       float               *_headers;
       char                *_char_headers;
       int                 _num_per_group;
       float               *_times;
       float               *_velocities;
       float               *_vfd_xlocs;
       float               *_vfd_ylocs;
       long                _glbls_curtr;
       SLTogBox            *_irregular_cmp;
       float               *_xlocations;
       float               *_ylocations;

  protected:
       virtual void    DoAction();
       virtual void    UndoInput();

  public:
       VaNmcOutput(Widget     p,
                   char       *name,
                   HelpCtx    hctx,
                   VfManager  *vf_manager,
                   VaCmpPlot  *cmp_plot,
                   VaIsoPlot  *iso_plot );
       ~VaNmcOutput();
       virtual Widget make(Widget p);
       virtual void manage();
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean do_method);
       static void validateTrap(void *user_data,
                              const char *filename1, const char *filename2,
                              long *valid1, long *valid2,
                              char *info1, char *info2,
                              long *same_datasets);
       int applyNMC();
       GLBL getglbl(int *nhead,char *name, int *ok);
       Boolean getVelocityLocations();
       int getHeaders(int trace, int open_file);
       int getData(int trace, int open_file);
       int writeGlbl(GLBL glbl,char *velfname);
       void cleanUp(GLBL *glbl = NULL);
       int reallocateArrays(int num_traces);
};

#endif
