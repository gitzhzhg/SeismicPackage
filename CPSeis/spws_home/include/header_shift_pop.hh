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
#include "va.h"
#include "sl/sl_form_pop.hh"
#include "sl/sl_file_pair_plus.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_tog_box.hh"


enum{ SHIFT_HEADER,   MATCH_HEADER,   TOLERANCE,
      FLOAT_TO_FINAL, SHIFT_ONLY};

class HeaderShiftPop :  public SLFPopSep {

  private:
       SLFilePairPlus      *_file_pair;
       VelStruct           *_vel;
       int                 _shift_header;
       int                 _match_header;
       float               _tolerance;
       SLTextBox           *_text_box;
       SLRadioBox          *_radio;
       SLTogBox            *_shift_tog;
       int                 _first_time;
       char                *_trace_filename;
       char                *_new_vel_filename;
       float               *_trace_xlocs;
       float               *_trace_ylocs;
       float               *_shift_vals;
       float               *_new_times;
       float               *_new_vels;
       float               *_temp_times;
       float               *_temp_vels;
       float               *_missing_locations;
       char                *_missing_location_string;
       long                 _num_locations;

  protected:
       virtual void    DoAction();
       virtual void    UndoInput();
       static void     losingFocusAction(void *data, long ident);

  public:
       HeaderShiftPop( Widget            p,
                       char              *name,
                       HelpCtx           hctx,
                       VelStruct         *vel);
       ~HeaderShiftPop();
       virtual Widget make(Widget p);
       virtual void manage();
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean do_method);
       static void validateTrap(void *user_data,
                              const char *filename1, const char *filename2,
                              long *valid1, long *valid2,
                              char *info1, char *info2,
                              long *same_datasets);
       int getHeaders();
       int computeNewVelocities();
       void checkNewPicks(float *newtimes, float *newvels);
};
