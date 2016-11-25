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
//========== Header shift class. Applies shifts from a user       ===========
//========== selected trc file to the velocity file and writes   ===========
//========== the new shifted velocity file.                       ===========
//========== Michael L. Sherrill 11/97                            ===========
//===========================================================================

#ifndef VA_HEADER_SHIFT_H
#define VA_HEADER_SHIFT_H

#include "sl/sl_form_pop.hh"
#include "sl/sl_file_choice.hh"
#include "sl/slp_file.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_tog_box.hh"
#include "oprim/file_base.hh"

class VfManager;
class VfDataset;
class VaCmpPlot;
class VfFileBase;

class VaReadShiftHeaders : public FileBase {

 public:
      VaReadShiftHeaders(char *description);
      virtual ~VaReadShiftHeaders();
      virtual Result virtualRead(const char *filename, char *errmsg);
      virtual Validity virtualValidate(const char *filename, char *info);
      virtual Prepare virtualPrepareRead (const char* , char* );

 protected:
      int                           _first_time; 

 private: 

};







class VaHeaderShift :  public SLFPopSep {

  private:
       VaReadShiftHeaders  *_trc_filebase;
       SLFileChoice        *_trc_file_choice;
       VfFileBase          *_vel_filebase;
       SLFileChoice        *_vel_file_choice;
       VfManager           *_vf_manager;
       VfManager           *_shifted_manager;
       VfDataset           *_shifted_dataset;
       VaCmpPlot           *_cmp_plot;
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
       float               *_vfd_xlocs;
       float               *_vfd_ylocs;
       float               *_missing_locations;
       char                *_missing_location_string;
       long                 _num_locations;

  protected:
       virtual void    DoAction();
       virtual void    UndoInput();
       static void     losingFocusAction(void *data, long ident);

  public:
       VaHeaderShift( Widget            p,
                      char              *name,
                      HelpCtx           hctx,
                      VfManager         *_vf_manager,
                      VaCmpPlot         *_cmp_plot);
       ~VaHeaderShift();
       enum{SHIFT_HEADER, MATCH_HEADER, TOLERANCE, FLOAT_TO_FINAL, SHIFT_ONLY};
       enum{GRPHDR = 2, READ10 = 10, LAV = 24};
       virtual Widget make(Widget p);
       virtual void manage();
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean do_method);
       virtual Boolean notifyComplex(SLDelay*, int);
       static void validateTrap(void *user_data,
                              const char *filename1, const char *filename2,
                              long *valid1, long *valid2,
                              char *info1, char *info2,
                              long *same_datasets);
       int getHeaders();
       int computeNewVelocities();
       void checkNewPicks(long ifun, float *newtimes, float *newvels);
};
#endif
