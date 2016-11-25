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
#ifndef VA_INPOP_H
#define VA_INPOP_H

#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <X11/Shell.h>
#include "wproc.h"
#include "image.h"
#include "file_choice.h"
#include "va_const.h"
#include "vel_data.h"
#include "sl/sl_text_box.hh"
#include "va.h"


class UserCtl;
class PopCtl;


void va_inpopcb(Widget w,
                struct CB    *udata,
                XmAnyCallbackStruct *CBdata );


typedef void inpop_pushfunc(void*, long);
typedef struct ImageInput *ImageInputPtr;


class va_inpop {
       public:
         // Constructor & Destructor
            va_inpop( Widget, VelStruct*,
                      inpop_pushfunc* =NULL, void* =NULL );
            ~va_inpop(void){};

         // methods
            Boolean get_fileset(int i) { return (Boolean)dofile[i]; };

            unsigned long get_flags(void);
            void manage(void);
            void    file_succ(Widget, struct CB*, 
                              wprocFileChoiceSuccCallbackStruct*);
            void    tog(Widget, struct CB*, XmToggleButtonCallbackStruct* );
            void    push(Widget, struct CB*, XmPushButtonCallbackStruct* );
            static  void check_headers( va_inpop *obj, long which);
            Widget  W() {return (wig[INPFRM].w ); };
            void reloadDefaults(Boolean do_method= True);
            void reloadSystemDefaults(Boolean do_method =True);
            Boolean checkNow();

       private:
            enum {MAXWIG   = 20};

            // set the file widget constants to the values from va_const.h
            // which we know are 0-3
            enum {SEMFILE  =  SEM};
            enum {CMPFILE  =  CMP};
            enum {GVSFILE  =  GVS};
            enum {VELFILE  =  ISOVEL};
            enum {REFFILE  =  VGRID};
          
            enum {SEMTOG   =  5};
            enum {CMPTOG   =  6};
            enum {GVSTOG   =  7};
            enum {VELTOG   =  8};
           
            enum {GVSPSH   =  9};
            enum {ANNOPSH  = 10};
            enum {SEMPSH   = 11};
            enum {MOVIEPSH = 12};
            enum {ISOPSH   = 13};
            enum {INPFRM   = 14};

            enum {XHEAD, YHEAD, NMCORDER, YBIN};
            enum {ERRBOX   = 15};
           
            wunion     wig[MAXWIG];
            struct CB  cb[MAXWIG];
            Cursor            watch_cur;
            HelpCtx           helpctx;
            inpop_pushfunc    *push_func;
            void        *push_data;
            char               gph_file[MAX_DRAW][200];
            Boolean            good_file[MAX_DRAW];
            long               dofile[MAX_DRAW];
            VelStruct         *_vel;
            UserCtl           *_uc;
            PopCtl            *_popctl;
            float             *_cmpxloc;
            float             *_cmpyloc;
            float             *_semxloc;
            float             *_semyloc;
            float             *_gvsxloc;
            float             *_gvsyloc;
            VdStruct          *_vd;
            SLTextBox         *_headers;
            long              _xhead;
            long              _yhead;
            long              _nmcorder;
            long              _old_xhead;
            long              _old_yhead;
            long              _old_nmcorder;
            Boolean           _reread_iso;
            Boolean           _reread_sem;
            Boolean           _reread_gvs;
            Boolean           _reread_cmp;
            float             _ybin;
            //struct ImageInput *user[MAX_DRAW];
            unsigned long doflags;
            Boolean get_defs(float *xlocs, float *ylocs, 
                             long  wconst, char *fname);
            Boolean get_glob( long  wconst, char  *fname);
            Boolean _first_time;
            Boolean _doing_file_validate;

            Boolean process_all(int wconst);

            /*
             * Stuff for REFFILE, ehs 12jun96
             */
            VdStruct *_backup;
            void file_succ_reffile(Widget, struct CB*); 
            void  backupVelFile();
            void restoreVelFile();
            void   storeRefFile();
};



#endif
