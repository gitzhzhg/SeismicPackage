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
#include "wproc.h"
#include "sl/sl_form_pop.hh"
#include "sl/psuedo_widget.hh"
#include "sl/sl_form.hh"
#include "sl/sl_tog_box.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_prim.hh"
#include "sl/sl_scale.hh"
#include "file_choice.h"
#include "image.h"
#include "cbar.hh"
#include "vel_data.h"
#include "va.h"

class ColSelPop;
class UserCtl;


//-------------------------------------
//----------------------------- Cscale
//-------------------------------------
class Cscale :  public SLScaleDrag {
               protected:
                 virtual void    ScaleAction(int);
                 class ColSelPop *_csel;
                 long            _last_val;
               public :
                 Cscale(  PsuedoWidget  *pw,
                          char          *name,
                          HelpCtx       hctx,
                          class ColSelPop *csel,
                          int           *valptr = NULL) :
                 SLScaleDrag(pw, name, hctx, valptr),  
                 _csel(csel), _last_val(-1) {};
};


//------------------------------------------
//----------------------------- PlottingPop
//------------------------------------------
class PlottingPop :  public SLFPopSep {

   protected:
       Boolean     _plot_on_doaciton;
       Boolean     _new_file;
       Boolean     _use_file_defaults;
       Boolean     _new_appdefaults;
       Boolean     _user_visited;
   public:
       PlottingPop( Widget            p,
                    char              *name,
                    HelpCtx           hctx,
                    Boolean           small_on_dpi    =True)
                   : SLFPopSep(p,name,FP_DOALL,hctx,small_on_dpi,False),
                         _plot_on_doaciton(False), _new_file(True), 
                         _use_file_defaults(False), _new_appdefaults(True),
                         _user_visited(False) {};

       virtual void plotPrep() {};
       virtual Widget make(Widget p) { SLFPopSep::make(p); return topWidget();};
       void willPlot(Boolean plot) {_plot_on_doaciton= plot; };
 
      /*
       * the manage function should set _new_file to false
       */
       virtual void newFile()     {_new_file= True;};
       virtual void newApp()      {_new_appdefaults= True;};
       virtual void useFileDefs(Boolean v) {_use_file_defaults= v;};
       virtual void loadValues()  { ValidInput();DoAction();};
       virtual Boolean userVisited(){return _user_visited;}

};

//----------------------------------------
//----------------------------- GvsCmpPop
//----------------------------------------
class GvsCmpPop :  public PlottingPop {

   private:

   protected:
       virtual void    DoAction();
       virtual Boolean ValidInput();
       static void setTP(void *data, long which);
       static void gvsNormAction(void *data, long which);
       static void cmpNormAction(void *data, long which);
       void set_to_file_defs();
       SLTextBox  *_p_params;
       SLTextBox  *_GVSdisp;
       SLTextBox  *_CMPdisp;
       SLRadioBox *_GVSptype;
       SLRadioBox *_CMPptype;
       SLTogBox   *_GVSop;
       SLTogBox   *_CMPop;
       SLRadioBox *_gvs_norm_type;
       SLRadioBox *_cmp_norm_type;
       SLForm     *_radform, *_togform, *_textform, *_normform;
       UserCtl    *_uc;
       Boolean _first_time;
       Widget     _rangelab;

       // --- toggles
       long    _gvsrp,    _cmprp;
       long    _gvsnorm,  _cmpnorm;
       long    _gvs_rtol, _cmp_rtol;

       // --- display texts
       long    _gvsft,    _cmpft;
       long    _gvsnskp,  _cmpnskp;
       long    _gvstp,    _cmptp;

       // --- plot texts
       float   _gvsti, _cmpti;
       float   _is;
       float   _ct;
       float   _tmin,  _tmax;
       float   _xmin,  _xmax;

   public:
       GvsCmpPop( Widget   p,
                  char     *name,
                  HelpCtx  hctx,
                  UserCtl  *uc);
       virtual void manage();
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean do_method =True);
       virtual Widget make(Widget p);
};

//----------------------------------------
//----------------------------- ColSelPop
//----------------------------------------
class ColSelPop :  public PlottingPop {

   private:

   protected:
       Widget        _cfilew;
       Widget        _coloraframe;
       SLScale      *_cnum;
       SLRadioBox   *_c_choice;
       Cbar         *_cbar;
       Widget        _clab;
       Boolean       _new_color;
       Boolean       _col_warning;
       ColorInfo     _col;
       ColorInfoPtr  _col_request;
        // data
       char        _cfile[200];
       static void cfilecb( Widget, XtPointer, wprocFileChoiceSuccCallbackPtr);
       static void doCtog( ColSelPop*, long); 
       virtual void    DoAction();
       virtual Boolean ValidInput();
       void unmade_filesetup();
   public:
       friend Cscale;
       ColSelPop( Widget               p,
                  char                 *name,
                  HelpCtx              hctx, 
                  struct PlotImage     *image,
                  struct    COLOR_INFO *col_plot,
                  struct    COLOR_INFO *col_gs);
       enum{CBAR = 1, USEDEF = 2};
       virtual void plotPrep();
       virtual Widget make(Widget p);
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean do_method =True);

};

//--------------------------------------
//----------------------------- IsoPop
//--------------------------------------
class IsoPop :  public ColSelPop {

   private:

   protected:
       virtual void    DoAction();
       virtual Boolean ValidInput();
       void set_to_file_defs();
       float   _pwidth;
       float   _is;
       float   _xmin;
       float   _xmax;
       float   _ymin;
       float   _ymax;
       float   _tmin;
       float   _tmax;
       float   _vmin;
       float   _vmax;
       float   _ybin;
       float   _xbin;
       float   _time;
       float   _orig_vmin;
       float   _orig_vmax;
       float   _oldtval;
       float   _olddval;
       float   _oldy;
       float   _oldx;
       float   _oldt;
       float   _user_vmin;
       float   _user_vmax;
       long    _grad_hor;
       long    _grad_ver;
       Boolean _first_time;
       Widget     _plotlab;
       Widget     _rangelab;
       Widget     _vellab;
       Widget     _linelab;
       SLTextBox  *_params;
       SLRadioBox *_ptypebox;
       SLTogBox   *_grade;
       SLRadioBox *_linebox;
       SLTextBox  *_binbox;
       UserCtl    *_uc;
       VelStruct  *_vel;
       virtual void UndoInput();
       static  void ChoiceAction( void *data, long which );
       static  void LineAction  ( void *data, long which );
       static  void BinFocusAction( void *data, long which );
       static  void BinLosingFocusAction  ( void *data, long which );
       static  void ParamFocusAction( void *data, long which );
       static  void ParamLosingFocusAction  ( void *data, long which );
   public:
       IsoPop( Widget               p,
               char                 *name,
               HelpCtx              hctx, 
               UserCtl              *uc,
               VelStruct            *vel,
               struct    COLOR_INFO *col_cust,
               struct    COLOR_INFO *col_gs);
       virtual void manage();
       virtual Widget make(Widget p);
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean do_method =True);
       long gradeVertical(){return _grad_ver;}
};
//--------------------------------------
//----------------------------- SemPop
//--------------------------------------
class SemPop :  public ColSelPop {

   private:

   protected:
       virtual void    DoAction();
       virtual Boolean ValidInput();
       void set_to_file_defs();
       float _pwidth;
       float _is;
       float _vmin;
       float _vmax;
       float _pdmin;
       float _pdmax;
       float _tmin;
       float _tmax;
       long  _connum;
       long  _grad_vert;
       long  _grad_hor;
       long  _plot_con;
       float _startvel;
       float _bot_time;
       Boolean _first_time;
       Widget _plotlab;
       Widget _rangelab;
       Widget _vellab;
       SLTogBox   *_grade;
       SLTextBox  *_params1;
       SLTextBox  *_params2;
       UserCtl    *_uc;
       VdStruct   *_vd;
   public:
       SemPop( Widget               p,
               char                 *name,
               HelpCtx              hctx, 
               UserCtl              *uc,
               VdStruct             *vd,
               struct    COLOR_INFO *col_cust,
               struct    COLOR_INFO *col_gs);
       virtual void manage();
       virtual Widget make(Widget p);
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean do_method =True);
};


//--------------------------------------
//----------------------------- Velocity Grid
//--------------------------------------

class GridPop : public PlottingPop {
 private:

 protected:
         virtual void    DoAction();
         virtual Boolean ValidInput();
         void            set_to_file_defs();
         float           _pwidth;
         float           _is;
         float           _xmin;
         float           _xmax;
         float           _ymin;
         float           _ymax;
         Boolean         _first_time;
         Widget          _plotlab;
         SLTextBox       *_params;
         UserCtl         *_uc;
         VelStruct       *_vel;
 public:
         GridPop(Widget               p,
                 char                 *name,
                 HelpCtx              hctx,
                 UserCtl              *uc,
                 VelStruct            *vel);
         virtual void manage();
         virtual Widget make(Widget p);
         virtual void reloadDefaults(Boolean do_method= True);
         virtual void reloadSystemDefaults(Boolean do_method =True);
};



//--------------------------------------
//----------------------------- Movie
//--------------------------------------
class MovieTextBox;

class MoviePop : public PlottingPop {
       private:

       protected:
          Widget               _mvlabl1;
          Widget               _gtotalp;
          Widget               _itotalp;
          SLTogBox             *_mvtog1;
          SLTogBox             *_mvtog2;
          SLTogBox             *_mvtog3;
          SLTextBox            *_gmovietxt1;
          SLTextBox            *_imovietxt1;
          long                 _do_gvsmovie;
          long                 _do_isomovie;
          long                 _do_semmovie;
          long                 _do_cmpmovie;
          long                 _gfirstpnl;
          long                 _gskippnl;
          long                 _gnumpnl;
          float                _ifirstpnl;
          float                _iskippnl;
          float                _inumpnl;
          UserCtl              *_uc;
          virtual void         DoAction();
          virtual Boolean      ValidInput();
          virtual void         UndoInput();
          struct velan_window  *_vel;
       public:
           friend MovieTextBox;
           virtual void manage();
           virtual Widget make(Widget p);
           virtual void reloadDefaults(Boolean do_method= True);
           virtual void reloadSystemDefaults(Boolean do_method =True);
           virtual void resetMovies(int which);
           MoviePop( Widget              parent,
                     char                *name,
                     HelpCtx             hctx,
                     UserCtl             *_uc,
                     struct velan_window *vel);
};

//--------------------------------------
//--------------------- Movie Text Box
//--------------------------------------
class MovieTextBox : public SLTextBox {
  private:

  protected:
          virtual void TextAction( long ident );
          class MoviePop  *_mp;
  public:
          MovieTextBox( PsuedoWidget *pw,
                        char *name,
                        HelpCtx hctx,
                        SLTextAry text,
                        unsigned int cnt,
                        MoviePop  *mp) :
                  SLTextBox( pw, name, hctx, text, cnt, True), _mp(mp) {};
};






