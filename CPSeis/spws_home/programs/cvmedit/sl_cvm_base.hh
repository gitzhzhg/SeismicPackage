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
#ifndef SL_CVM_APP_BASE_HH
#define SL_CVM_APP_BASE_HH

#include <string.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include "sl/sl_app.hh"
#include "sp/seis_plot.hh"
#include "mlimits.h"
#include "glimits.h"
#include "wproc.h"

class SeisTransf;
class SeisVectLinkedList;
class VectorLinkedList;
class VLDataUser;
class VectorListData;
class CoordData;
class gridData;
class ShellMouseHelp;
class SLPullPop;
class SLForm;
class PickGuiPop;
class SeisSelectPop;
class SeisGridPop;
class SeisColorPop;
class SeisCbarPop;
class SeisPlotUnder;
class SeisLocOut;
class SeisTransf;
class ModelDesc;
class SeisLabel;

typedef struct _appres
  { char    *graph_font;   /* font to use in graph */
    Boolean private_cmap;  /* use a private color map */
    long    num_col_colors;/* num color colors */
    char    *help_file;    /* name of help file */
    Boolean frame_buff;    /* server has frame buff limit*/
    Boolean wigonly;       /* only do wiggle traces */
    Boolean showhelp;      /* show help for options */
    Boolean dobacking;     /* do backing store */
  } cvm_app_res;


class CvmAppBase : public SLApp {
  private:

  protected:

   char          _infile[96];
   char          _help_file[96];
   void         *_help_data;  // non-volatile pointer for help
   static cvm_app_res  _resources;
   Boolean       _cando_colors;
   Boolean       _cando_underlay;
   long          _total_to_alloc;

   ModelDesc          *_moddesc;
   int                 _owns_model;
   VLDataUser         *_data_user_pr;
   VLDataUser         *_data_user_mats;
 
   SeisSelectPop      *_select;
   SeisGridPop        *_sgpop;
   SeisColorPop       *_color_pop;
   SeisCbarPop        *_cb_pop;
   PickGuiPop         *_pickdata;
   SLPullPop          *_filepd;
   ShellMouseHelp     *_shmouse;
   SLForm             *_main_form;
   SeisPlot           *_sp;
   SeisPlotUnder      *_spund;    // for grid models
   SeisLocOut         *_slout;    // for cursor readout
   SeisLabel          *_label;   // for labels

   virtual void  buildMenuBar();
   virtual void  buildPlotArea();
   virtual void  buildPopups();
   void          doColor();
   void          (*_notify_quit)(void *, void *);
   void          *_notify_data;

   static void SaveCVM(void *data, long ident );
   static void SaveDes(Widget W, CvmAppBase *, caddr_t b);
   static void SelectSeis(void *data, long ident );
   static void KillCVM(void *, long ident);
   static void ZoomCB(void *data, long ident );
   static void ColorCB(void *data, long ident );
   static void InitPikCB(void *data, long ident );
   static void HelpCVM(void *data, long ident );
   virtual void   Shutdown();
   void   InitSeisPlot();


  public:


   CvmAppBase( int               &argc,
               char              **argv);
   CvmAppBase( Display *dpy, char *appname, char *classname);
   CvmAppBase( char *display_name, XtAppContext app_context);
   ~CvmAppBase();
   virtual void  setup();
   int OwnsModel() { return _owns_model; }
   void setOwnsModel(int owns) { _owns_model= owns; }

   virtual Boolean isTopLevel()  { return True; };

   void            set_notify(void (*notify)(void *,void *), void *data)
                    { _notify_quit = notify; _notify_data = data; }
   int             PickingActive();
   void            StopPicking();
   void            cvmSetSeisTransf(SeisTransf *t);
   SeisTransf     *cvmGetSeisTransf() { return (SeisTransf *) _sp->transform();}
   void    cvm_set_file(char *infile) { strcpy(_infile,infile); }
   void    set_user_win(float l,float r,float b,float t);
   static  void   *cvm_get_seisplot(CvmAppBase *);
   static  void   *cvm_get_seisplotu(CvmAppBase *);
   static  void   *cvm_get_seispop(CvmAppBase *);
   static  void   *cvm_get_seisgpop(CvmAppBase *);
   SeisPlot       *getSeisPlot() { return _sp; }
   SeisPlotUnder  *getSeisPlotUnder() { return _spund; }

   static  void   *cvm_get_vlpr(CvmAppBase *);
   static  void   *cvm_get_vlmats(CvmAppBase *);
   static  void   *cvm_get_vlcells(CvmAppBase *);
   static  void   *cvm_get_vlclabs(CvmAppBase *);
   static  void   *cvm_get_vlprd(CvmAppBase *);
   static  void   *cvm_get_vlmatsd(CvmAppBase *);

   ModelDesc      *getModelDesc() { return _moddesc; }
   void            setModelDesc(ModelDesc *M);
   void            setModelDescPntr(ModelDesc *M) {_moddesc = M; }
   ErsTransforms  *cvmGetTransforms();
   void            cvm_gettrans(ErsTransform **tx, ErsTransform **ty,
                    ErsTransform **tz);
   ModLimits      *cvmGetMLimits();
   GridLimits     *cvmGetGLimits();
   void            cvmSetCDdata(CoordData *data);
   CoordData      *cvmGetCDdata();
   gridData       *cvmGetGridData();
   void            cvmSetGridData(gridData *gd);
   void            cvm_set_vldata(int list, VectorListData *data);
   VectorListData *cvm_get_vldata(int list);
   VectorLinkedList *cvm_get_vll(int list);
   void            cvm_set_datauser(int list, VLDataUser *data);
   VLDataUser     *cvm_get_datauser(int list);

   enum VectorListDataType
        {
         Structure,
         Materials,
         Boundarys,
         Cpointers
        };


};

#endif


