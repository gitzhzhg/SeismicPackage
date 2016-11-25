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
#ifndef SL_CVM_APP_HH
#define SL_CVM_APP_HH

#include <string.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include "sl/sl_app.hh"
#include "wproc.h"
#include "model.h"
#include "sl_cvm_base.hh"

class SeisTransf;
class SeisVectLinkedList;
class VLDataUser;
class VectorListData;
class ShellMouseHelp;
class SLDialog;
class SLPullPop;
class SLForm;
class PickGuiPop;
class SeisSelectPop;
class SeisGridPop;
class SeisColorPop;
class SeisCbarPop;
class HardCopySeisPop;
class RayEditPop;
class TimeDepthPop;
class SeisPlot;
class SeisPlotUnder;
class SeisLocOut;
class Mlimits;


class CvmApp : public CvmAppBase {
  private:

  protected:

   ErsModel *_ersmod;    // Kept for IO purposes only
   SLDialog *_new_pop;
   SLDialog *_remdisp;  // Popup for remote display control
   HardCopySeisPop    *_hrdcpy; // for plots
   Mlimits  *_mlimdata; // GUI to change the model limits
   void     *_glimdata; // GUI to change the grid limits
   void     *_csysdata; // GUI to change coordinate system
   void     *_transpop; // Pointer to a CoordEditPop object 
   Widget    _openform; // Pointer to form for file input
   void     *_struc;    // Pointer to a HorizEdit object-structure
   void     *_hzedit;   // Pointer to a HorizEdit object-velocity
   void     *_celldata; // Pointer to cell popup object
   void     *_griddata; // pointer to a gridcvm structure
   RayEditPop *_raypop; // controls ray shooting
   TimeDepthPop *_tdpop;   // controls time - depth conversions
   void     *_test;
   Widget   _display;   // For the display control
   int       _num_plots;
   CvmAppBase **_plots;


   virtual void  buildMenuBar();
   void          buildPlotArea();
   virtual void  buildPopups();

   virtual void  Shutdown();

  public:


   CvmApp( int               &argc,
           char              **argv);
   CvmApp( char *display_name, XtAppContext app_context);
   ~CvmApp();
   virtual void   setup();

   virtual Boolean isTopLevel()  { return True; };

   ErsModel     *cvm_getmod(CvmApp *cvm);
   void          cvm_setmod(CvmApp *cvm, ErsModel *mod);
   void          startNewModel(char *name);
   void          startNewModel(ErsModel *model);
   void          killOldModel(int remove_model);
   int           cvmTransform(char *xlab,char *ylab,char *zlab,char *slab);
   int           cvmTransform(char *xlab,char *ylab,char *zlab);
   int           cvmSPTransform(SeisPlot *s, ErsTransform *tx, ErsTransform *ty,
                 ErsTransform *tz, ErsTransform *tzs);
   int           cvmSPTransform(ErsTransform *tx, ErsTransform *ty,
                 ErsTransform *tz, ErsTransform *tzs);
   int           numRemotePlot(){ return _num_plots; }
   CvmAppBase   *nthRemotePlot(int n)
    { if(n > _num_plots) return NULL; else return _plots[n-1];}
   void          setRemote(SLDialog *s) { _remdisp = s; };
   SLDialog     *getRemote() { return _remdisp; };
   int           connectLists();
   int           connectLists(CvmAppBase *);
   void          updateRemoteUnders();
   int           updateOneRemoteUnd(CvmAppBase *cvm_remote);
   void          cvmGuiUpdate();
   int           cvmUpdateModel();

   void    cvm_set_file(char *infile) { strcpy(_infile,infile); }
   static  void   *cvm_get_hzedit(CvmApp *);
   static  void   *cvm_get_struc(CvmApp *);
   int     cvm_bld_vlists(int p,int s,int zeit,int szeit);
   void *  get_tpop() { return _transpop; }
   void    set_tpop(void *data) { _transpop = data; }
   void *  get_cpop() { return _celldata; }
   void    set_cpop(void *data) { _celldata = data; }
   void *  get_gpop() { return _griddata; }
   void    set_gpop(void *data) { _griddata = data; }
   void    set_struc(void *data) { _struc = data; }
   void    set_hzedit(void *data) { _hzedit = data; }

   void            cvmSetVis(int vis);

// Callbacks for the menu bar system.
   static void NewCVM(void *data, long ident );
   static void OpenCVM(void *data, long ident);
   static void OpenDes(Widget W, CvmApp *, caddr_t b);

   static void MlimitsCB(void *data, long ident );
   static void MlimitsDes(Widget W, CvmApp *cvmapp, caddr_t b);
   static void GlimitsCB(void *data, long ident );
   static void GlimitsDes(Widget W, CvmApp *cvmapp, caddr_t b);
   static void TransfCB(void *data, long ident );
          void TransfDes();
   static void CoordCB(void *data, long ident );
   static void CoordDes(Widget W, CvmApp *cvmapp, caddr_t b);
   static void DisplayCB(void *data, long ident );
   static void DisplayDes(Widget W, CvmApp *cvmapp, caddr_t b);
   static void CellCB(void *data, long ident);
   static void CellDes(Widget , CvmApp *, caddr_t );
   static void StructCB(void *data, long ident );
   static void VelocityCB(void *data, long ident );
   static void GridCVM(void *data, long ident  );
   static void TestCVM(void *data, long ident  );
   static void RayCB(void *data, long ident  );
   static void TimeDepthCB(void *data, long ident  );
   static void GridDes(Widget , CvmApp * , caddr_t );


   static void HardCopy(void * data, long ident);
   static void AddDispCB(void *data, long ident );
   CvmAppBase    *addDisp(char *dname);
   static void RemDes(void *appbase,void *app);

};

#endif

