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

//-------------------------- msepita_application.cc -------------------------//
//-------------------------- msepita_application.cc -------------------------//
//-------------------------- msepita_application.cc -------------------------//

//         implementation file for the MsepitaApplication class
//                  derived from the SLApp class
//                   subdirectory ~spws/msepita
//                   subdirectory ~tom/msepita


#include "msepita_application.hh"
#include "sl/sl_pull_pop.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include "sl/slp_toggle.hh"
#include "sl/container_list.hh"

#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "stat/static_inform.hh"
#include "stat/static_file_base.hh"

#include "statgui/regulate_statgui.hh"
#include "statgui/statgui_watch.hh"
#include "statgui/statgui_quit.hh"
#include "statgui/statgui_status.hh"
#include "statgui/statpop_details.hh"
#include "statgui/statpop_list.hh"
#include "statgui/statpop_nils2.hh"
#include "statgui/statpop_transform.hh"
#include "statgui/statpop_resample.hh"
#include "statgui/statpop_merge.hh"
#include "statgui/statpop_splice.hh"
#include "statgui/statpop_edit.hh"
#include "statgui/statpop_nils.hh"
#include "statgui/statpop_history.hh"
#include "statgui/statpop_values.hh"
#include "statgui/statpop_compare.hh"
#include "statgui/statpop_several.hh"
#include "statgui/statpop_read.hh"
#include "statgui/statpop_save.hh"


enum { APPHELP = 77, QUIT };


//--------------------- custom resources ---------------------//
//--------------------- custom resources ---------------------//
//--------------------- custom resources ---------------------//

   // XtResource resources[] must be inside a member function
   // since it references member variable _helpfile.

#define III XtRInt   , sizeof(int   )
#define PPP XtRPixel , sizeof(Pixel )
#define SSS XtRString, sizeof(String)
#define JJ  XtRImmediate
#define SS  XtRString
#define RRR(n,i,p,j,c) \
   { n, "MsepResources", i, XtOffsetOf(MsepitaApplication,p), j, c },

void MsepitaApplication::customResources()
{
  static XtResource resources[]= {
        RRR("helpFile", SSS, _helpfile, SS,
                         (void*)"/usr/lib/X11/app-defaults/Msep_help")
  };

  /********  (equivalent to the above)
  static XtResource resources[]= {
        { "helpFile", "MsepResources", XtRString, sizeof(char *),
          XtOffsetOf(MsepitaApplication,_helpfile), XtRString,
              "/usr/lib/X11/app-defaults/Msep_help" },
  };
  ********/

  getResources(this, resources, XtNumber(resources));
}



//------------------ command line options ---------------------//
//------------------ command line options ---------------------//
//------------------ command line options ---------------------//

static XrmOptionDescRec options[] = {
      { "-helpfile",    "*helpFile",   XrmoptionSepArg, NULL   },
     };
 
 

//------------------- fallback resources -------------------------------//
//------------------- fallback resources -------------------------------//
//------------------- fallback resources -------------------------------//

static String fallback_resources[] = {
    "*helpfile:          Msep_help",
    "*testing:           no",
     NULL
};



//----------------------- closing ---------------------------------//
//----------------------- closing ---------------------------------//
//----------------------- closing ---------------------------------//

        // public virtual function overriding SLApp.
        // called when trying to exit through the window manager.

void MsepitaApplication::closing()
{
  _quit->askQuitQuestion();
}



//--------------------- notify complex -----------------------//
//--------------------- notify complex -----------------------//
//--------------------- notify complex -----------------------//

      // private virtual function overriding SLDelay.
      // called from pulldown menus with ident == one of the enums.

Boolean MsepitaApplication::notifyComplex(SLDelay* /*sender*/, int ident)
{
  switch(ident)
      {
      case QUIT: _quit->askQuitQuestion(); break;
      }
  return TRUE;
}



//------------------------ constructor -------------------------//
//------------------------ constructor -------------------------//
//------------------------ constructor -------------------------//

MsepitaApplication::MsepitaApplication(int argc, char **argv)
         : SLApp("msep", "Msep", argc, argv, options, XtNumber(options)),
              _manager                  (NULL),
              _clist                    (NULL),
              _file                     (NULL),
              _option                   (NULL),
              _helpfile                 (NULL),
              _quit                     (NULL),
              _regulate                 (NULL)
{
  setTitle("Small Motif Static Editing Program (MSEPITA)", FALSE);
  setIcon ("MSEPITA");
  setFallbacks(fallback_resources);
  customResources();
  initHelp(_helpfile, "Help for the MSEPITA program");
  HelpCtx hctx = getHelpCtx();

//-------------------------- create main objects --------------------------//
//-------------------------- create main objects --------------------------//
//-------------------------- create main objects --------------------------//

  _manager       = new StaticManager   ("MSEPITA");
  _regulate      = new RegulateStatgui (_manager);
  _clist         = new ContainerList   ();

//------------------------ create dialog boxes -----------------------//
//------------------------ create dialog boxes -----------------------//
//------------------------ create dialog boxes -----------------------//

  StatpopRead    *aread = new StatpopRead   (this, "aread", _manager, _clist);
  StatpopSave    *asave = new StatpopSave   (this, "asave", _manager, _clist);
  StatpopDetails *att   = new StatpopDetails(this, "att"  , _manager, _clist);
  StatpopList    *list  = new StatpopList   (this, "list" , _manager, _clist);
  StatpopValues  *val   = new StatpopValues (this, "val"  , _manager, _clist);
  StatpopCompare *comp  = new StatpopCompare(this, "comp" , _manager, _clist);
  StatpopSeveral *sev   = new StatpopSeveral(this, "sev"  , _manager, _clist);
  StatpopHistory *hist  = new StatpopHistory(this, "hist" , _manager, _clist);
  StatpopEdit    *editv = new StatpopEdit   (this, "editv", _manager, _clist);
  StatpopNils    *nils  = new StatpopNils   (this, "nils" , _manager, _clist);
  StatpopNils2   *nils2 = new StatpopNils2  (this, "nils2", _manager, _clist);

  StatpopTransform *transform = new StatpopTransform
                                        (this, "transform", _manager, _clist);

  StatpopResample  *resample  = new StatpopResample
                                        (this, "resample" , _manager, _clist);

  StatpopMerge     *merge     = new StatpopMerge
                                        (this, "merge"    , _manager, _clist);

  StatpopSplice    *splice    = new StatpopSplice
                                        (this, "splice"   , _manager, _clist);

//----------------------- populate goto menu --------------------------//
//----------------------- populate goto menu --------------------------//
//----------------------- populate goto menu --------------------------//

////////////// File pulldown menu:
  _clist->add(aread);
  _clist->add(asave);

////////////// Table pulldown menu:
  _clist->putSeparatorAtBottomElement();
  _clist->add(list);
  _clist->add(val);
  _clist->add(comp);
  _clist->add(sev);
  _clist->add(hist);

////////////// Edit pulldown menu:
  _clist->putSeparatorAtBottomElement();
  _clist->add(att);
  _clist->add(editv);
  _clist->add(nils);
  _clist->add(nils2);
  _clist->add(transform);
  _clist->add(resample);
  _clist->add(merge);
  _clist->add(splice);

//-------------------- create pulldown menus ---------------------//
//-------------------- create pulldown menus ---------------------//
//-------------------- create pulldown menus ---------------------//

            _file   = new SLPullPop ("file"  , hctx, this);
  SLPullPop *edit   = new SLPullPop ("edit"  , hctx, this);
            _option = new SLPullPop ("option", hctx, this);
  SLPullPop *table  = new SLPullPop ("table" , hctx, this);
  SLPullPop *help   = new SLPullPop ("help"  , hctx, this);

//------------------- populate file pulldown menu ---------------------//
//------------------- populate file pulldown menu ---------------------//
//------------------- populate file pulldown menu ---------------------//

  _file->addPushUp("aread",  aread);
  _file->addPushUp("asave",  asave);
  _file->addSep();
  _file->addPush("backup1");
  _file->addPush("backup2");
  _file->addSep();
  _file->addPush("compare1");
  _file->addPush("compare2");
  _file->addPush("compare3");
  _file->addSep();
  _file->addPush("Quit", QUIT);

  SLpPush *backup1 = (SLpPush*)_file->primObj("backup1");
  SLpPush *backup2 = (SLpPush*)_file->primObj("backup2");
  _regulate->regulateBackup(backup1, backup2);

  SLpPush *compare1 = (SLpPush*)_file->primObj("compare1");
  SLpPush *compare2 = (SLpPush*)_file->primObj("compare2");
  SLpPush *compare3 = (SLpPush*)_file->primObj("compare3");
  _regulate->regulateCompare(compare1, compare2, compare3);

//------------------- populate edit pulldown menu ---------------------//
//------------------- populate edit pulldown menu ---------------------//
//------------------- populate edit pulldown menu ---------------------//

  edit->addPushUp("att"      , att);
  edit->addPushUp("editv"    , editv);
  edit->addPushUp("nils"     , nils);
  edit->addPushUp("nils2"    , nils2);
  edit->addSep();
  edit->addPushUp("transform", transform);
  edit->addPushUp("resample" , resample);
  edit->addSep();
  edit->addPushUp("merge"    , merge);
  edit->addPushUp("splice"   , splice);

//------------------- populate option pulldown menu ---------------------//
//------------------- populate option pulldown menu ---------------------//
//------------------- populate option pulldown menu ---------------------//

  _option->addTog ("lock1");
  _option->addPush("lock2");
  _option->addPush("lock3");
  _option->addSep();
  _option->addTog ("select1");
  _option->addPush("select2");
  _option->addPush("select3");
  _option->addSep();
  _option->addTog ("inform1");

  SLpToggle *lock1 = (SLpToggle*)_option->primObj("lock1");
  SLpPush   *lock2 = (SLpPush  *)_option->primObj("lock2");
  SLpPush   *lock3 = (SLpPush  *)_option->primObj("lock3");
  _regulate->regulateLock(lock1, lock2, lock3);

  SLpToggle *select1 = (SLpToggle*)_option->primObj("select1");
  SLpPush   *select2 = (SLpPush  *)_option->primObj("select2");
  SLpPush   *select3 = (SLpPush  *)_option->primObj("select3");
  _regulate->regulateSelect(select1, select2, select3);

  SLpToggle *inform1 = (SLpToggle*)_option->primObj("inform1");
  _regulate->regulateInform(inform1);

//------------------- populate table pulldown menu ---------------------//
//------------------- populate table pulldown menu ---------------------//
//------------------- populate table pulldown menu ---------------------//

  table->addPushUp("list", list);
  table->addPushUp("val" , val );
  table->addPushUp("comp", comp);
  table->addPushUp("sev" , sev );
  table->addPushUp("hist", hist);

//------------------- populate help pulldown menu ---------------------//
//------------------- populate help pulldown menu ---------------------//
//------------------- populate help pulldown menu ---------------------//

  help->addPush("apphelp", APPHELP);
  SLpPush *phelp = (SLpPush*)help->primObj(APPHELP);
  phelp->showHelpWhenPressed("APPLICATION_OVERVIEW");

//-------------------- populate main window --------------------------//
//-------------------- populate main window --------------------------//
//-------------------- populate main window --------------------------//

  SLSmartForm *work = new SLSmartForm(this, "mw_work");
  setWorkArea(work);

  StatguiStatus *status = new StatguiStatus(work, _manager, _clist, FALSE);

  work->attach(status, work, work, work, work, 30, 30, 10, 10);

//--------------- finish up and enter event loop ---------------------//
//--------------- finish up and enter event loop ---------------------//
//--------------- finish up and enter event loop ---------------------//

          new StatguiWatch (_manager, this);
  _quit = new StatguiQuit  (this, asave, _manager);

  update();
  loop();
}



//---------------------- destructor -------------------------//
//---------------------- destructor -------------------------//
//---------------------- destructor -------------------------//

MsepitaApplication::~MsepitaApplication()
{
  delete _regulate;
  delete _clist;
  delete _manager;         // saves backup files when necesary.
  exit(0);
}



//--------------------------- end --------------------------------//
//--------------------------- end --------------------------------//
//--------------------------- end --------------------------------//
