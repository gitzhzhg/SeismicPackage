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

//------------------------- vfpop_headers.cc ---------------------------//
//------------------------- vfpop_headers.cc ---------------------------//
//------------------------- vfpop_headers.cc ---------------------------//

//          implementation file for the VfpopHeaders class
//               derived from the VfpopEditBase class
//                       subdirectory vfgui


#include "vfgui/vfpop_headers.hh"
#include "vf/vf_edit_headers.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_text.hh"
#include "sl/slp_push.hh"
#include "sl/slp_option.hh"
#include "sl/slp_label.hh"
#include "sl/slp_toggle.hh"
#include "sl/sl_sep.hh"
#include <stream.h>
#include <assert.h>


#define  EDIT     VfEditHeaders *edit = (VfEditHeaders*)data;
#define  MANAGER  VfManager  *manager = (VfManager*)data;


//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//


#define TRAP(choice_trap, long2, int2, setChoice)      \
                                                       \
static void choice_trap(void *data, long /*ident*/,    \
                     long2 /*oldvar*/, long2 newvar)   \
{                                                      \
  EDIT                                                 \
  edit->setChoice((int2)newvar);                       \
}


TRAP (         choice_trap, long , int  , setChoice)
TRAP (default_project_trap, char*, char*, setDefaultProject)
TRAP (   default_line_trap, char*, char*, setDefaultLine   )
TRAP (  default_rdate_trap, char*, char*, setDefaultRdate  )
TRAP (  default_pdate_trap, char*, char*, setDefaultPdate  )
TRAP ( default_userid_trap, char*, char*, setDefaultUserid )
TRAP (default_comment_trap, char*, char*, setDefaultComment)
TRAP (   edit_project_trap, long , int  , setEditProject)
TRAP (      edit_line_trap, long , int  , setEditLine   )
TRAP (     edit_rdate_trap, long , int  , setEditRdate  )
TRAP (     edit_pdate_trap, long , int  , setEditPdate  )
TRAP (    edit_userid_trap, long , int  , setEditUserid )
TRAP (   edit_comment_trap, long , int  , setEditComment)
TRAP (    set_comment_trap, long , int  , setSetCommentToVfid)



//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//


#define UPFUN(choice_upfun, long2, getChoice)    \
                                                 \
static long2 choice_upfun(void *data)            \
{                                                \
  EDIT                                           \
  return (long2)edit->getChoice();               \
}


UPFUN (         choice_upfun, long , getChoice)
UPFUN (default_project_upfun, char*, getDefaultProject)
UPFUN (   default_line_upfun, char*, getDefaultLine   )
UPFUN (  default_rdate_upfun, char*, getDefaultRdate  )
UPFUN (  default_pdate_upfun, char*, getDefaultPdate  )
UPFUN ( default_userid_upfun, char*, getDefaultUserid )
UPFUN (default_comment_upfun, char*, getDefaultComment)
UPFUN (   edit_project_upfun, long , getEditProject)
UPFUN (      edit_line_upfun, long , getEditLine   )
UPFUN (     edit_rdate_upfun, long , getEditRdate  )
UPFUN (     edit_pdate_upfun, long , getEditPdate  )
UPFUN (    edit_userid_upfun, long , getEditUserid )
UPFUN (   edit_comment_upfun, long , getEditComment)
UPFUN (    set_comment_upfun, long , getSetCommentToVfid)



static long nfun_upfun(void *data)
{
  MANAGER
  return manager->activeDataset()->numVelocityFunctions();
}


static long nsel_upfun(void *data)
{
  MANAGER
  return manager->activeDataset()->numSelectedVelocityFunctions();
}


static long act_upfun(void *data)
{
  MANAGER
  return manager->activeDataset()->getActiveVelocityFunction() + 1;
}



//-------------------- sense update functions ----------------------//
//-------------------- sense update functions ----------------------//
//-------------------- sense update functions ----------------------//


//// some of the above update functions are used here also.



//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


VfpopHeaders::VfpopHeaders(SLDelay *slparent, char *name,
                   VfManager *manager, ContainerList *clist)
       : VfpopEditBase(slparent, name, manager, clist, "HEADERS_OVERVIEW")
{
  _edit = new VfEditHeaders();

  SLSmartForm *row1 = new SLSmartForm(_editor, "row1", NULL);
  SLSmartForm *row2 = new SLSmartForm(_editor, "row2", NULL);
  SLSep       *sep  = new SLSep      (_editor, "sep");

  row1->showEvenSpacing();
  row2->showEvenSpacing();

  SLpOption *choice = new SLpOption(row1, "choice", 0, "");
  choice->addOption("choice", VfEditHeaders::CHOICE_SELECTED,
            "reset headers in SELECTED velocity functions in active dataset");
  choice->addOption("choice", VfEditHeaders::CHOICE_ALL,
            "reset headers in ALL velocity functions in active dataset");
  choice->addOption("choice", VfEditHeaders::CHOICE_ACTIVE,
            "reset headers in ACTIVE velocity function in active dataset");

  SL2Text *nfun  = new SL2Text (row2, "nfun", 0,
                           "#functions in active dataset:", SLpText::_LONG, 6);
  SL2Text *nsel  = new SL2Text (row2, "nsel", 0,
                           "#selected:", SLpText::_LONG, 6);
  SL2Text *act   = new SL2Text (row2, "act" , 0,
                           "active function:", SLpText::_LONG, 6);

  SLpToggle *tog1 = new SLpToggle (_editor, "tog", 0,
                            "set project to this field:");
  SLpToggle *tog2 = new SLpToggle (_editor, "tog", 0,
                            "set line to this field:");
  SLpToggle *tog3 = new SLpToggle (_editor, "tog", 0,
                            "set rdate to this field:");
  SLpToggle *tog4 = new SLpToggle (_editor, "tog", 0,
                            "set pdate to this field:");
  SLpToggle *tog5 = new SLpToggle (_editor, "tog", 0,
                            "set userid to this field:");
  SLpToggle *tog6 = new SLpToggle (_editor, "tog", 0,
                            "set comment to this field:");
  SLpToggle *tog7 = new SLpToggle (_editor, "tog", 0,
                            "set comment to velocity function name");

  SLpText *text1 = new SLpText (_editor, "text", 0, SLpText::_CHAR, 10);
  SLpText *text2 = new SLpText (_editor, "text", 0, SLpText::_CHAR, 10);
  SLpText *text3 = new SLpText (_editor, "text", 0, SLpText::_CHAR,  5);
  SLpText *text4 = new SLpText (_editor, "text", 0, SLpText::_CHAR,  5);
  SLpText *text5 = new SLpText (_editor, "text", 0, SLpText::_CHAR,  3);
  SLpText *text6 = new SLpText (_editor, "text", 0, SLpText::_CHAR, 15);

  nfun->showLabelAppearance();
  nsel->showLabelAppearance();
  act ->showLabelAppearance();

  choice->setItrap (         choice_trap, _edit);
  text1 ->setCtrap (default_project_trap, _edit);
  text2 ->setCtrap (   default_line_trap, _edit);
  text3 ->setCtrap (  default_rdate_trap, _edit);
  text4 ->setCtrap (  default_pdate_trap, _edit);
  text5 ->setCtrap ( default_userid_trap, _edit);
  text6 ->setCtrap (default_comment_trap, _edit);
  tog1  ->setItrap (   edit_project_trap, _edit);
  tog2  ->setItrap (      edit_line_trap, _edit);
  tog3  ->setItrap (     edit_rdate_trap, _edit);
  tog4  ->setItrap (     edit_pdate_trap, _edit);
  tog5  ->setItrap (    edit_userid_trap, _edit);
  tog6  ->setItrap (   edit_comment_trap, _edit);
  tog7  ->setItrap (    set_comment_trap, _edit);


  choice->setupIvarFun (         choice_upfun, _edit);
  nfun  ->setupIvarFun (           nfun_upfun, manager);
  nsel  ->setupIvarFun (           nsel_upfun, manager);
  act   ->setupIvarFun (            act_upfun, manager);
  text1 ->setupCvarFun (default_project_upfun, _edit);
  text2 ->setupCvarFun (   default_line_upfun, _edit);
  text3 ->setupCvarFun (  default_rdate_upfun, _edit);
  text4 ->setupCvarFun (  default_pdate_upfun, _edit);
  text5 ->setupCvarFun ( default_userid_upfun, _edit);
  text6 ->setupCvarFun (default_comment_upfun, _edit);
  tog1  ->setupIvarFun (   edit_project_upfun, _edit);
  tog2  ->setupIvarFun (      edit_line_upfun, _edit);
  tog3  ->setupIvarFun (     edit_rdate_upfun, _edit);
  tog4  ->setupIvarFun (     edit_pdate_upfun, _edit);
  tog5  ->setupIvarFun (    edit_userid_upfun, _edit);
  tog6  ->setupIvarFun (   edit_comment_upfun, _edit);
  tog7  ->setupIvarFun (    set_comment_upfun, _edit);

  text1  ->setupSenseFun (   edit_project_upfun, _edit);
  text2  ->setupSenseFun (      edit_line_upfun, _edit);
  text3  ->setupSenseFun (     edit_rdate_upfun, _edit);
  text4  ->setupSenseFun (     edit_pdate_upfun, _edit);
  text5  ->setupSenseFun (    edit_userid_upfun, _edit);
  text6  ->setupSenseFun (   edit_comment_upfun, _edit);


//                         LEFT     RIGHT    TOP      BOTTOM
  _editor->attach(row1  , _editor, _editor, _editor,  NULL  ,  0,  0, 5, 5);
  _editor->attach(row2  , _editor, _editor,  row1  ,  NULL  ,  0,  0, 5, 5);
  _editor->attach(sep   , _editor, _editor,  row2  ,  NULL  ,  0,  0, 5, 5);
  _editor->attach(tog1  , _editor,  NULL  ,  sep   ,  NULL  , 90,  0, 5, 5);
  _editor->attach(tog2  , _editor,  NULL  ,  tog1  ,  NULL  , 90,  0, 5, 5);
  _editor->attach(tog3  , _editor,  NULL  ,  tog2  ,  NULL  , 90,  0, 5, 5);
  _editor->attach(tog4  , _editor,  NULL  ,  tog3  ,  NULL  , 90,  0, 5, 5);
  _editor->attach(tog5  , _editor,  NULL  ,  tog4  ,  NULL  , 90,  0, 5, 5);
  _editor->attach(tog6  , _editor,  NULL  ,  tog5  ,  NULL  , 90,  0, 5, 5);
  _editor->attach(tog7  , _editor,  NULL  ,  tog6  , _editor, 90,  0, 5, 5);
  _editor->attach(text1 ,  tog1  ,  NULL  ,  sep   ,  NULL  , 10,  0, 5, 5);
  _editor->attach(text2 ,  tog2  ,  NULL  ,  tog1  ,  NULL  , 10,  0, 5, 5);
  _editor->attach(text3 ,  tog3  ,  NULL  ,  tog2  ,  NULL  , 10,  0, 5, 5);
  _editor->attach(text4 ,  tog4  ,  NULL  ,  tog3  ,  NULL  , 10,  0, 5, 5);
  _editor->attach(text5 ,  tog5  ,  NULL  ,  tog4  ,  NULL  , 10,  0, 5, 5);
  _editor->attach(text6 ,  tog6  ,  NULL  ,  tog5  ,  NULL  , 10,  0, 5, 5);
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopHeaders::~VfpopHeaders()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
