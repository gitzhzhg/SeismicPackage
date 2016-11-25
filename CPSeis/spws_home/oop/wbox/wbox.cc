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

//------------------------------- wbox.cc ------------------------------//
//------------------------------- wbox.cc ------------------------------//
//------------------------------- wbox.cc ------------------------------//

  // this file contains all C-callable public functions.

  // this file also contains the following:
  //  (1) the static object WboxAllbox *allbox.
  //  (2) the private C++ function ubox_register_fortran_trap_handler
  //       which is called from the cbox_wrappers.cc file.
  //  (3) the private static C++ function ubox_fortran_trap_handler
  //       which is called (by pointer) from the WboxVector class.


#include "wbox.h"
#include "wbox/wbox_allbox.hh"
#include "wbox/wbox_box.hh"
#include "wbox/wbox_vector.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>


    //   function prefix               status     filename    
    //   ---------------               ------     ---------    
    //   wbox_  dbox_  bbox_  ibox_    public     wbox.cc       
    //   ubox_                         private    wbox.cc       
    //   fbox_  nbox_                  public     fbox_wrappers.f
    //   cbox_                         private    cbox_wrappers.cc



//----------------- ubox register fortran trap handler ---------------//
//----------------- ubox register fortran trap handler ---------------//
//----------------- ubox register fortran trap handler ---------------//

        // private.
        // called only from cbox_wrappers.cc.
        // prototype is in cbox_wrappers.cc.
        // sets static variable trap_handler, which is a pointer
        //   to a subroutine in cbox_wrappers.cc.

static WboxAllbox::WboxFortranTrapHandler *trap_handler = NULL;


void ubox_register_fortran_trap_handler
                      (WboxAllbox::WboxFortranTrapHandler *handler)
{
  trap_handler = handler;
}



//----------------- ubox fortran trap handler ---------------------//
//----------------- ubox fortran trap handler ---------------------//
//----------------- ubox fortran trap handler ---------------------//

        // private.
        // called only from WboxVector.
        // registered with WboxAllbox below.
        // calls subroutine in cbox_wrappers.cc pointed to by the
        //   static variable trap_handler.

static void ubox_fortran_trap_handler(WboxGenericTrap *trap, int traptype,
                                      int ibox, int *ident, int *index,
                                      char *text, int nread, char *endkey)
{
  if(!trap_handler) return;
  switch(traptype)
     {
     case WboxVector::_OLDF: traptype = STANDARD_FORTRAN_TRAPTYPE; break;
     case WboxVector::_SIMP: traptype =   SIMPLE_FORTRAN_TRAPTYPE; break;
     case WboxVector::_EZED: traptype =     EZED_FORTRAN_TRAPTYPE; break;
     default               : assert(FALSE);
     }
  trap_handler(trap, traptype, ibox, ident, index, text, nread, endkey);
}



//------------------ static declarations ------------------------------//
//------------------ static declarations ------------------------------//
//------------------ static declarations ------------------------------//

    // private.
    // allbox is only a static pointer, rather than a static object,
    //   because the HP does not call the WboxAllbox constructor when
    //   allbox is a static object!

static WboxAllbox *allbox = NULL;


static inline void ccc()
{
  if(allbox == NULL)
      {
      allbox = new WboxAllbox();
      allbox->registerFortranTrapHandler(ubox_fortran_trap_handler);
      }
}


static WboxBox *get_growing_box()
{
  assert(allbox);
  WboxBox *box = allbox->getBoxBeingCreated();
  assert(box);
  return box;
}





             //-------- miscellaneous functions --------//
             //-------- miscellaneous functions --------//
             //-------- miscellaneous functions --------//
             //-------- miscellaneous functions --------//
             //-------- miscellaneous functions --------//
             //-------- miscellaneous functions --------//
             //-------- miscellaneous functions --------//
             //-------- miscellaneous functions --------//
             //-------- miscellaneous functions --------//
             //-------- miscellaneous functions --------//
             //-------- miscellaneous functions --------//
             //-------- miscellaneous functions --------//
             //-------- miscellaneous functions --------//



//---------------- set and get resources --------------------------//
//---------------- set and get resources --------------------------//
//---------------- set and get resources --------------------------//

          // public.

void wbox_set_maxrows (int maxrows)  { ccc(); allbox->setMaxRows(maxrows); }
int  wbox_get_maxrows (void)         { ccc(); return allbox->getMaxRows(); }


void wbox_set_debug(int debug)  { ccc(); allbox->setDebug(debug); }
int  wbox_get_debug(void)       { ccc(); return allbox->getDebug(); }
int  wbox_get_tiny_size(void)   { ccc(); return allbox->getTinySize(); }


void wbox_set_keymode(int keymode)  { ccc(); allbox->setKeymode(keymode); }
int  wbox_get_keymode(void)         { ccc(); return allbox->getKeymode(); }
void wbox_toggle_keymode(void)      { ccc(); allbox->toggleKeymode(); }



//------------ display message at bottom of windowbox --------------//
//------------ display message at bottom of windowbox --------------//
//------------ display message at bottom of windowbox --------------//

          // public.

void wbox_maybe_show_message(const char *msg)
{
  ccc();
  int ibox = allbox->getTrapBoxNumber();
  if(ibox <= 0) return;
  WboxBox *box = allbox->getBoxPointer(ibox);
  if(box) box->maybeShowMessage(msg);
}



void wbox_show_message(const char *msg)
{
  ccc();
  int ibox = allbox->getTrapBoxNumber();
  if(ibox <= 0) return;
  WboxBox *box = allbox->getBoxPointer(ibox);
  if(box) box->showMessage(msg);
}


void wbox_immediate_message(const char *msg)
{
  wbox_show_message(msg);
}



void wbox_messageline(void *box2, const char *msg)
{
  WboxBox *box = (WboxBox*)box2;
  if(box) box->showMessage(msg);
}



//----------------- save table to file for hardcopy ---------------//
//----------------- save table to file for hardcopy ---------------//
//----------------- save table to file for hardcopy ---------------//

         // public.

void wbox_save_table(void *box2, const char *filename)
{
  WboxBox *box = (WboxBox*)box2;
  if(box) box->saveTable(filename);
}



//------------------- get information --------------------------//
//------------------- get information --------------------------//
//------------------- get information --------------------------//

          // public.

void wbox_get_name(void *box2, char *boxname)
{
  WboxBox *box = (WboxBox*)box2;
  assert(box);
  strcpy(boxname, box->getBoxName());
}


int wbox_get_ibox(void *box2)
{
  WboxBox *box = (WboxBox*)box2;
  assert(box);
  return box->getIbox();
}


int wbox_get_num_boxes(void)
{
  ccc();
  return allbox->totNumBox();
}


void *wbox_get_pointer(int ibox)
{
  ccc();
  return allbox->getBoxPointer(ibox);
}


void *wbox_find_pointer(const char *boxname)
{
  ccc();
  return allbox->findBoxPointer(boxname);
}


Widget wbox_get_tiny(void *box2)
{
  WboxBox *box = (WboxBox*)box2;
  if(box) return box->getTinyPushbutton();
  return NULL;
}



//--------------------- get true or false -----------------------//
//--------------------- get true or false -----------------------//
//--------------------- get true or false -----------------------//

        // public.

int wbox_managed(int ibox)
{
  ccc();
  WboxBox *box = allbox->getBoxPointer(ibox);
  if(box) return box->isManaged();
  return FALSE;
}


int wbox_in_trap(void)
{
  ccc();
  int ibox = allbox->getTrapBoxNumber();
  if(ibox > 0) return TRUE;
  return FALSE;
}


int wbox_is_visible(void *box2, int ident)
{
  WboxBox *box = (WboxBox*)box2;
  return box->identIsVisible(ident);
}



//--------------- manage or unmanage or destroy -----------------//
//--------------- manage or unmanage or destroy -----------------//
//--------------- manage or unmanage or destroy -----------------//

        // public.

void wbox_manage(const char *boxname)
{
  ccc();
  WboxBox *box = allbox->findBoxPointer(boxname);
  if(box) box->manage();
}


void wbox_unmanage(const char *boxname)
{
  ccc();
  WboxBox *box = allbox->findBoxPointer(boxname);
  if(box) box->unmanage();
}


void wbox_destroy(const char *boxname)
{
  ccc();
  WboxBox *box = allbox->findBoxPointer(boxname);
  if(box) box->destroy();
}


void wbox_manage_box(void *box2)
{
  WboxBox *box = (WboxBox*)box2;
  if(box) box->manage();
}


void wbox_unmanage_box(void *box2)
{
  WboxBox *box = (WboxBox*)box2;
  if(box) box->unmanage();
}


void wbox_destroy_box(void *box2)
{
  WboxBox *box = (WboxBox*)box2;
  if(box) box->destroy();
}



//------------------------ do something -------------------------//
//------------------------ do something -------------------------//
//------------------------ do something -------------------------//

          // public.


void wbox_flush_buffer(void)
{
  ccc();
  allbox->flushBuffer();
}


void wbox_ring_bell(void)
{
  ccc();
  allbox->ringBell();
}


void wbox_waste_time(int n)
{
  ccc();
  allbox->wasteTime(n);
}


void wbox_set_focus(void *box2, int ident, int index)
{
  WboxBox *box = (WboxBox*)box2;
  if(box) box->setFocus(ident, index);
}


void wbox_update(void)
{
  ccc();
  allbox->update();
}



//---------------------------- send event ---------------------------//
//---------------------------- send event ---------------------------//
//---------------------------- send event ---------------------------//

       // public.

void wbox_event(void *box2, const char *endkey)
{
  WboxBox *box = (WboxBox*)box2;
  if(box) box->sendImmediateEvent(endkey);
}


void wbox_send_event(void *box2, const char *endkey)
{
  WboxBox *box = (WboxBox*)box2;
  if(box == NULL) return;
  box->sendClientMessageEvent(endkey);
}


void wbox_send_message(int ibox)
{
  WboxBox *box = (WboxBox*)wbox_get_pointer(ibox);
  if(box == NULL) return;
  box->sendClientMessageEvent("CLIENT");
}
    


//---------------------- restore previous user value ---------------------//
//---------------------- restore previous user value ---------------------//
//---------------------- restore previous user value ---------------------//

       // public.

void wbox_restore_previous_user_value
                       (void *box2, long *ident, long *index, long *istat)
{
  WboxBox *box = (WboxBox*)box2;
  int ident2, index2, istat2;
  box->restorePreviousUserValue(&ident2, &index2, &istat2);
  *ident = ident2;
  *index = index2;
  *istat = istat2;
}



void ibox_restore_previous_user_value
                       (void *box2, int  *ident, int  *index, int  *istat)
{
  WboxBox *box = (WboxBox*)box2;
  box->restorePreviousUserValue(ident, index, istat);
}








              //------------ create windowbox -----------//
              //------------ create windowbox -----------//
              //------------ create windowbox -----------//
              //------------ create windowbox -----------//
              //------------ create windowbox -----------//
              //------------ create windowbox -----------//
              //------------ create windowbox -----------//
              //------------ create windowbox -----------//
              //------------ create windowbox -----------//
              //------------ create windowbox -----------//
              //------------ create windowbox -----------//
              //------------ create windowbox -----------//



//---------------------- create unmanaged windowbox ---------------------//
//---------------------- create unmanaged windowbox ---------------------//
//---------------------- create unmanaged windowbox ---------------------//

               // public.

  // default_trap is a trap which is to be called from a datafield
  //   when no other trap has been specified for that datafield.     
  // default_trap can be NULL (meaning there is no default trap).
  // default_trap must be one of the following types:
  //   WboxFortranTrap, WboxSimpleTrap, WboxEzedTrap, or WboxClangTrap,
  //   cast to (WboxGenericTrap*).
  // the default trap cannot be any of the type-specific traps.         
  // regardless of the type of the (possibly NULL) default trap, any 
  //   type of trap can be registered with any datafield, overriding
  //   the default trap.                                           

 
void *wbox_create1_more(WboxGenericTrap *default_trap, int default_traptype,
                        int omit, int nrows_init)
{
  ccc();
  switch(default_traptype)
      {
      case  STANDARD_FORTRAN_TRAPTYPE:
                         default_traptype = WboxVector::_OLDF; break;
      case    SIMPLE_FORTRAN_TRAPTYPE:
                         default_traptype = WboxVector::_SIMP; break;
      case      EZED_FORTRAN_TRAPTYPE:
                         default_traptype = WboxVector::_EZED; break;
      case        STANDARD_C_TRAPTYPE:
                         default_traptype = WboxVector::_OLDC; break;
      default          : default_traptype = WboxVector::_OLDC; break;
      }
  WboxBox *box = allbox->create1
                      (default_trap, default_traptype, omit, nrows_init);
  return (void*)box;
}


void *wbox_create1(WboxGenericTrap *default_trap, int default_traptype)
{
  ccc();
  switch(default_traptype)
      {
      case  STANDARD_FORTRAN_TRAPTYPE:
                         default_traptype = WboxVector::_OLDF; break;
      case    SIMPLE_FORTRAN_TRAPTYPE:
                         default_traptype = WboxVector::_SIMP; break;
      case      EZED_FORTRAN_TRAPTYPE:
                         default_traptype = WboxVector::_EZED; break;
      case        STANDARD_C_TRAPTYPE:
                         default_traptype = WboxVector::_OLDC; break;
      default          : default_traptype = WboxVector::_OLDC; break;
      }
  WboxBox *box = allbox->create1(default_trap, default_traptype);
  return (void*)box;
}



  // callable from c only.                                
  // if hctx = NULL, helpfile and helptitle are used to create help.
  // if w = NULL, drawing area widget is created.            
  // if parent & w = NULL, dialog box is created to contain drawing area.
  // if parent & w = NULL, toplevel will be the parent of dialog box.   
  // if parent & w = NULL, dialog box will be the parent of drawing area.
  // at least one of w, parent, and toplevel must not be NULL.
  // toplevel does not have to be the actual toplevel shell.

Widget wbox_create2(const char *boxname,
                    Widget toplevel, Widget parent, Widget w,
                    HelpCtx hctx, const char *helpfile, const char *helptitle)
{ 
  assert(allbox);
  w = allbox->create2(boxname, toplevel, parent, w,
                                 hctx, helpfile, helptitle);
  return w;
}



//---------------------- create managed windowbox ---------------------//
//---------------------- create managed windowbox ---------------------//
//---------------------- create managed windowbox ---------------------//

              // public.

    // the parent must exist
    // the help context should exist
    // does no attachments

void *dbox_create1(void *userdata)
{
  ccc();
  WboxBox *box = allbox->create1(NULL, STANDARD_C_TRAPTYPE);
  box->setUserData(userdata);
  return (void*)box;
}


Widget dbox_create2(Widget parent, const char *boxname, HelpCtx hctx)
{
  assert(allbox);
  Widget w = allbox->create2(boxname, NULL, parent, NULL, hctx, NULL, NULL);
  XtManageChild(w);
  return w;
}



//------------ get or put userdata ------------------------------------//
//------------ get or put userdata ------------------------------------//
//------------ get or put userdata ------------------------------------//

              // public.

void wbox_put_userdata(void *box, void *userdata)
{ ((WboxBox*)box)->setUserData(userdata); }


void *wbox_get_userdata(void *box)
{ return ((WboxBox*)box)->getUserData(); }



//---- register a user function to be called prior to updating --------//
//---- register a user function to be called prior to updating --------//
//---- register a user function to be called prior to updating --------//
 
              // public.

void wbox_additional_update(WboxUpdate *updatename, void *updatedata)
{
  ccc();
  allbox->registerAdditionalUpdate(updatename, updatedata);
}



             //--------- create linked array set ---------//
             //--------- create linked array set ---------//
             //--------- create linked array set ---------//
             //--------- create linked array set ---------//
             //--------- create linked array set ---------//
             //--------- create linked array set ---------//
             //--------- create linked array set ---------//
             //--------- create linked array set ---------//
             //--------- create linked array set ---------//
             //--------- create linked array set ---------//



                          // public.

void wbox_rega(long *npoint, long *nmaxpoint, int irow, int numrow)
{
  WboxBox *box = get_growing_box();
  box->createLinkedArraySet(irow, numrow, npoint, nmaxpoint);
}



void ibox_rega(int *npoint, int *nmaxpoint, int irow, int numrow)
{
  WboxBox *box = get_growing_box();
  box->createLinkedArraySet(irow, numrow, npoint, nmaxpoint);
}



void dbox_rega(WboxNupdateFun *nfun, WboxNupdateFun *nmaxfun,
                 int irow, int icol, int nchar, int numrow)
{
  WboxBox *box = get_growing_box();
  box->createLinkedArraySet(irow, numrow, nfun, nmaxfun);

  if(nchar <= 0) return;          // added 2001-08-09
  int ident = 0;
  int ncharp = 1;
  int lengthp = 1;
  static long spoint  = -45;      // changed from -44  2001-08-08
//if(nchar <= 0) nchar = 4;       // removed 2001-08-09
  bbox_irega(ident, ncharp, lengthp, icol, nchar);
  dbox_set_index_behavior(ident);
  dbox_set_spoint        (ident, &spoint);
}



void dbox_rega2(WboxNupdateFun *nfun, WboxNupdateFun *nmaxfun,
                 int irow, int icol, int nchar, int numrow, int numrow_init)
{
  WboxBox *box = get_growing_box();
  box->createLinkedArraySet(irow, numrow, nfun, nmaxfun, numrow_init);

  if(nchar <= 0) return;          // added 2001-08-09
  int ident = 0;
  int ncharp = 1;
  int lengthp = 1;
  static long spoint  = -45;      // changed from -44  2001-08-08
//if(nchar <= 0) nchar = 4;       // removed 2001-08-09
  bbox_irega(ident, ncharp, lengthp, icol, nchar);
  dbox_set_index_behavior(ident);
  dbox_set_spoint        (ident, &spoint);
}







        //-------------- create datafields --------------//
        //-------------- create datafields --------------//
        //-------------- create datafields --------------//
        //-------------- create datafields --------------//
        //-------------- create datafields --------------//
        //-------------- create datafields --------------//
        //-------------- create datafields --------------//
        //-------------- create datafields --------------//
        //-------------- create datafields --------------//
        //-------------- create datafields --------------//
        //-------------- create datafields --------------//
        //-------------- create datafields --------------//
        //-------------- create datafields --------------//


  // these functions apply only to the current windowbox being
  //   created, and assert if no windowbox is in the process of
  //   being created.
  // the trap is assumed to be the type corresponding to the default
  //   traptype specified to WboxBox.  if there is no default traptype,
  //   the trap is assumed to be clanguage type (WboxClangTrap).
  // the trap cannot be a newstyle type-sensitive trap, because
  //   the trap is registered with the variable datafield and also
  //   with the prompt datafield, and these are usually different types.
  // when calling these functions, the trap must be re-cast to
  //   WboxGenericTrap from its actual type.
  // if the trap argument is NULL, the default trap will be used if there
  //   is one.



//---------------------- shorthand registrations ----------------------//
//---------------------- shorthand registrations ----------------------//
//---------------------- shorthand registrations ----------------------//

                  // public.

void wbox_mreg (char *msg, int irow, int icol)
{
  WboxBox *box = get_growing_box();
  int ident  = 0;
  int nchar  = 0;
  int length = strlen(msg);
  box->createCvarDatafield(ident, irow, icol, nchar, length);
  dbox_set_cpoint      (ident, msg);
}


void wbox_message (char *msg)
{
  int irow = 0;
  int icol = 1;
  wbox_mreg(msg, irow, icol);
}


void wbox_blank_line (void)
{
  WboxBox *box = get_growing_box();
  int ident  = 0;
  int irow   = 0;
  int icol   = 1;
  int nchar  = 0;
  int length = 1;
  box->createCvarDatafield(ident, irow, icol, nchar, length);
}



//--------------------- bbox registrations ------------------------//
//--------------------- bbox registrations ------------------------//
//--------------------- bbox registrations ------------------------//

                  // public.

   // these are the most general (and "best") registrations.
   // the prompts and character variables need not be
   //   null-terminated, and therefore may reside within
   //   fortran code.

   // all other registrations (C and fortran) call these functions.
   // these functions get passed the length of character
   //   variables, and the width of the prompt fields, which the
   //   other functions lack.
   // these functions can be publicly called.
   // other functions are retained for backward compatibility.


void bbox_ireg (int ident, int irow, int icol, int nchar)
{
  WboxBox *box = get_growing_box();
  box->createIvarDatafield(ident, irow, icol, nchar);
}



void bbox_freg (int ident, int irow, int icol, int nchar, int ndec)
{
  WboxBox *box = get_growing_box();
  box->createFvarDatafield(ident, irow, icol, nchar, ndec);
}



void bbox_dreg (int ident, int irow, int icol, int nchar, int ndec)
{
  WboxBox *box = get_growing_box();
  box->createDvarDatafield(ident, irow, icol, nchar, ndec);
}



void bbox_creg (int ident, int irow, int icol, int nchar, int length)
{
  WboxBox *box = get_growing_box();
  box->createCvarDatafield(ident, irow, icol, nchar, length);
}



void bbox_ireg2 (int ident,
                 int ncharp, int lengthp,
                 int irow, int icol, int nchar)
{
  WboxBox *box = get_growing_box();
  box->createIvarDatafieldPair(ident, irow, icol,
                               ncharp, lengthp, nchar);
}


void bbox_freg2 (int ident,
                 int ncharp, int lengthp,
                 int irow, int icol, int nchar, int ndec)
{
  WboxBox *box = get_growing_box();
  box->createFvarDatafieldPair(ident, irow, icol,
                               ncharp, lengthp, nchar, ndec);
}


void bbox_dreg2 (int ident,
                 int ncharp, int lengthp,
                 int irow, int icol, int nchar, int ndec)
{
  WboxBox *box = get_growing_box();
  box->createDvarDatafieldPair(ident, irow, icol,
                               ncharp, lengthp, nchar, ndec);
}


void bbox_creg2 (int ident,
                 int ncharp, int lengthp,
                 int irow, int icol, int nchar, int length)
{
  WboxBox *box = get_growing_box();
  box->createCvarDatafieldPair(ident, irow, icol,
                               ncharp, lengthp, nchar, length);
}


void bbox_ireg3 (int ident,
                 int ncharp, int lengthp,
                 int irow, int icol, int nchar)
{
  WboxBox *box = get_growing_box();
  box->createIvarDatafieldRevPair(ident, irow, icol,
                               ncharp, lengthp, nchar);
}




void bbox_irega (int ident,
                 int ncharp, int lengthp,
                 int icol, int nchar)
{
  WboxBox *box = get_growing_box();
  box->createIvarDatafieldArray(ident, icol,
                                ncharp, lengthp, nchar);
}



void bbox_frega (int ident,
                 int ncharp, int lengthp,
                 int icol, int nchar, int ndec)
{
  WboxBox *box = get_growing_box();
  box->createFvarDatafieldArray(ident, icol,
                                ncharp, lengthp, nchar, ndec);
}



void bbox_drega (int ident,
                 int ncharp, int lengthp,
                 int icol, int nchar, int ndec)
{
  WboxBox *box = get_growing_box();
  box->createDvarDatafieldArray(ident, icol,
                                ncharp, lengthp, nchar, ndec);
}



void bbox_crega (int ident,
                 int ncharp, int lengthp,
                 int icol, int nchar, int length)
{
  WboxBox *box = get_growing_box();
  box->createCvarDatafieldArray(ident, icol,
                                ncharp, lengthp, nchar, length);
}





//------------------ wbox registrations ------------------------------//
//------------------ wbox registrations ------------------------------//
//------------------ wbox registrations ------------------------------//

                   // public.

     // these functions take a pointer to the prompt (if there is one),
     //   and a pointer to the variable (which can be NULL).
     // also, these functions take a trap (which can be NULL).

     // the prompt must be null-terminated (and can be NULL for
     //              ...rega but not for ...reg2).
     // the length of the prompt is obtained from the prompt or
     //   from nchar.
     // the width of the prompt field is obtained from the prompt
     //   or from nchar.
     // the length of a character variable is obtained from the
     //   character variable or from nchar.


void wbox_ireg (WboxGenericTrap *trap, int ident, long *ipoint, long *spoint,
                int irow, int icol, int nchar, int /*ndec*/)
{
  bbox_ireg(ident, irow, icol, nchar);
  dbox_set_generic_trap(ident, trap);
  dbox_set_ipoint      (ident, ipoint);
  dbox_set_spoint      (ident, spoint);
}



void wbox_freg (WboxGenericTrap *trap, int ident, float *fpoint, long *spoint,
                int irow, int icol, int nchar, int ndec)
{
  bbox_freg(ident, irow, icol, nchar, ndec);
  dbox_set_generic_trap(ident, trap);
  dbox_set_fpoint      (ident, fpoint);
  dbox_set_spoint      (ident, spoint);
}



void wbox_dreg (WboxGenericTrap *trap, int ident, double *dpoint, long *spoint,
                int irow, int icol, int nchar, int ndec)
{
  bbox_dreg(ident, irow, icol, nchar, ndec);
  dbox_set_generic_trap(ident, trap);
  dbox_set_dpoint      (ident, dpoint);
  dbox_set_spoint      (ident, spoint);
}



void wbox_creg (WboxGenericTrap *trap, int ident, char *cpoint, long *spoint,
                int irow, int icol, int nchar, int /*ndec*/)
{
  int length;
  if(cpoint) length = strlen(cpoint);
  else       length = nchar;
  bbox_creg(ident, irow, icol, nchar, length);
  dbox_set_generic_trap(ident, trap);
  dbox_set_cpoint      (ident, cpoint);
  dbox_set_spoint      (ident, spoint);
}



void wbox_ireg2 (WboxGenericTrap *trap, int ident,
                 char *cprompt, long *sprompt,
                 long *ipoint,  long *spoint,
                 int irow, int icol, int nchar, int /*ndec*/)
{
  assert(cprompt);
  int lengthp   = strlen(cprompt);
  int ncharp = lengthp;
  bbox_ireg2(ident, ncharp, lengthp, irow, icol, nchar);
  dbox_set_generic_trap(-ident, trap);
  dbox_set_cpoint      (-ident, cprompt);
  dbox_set_spoint      (-ident, sprompt);
  dbox_set_generic_trap( ident, trap);
  dbox_set_ipoint      ( ident, ipoint);
  dbox_set_spoint      ( ident, spoint);
}


void wbox_freg2 (WboxGenericTrap *trap, int ident,
                 char  *cprompt, long *sprompt,
                 float *fpoint,  long *spoint,
                 int irow, int icol, int nchar, int ndec)
{
  assert(cprompt);
  int lengthp   = strlen(cprompt);
  int ncharp = lengthp;
  bbox_freg2(ident, ncharp, lengthp, irow, icol, nchar, ndec);
  dbox_set_generic_trap(-ident, trap);
  dbox_set_cpoint      (-ident, cprompt);
  dbox_set_spoint      (-ident, sprompt);
  dbox_set_generic_trap( ident, trap);
  dbox_set_fpoint      ( ident, fpoint);
  dbox_set_spoint      ( ident, spoint);
}


void wbox_dreg2 (WboxGenericTrap *trap, int ident,
                 char   *cprompt, long *sprompt,
                 double *dpoint,  long *spoint,
                 int irow, int icol, int nchar, int ndec)
{
  assert(cprompt);
  int lengthp   = strlen(cprompt);
  int ncharp = lengthp;
  bbox_dreg2(ident, ncharp, lengthp, irow, icol, nchar, ndec);
  dbox_set_generic_trap(-ident, trap);
  dbox_set_cpoint      (-ident, cprompt);
  dbox_set_spoint      (-ident, sprompt);
  dbox_set_generic_trap( ident, trap);
  dbox_set_dpoint      ( ident, dpoint);
  dbox_set_spoint      ( ident, spoint);
}


void wbox_creg2 (WboxGenericTrap *trap, int ident,
                 char *cprompt, long *sprompt,
                 char *cpoint,  long *spoint,
                 int irow, int icol, int nchar, int /*ndec*/)
{
  assert(cprompt);
  int lengthp   = strlen(cprompt);
  int ncharp = lengthp;
  int length;
  if(cpoint) length = strlen(cpoint);
  else       length = nchar;
  bbox_creg2(ident, ncharp, lengthp, irow, icol, nchar, length);
  dbox_set_generic_trap(-ident, trap);
  dbox_set_cpoint      (-ident, cprompt);
  dbox_set_spoint      (-ident, sprompt);
  dbox_set_generic_trap( ident, trap);
  dbox_set_cpoint      ( ident, cpoint);
  dbox_set_spoint      ( ident, spoint);
}


void wbox_ireg3 (WboxGenericTrap *trap, int ident,
                 char *cprompt, long *sprompt,
                 long *ipoint,  long *spoint,
                 int irow, int icol, int nchar, int /*ndec*/)
{
  assert(cprompt);
  int lengthp   = strlen(cprompt);
  int ncharp = lengthp;
  bbox_ireg3(ident, ncharp, lengthp, irow, icol, nchar);
  dbox_set_generic_trap(-ident, trap);
  dbox_set_cpoint      (-ident, cprompt);
  dbox_set_spoint      (-ident, sprompt);
  dbox_set_generic_trap( ident, trap);
  dbox_set_ipoint      ( ident, ipoint);
  dbox_set_spoint      ( ident, spoint);
}



void wbox_xrega(WboxGenericTrap *trap, int ident,
                char   *cprompt,  long *sprompt,
                void* /*ipoint*/, long *spoint,
                int icol, int nchar, int /*ndec*/)
{
  int lengthp = nchar;
  if(cprompt) lengthp = strlen(cprompt);
  int ncharp = MaximumValue(lengthp, nchar);

  bbox_irega(ident, ncharp, lengthp, icol, nchar);
  dbox_set_generic_trap  (-ident, trap);
  dbox_set_cpoint        (-ident, cprompt);
  dbox_set_spoint        (-ident, sprompt);
  dbox_set_generic_trap  ( ident, trap);
  dbox_set_index_behavior( ident);
  dbox_set_spoint        ( ident, spoint);
}



void wbox_rrega(WboxGenericTrap *trap, int ident,
                char *cprompt, long *sprompt,
                long *ipoint,  long *spoint,
                int icol, int nchar, int /*ndec*/)
{
  int lengthp = nchar;
  if(cprompt) lengthp = strlen(cprompt);
  int ncharp = MaximumValue(lengthp, nchar);

  bbox_irega(ident, ncharp, lengthp, icol, nchar);
  dbox_set_generic_trap(-ident, trap);
  dbox_set_cpoint      (-ident, cprompt);
  dbox_set_spoint      (-ident, sprompt);
  dbox_set_generic_trap( ident, trap);
  dbox_set_rpoint      ( ident, ipoint);
  dbox_set_spoint      ( ident, spoint);
}



void wbox_irega(WboxGenericTrap *trap, int ident,
                char *cprompt, long *sprompt,
                long *ipoint,  long *spoint,
                int icol, int nchar, int /*ndec*/)
{
  int lengthp = nchar;
  if(cprompt) lengthp = strlen(cprompt);
  int ncharp = MaximumValue(lengthp, nchar);

  bbox_irega(ident, ncharp, lengthp, icol, nchar);
  dbox_set_generic_trap(-ident, trap);
  dbox_set_cpoint      (-ident, cprompt);
  dbox_set_spoint      (-ident, sprompt);
  dbox_set_generic_trap( ident, trap);
  dbox_set_ipoint      ( ident, ipoint);
  dbox_set_spoint      ( ident, spoint);
}



void wbox_frega(WboxGenericTrap *trap, int ident,
                char  *cprompt, long *sprompt,
                float *fpoint,  long *spoint,
                int icol, int nchar, int ndec)
{
  int lengthp = nchar;
  if(cprompt) lengthp = strlen(cprompt);
  int ncharp = MaximumValue(lengthp, nchar);

  bbox_frega(ident, ncharp, lengthp, icol, nchar, ndec);
  dbox_set_generic_trap(-ident, trap);
  dbox_set_cpoint      (-ident, cprompt);
  dbox_set_spoint      (-ident, sprompt);
  dbox_set_generic_trap( ident, trap);
  dbox_set_fpoint      ( ident, fpoint);
  dbox_set_spoint      ( ident, spoint);
}



void wbox_drega(WboxGenericTrap *trap, int ident,
                char   *cprompt, long *sprompt,
                double *dpoint,  long *spoint,
                int icol, int nchar, int ndec)
{
  int lengthp = nchar;
  if(cprompt) lengthp = strlen(cprompt);
  int ncharp = MaximumValue(lengthp, nchar);

  bbox_drega(ident, ncharp, lengthp, icol, nchar, ndec);
  dbox_set_generic_trap(-ident, trap);
  dbox_set_cpoint      (-ident, cprompt);
  dbox_set_spoint      (-ident, sprompt);
  dbox_set_generic_trap( ident, trap);
  dbox_set_dpoint      ( ident, dpoint);
  dbox_set_spoint      ( ident, spoint);
}



void wbox_crega(WboxGenericTrap *trap, int ident,
                char *cprompt, long *sprompt,
                char *cpoint,  long *spoint,
                int icol, int nchar, int /*ndec*/)
{
  int lengthp = nchar;
  if(cprompt) lengthp = strlen(cprompt);
  int ncharp = MaximumValue(lengthp, nchar);

  int length = nchar;
//if(cpoint) length = strlen(cpoint);

  bbox_crega(ident, ncharp, lengthp, icol, nchar, length);
  dbox_set_generic_trap(-ident, trap);
  dbox_set_cpoint      (-ident, cprompt);
  dbox_set_spoint      (-ident, sprompt);
  dbox_set_generic_trap( ident, trap);
  dbox_set_cpoint      ( ident, cpoint);
  dbox_set_spoint      ( ident, spoint);
}



//------------------ dbox registrations ------------------------------//
//------------------ dbox registrations ------------------------------//
//------------------ dbox registrations ------------------------------//

                   // public.

     // these functions take a pointer to the prompt (if there is one),
     //   but not a pointer to the variable.
     // also, these functions do not take a trap.

     // the prompt must be null-terminated and cannot be NULL.
     // the length of the prompt is obtained from the prompt.
     // the width of the prompt field is obtained from the prompt
     //   or from nchar.
     // the length of a character variable is obtained from nchar.


void dbox_ireg (int ident,
                long *spoint, int irow, int icol, int nchar, int /*ndec*/)
{
  bbox_ireg(ident, irow, icol, nchar);
  dbox_set_spoint      ( ident, spoint);
}



void dbox_freg (int ident,
                long *spoint, int irow, int icol, int nchar, int ndec)
{
  bbox_freg(ident, irow, icol, nchar, ndec);
  dbox_set_spoint      ( ident, spoint);
}



void dbox_dreg (int ident,
                long *spoint, int irow, int icol, int nchar, int ndec)
{
  bbox_dreg(ident, irow, icol, nchar, ndec);
  dbox_set_spoint      ( ident, spoint);
}



void dbox_creg (int ident,
                long *spoint, int irow, int icol, int nchar, int /*ndec*/)
{
  int length = nchar;
  bbox_creg(ident, irow, icol, nchar, length);
  dbox_set_spoint      ( ident, spoint);
}



void dbox_ireg2 (int ident, char *cprompt, long *sprompt,
                long *spoint, int irow, int icol, int nchar, int /*ndec*/)
{
  assert(cprompt);
  int lengthp   = strlen(cprompt);
  int ncharp = lengthp;
  bbox_ireg2(ident, ncharp, lengthp, irow, icol, nchar);
  dbox_set_cpoint      (-ident, cprompt);
  dbox_set_spoint      (-ident, sprompt);
  dbox_set_spoint      ( ident, spoint);
}



void dbox_freg2 (int ident, char *cprompt, long *sprompt,
                long *spoint, int irow, int icol, int nchar, int ndec)
{
  assert(cprompt);
  int lengthp   = strlen(cprompt);
  int ncharp = lengthp;
  bbox_freg2(ident, ncharp, lengthp, irow, icol, nchar, ndec);
  dbox_set_cpoint      (-ident, cprompt);
  dbox_set_spoint      (-ident, sprompt);
  dbox_set_spoint      ( ident, spoint);
}



void dbox_dreg2 (int ident, char *cprompt, long *sprompt,
                long *spoint, int irow, int icol, int nchar, int ndec)
{
  assert(cprompt);
  int lengthp   = strlen(cprompt);
  int ncharp = lengthp;
  bbox_dreg2(ident, ncharp, lengthp, irow, icol, nchar, ndec);
  dbox_set_cpoint      (-ident, cprompt);
  dbox_set_spoint      (-ident, sprompt);
  dbox_set_spoint      ( ident, spoint);
}



void dbox_creg2 (int ident, char *cprompt, long *sprompt,
                long *spoint, int irow, int icol, int nchar, int /*ndec*/)
{
  assert(cprompt);
  int lengthp   = strlen(cprompt);
  int ncharp = lengthp;
  int length = nchar;
  bbox_creg2(ident, ncharp, lengthp, irow, icol, nchar, length);
  dbox_set_cpoint      (-ident, cprompt);
  dbox_set_spoint      (-ident, sprompt);
  dbox_set_spoint      ( ident, spoint);
}



void dbox_ireg3 (int ident, char *cprompt, long *sprompt,
                long *spoint, int irow, int icol, int nchar, int /*ndec*/)
{
  assert(cprompt);
  int lengthp   = strlen(cprompt);
  int ncharp = lengthp;
  bbox_ireg3(ident, ncharp, lengthp, irow, icol, nchar);
  dbox_set_cpoint      (-ident, cprompt);
  dbox_set_spoint      (-ident, sprompt);
  dbox_set_spoint      ( ident, spoint);
}



void dbox_irega(int ident, char *cprompt, long *sprompt,
                long *spoint, int icol, int nchar, int /*ndec*/)
{
  int lengthp = nchar;
  if(cprompt) lengthp = strlen(cprompt);
  int ncharp = MaximumValue(lengthp, nchar);

  bbox_irega(ident, ncharp, lengthp, icol, nchar);
  dbox_set_cpoint      (-ident, cprompt);
  dbox_set_spoint      (-ident, sprompt);
  dbox_set_spoint      ( ident, spoint);
}



void dbox_frega(int ident, char *cprompt, long *sprompt,
                long *spoint, int icol, int nchar, int ndec)
{
  int lengthp = nchar;
  if(cprompt) lengthp = strlen(cprompt);
  int ncharp = MaximumValue(lengthp, nchar);

  bbox_frega(ident, ncharp, lengthp, icol, nchar, ndec);
  dbox_set_cpoint      (-ident, cprompt);
  dbox_set_spoint      (-ident, sprompt);
  dbox_set_spoint      ( ident, spoint);
}



void dbox_drega(int ident, char *cprompt, long *sprompt,
                long *spoint, int icol, int nchar, int ndec)
{
  int lengthp = nchar;
  if(cprompt) lengthp = strlen(cprompt);
  int ncharp = MaximumValue(lengthp, nchar);

  bbox_drega(ident, ncharp, lengthp, icol, nchar, ndec);
  dbox_set_cpoint      (-ident, cprompt);
  dbox_set_spoint      (-ident, sprompt);
  dbox_set_spoint      ( ident, spoint);
}



void dbox_crega(int ident, char *cprompt, long *sprompt,
                long *spoint, int icol, int nchar, int /*ndec*/)
{
  int lengthp = nchar;
  if(cprompt) lengthp = strlen(cprompt);
  int ncharp = MaximumValue(lengthp, nchar);

  int length = nchar;

  bbox_crega(ident, ncharp, lengthp, icol, nchar, length);
  dbox_set_cpoint      (-ident, cprompt);
  dbox_set_spoint      (-ident, sprompt);
  dbox_set_spoint      ( ident, spoint);
}



               //-------- initial registrations ----------//
               //-------- initial registrations ----------//
               //-------- initial registrations ----------//
               //-------- initial registrations ----------//
               //-------- initial registrations ----------//
               //-------- initial registrations ----------//
               //-------- initial registrations ----------//
               //-------- initial registrations ----------//
               //-------- initial registrations ----------//
               //-------- initial registrations ----------//
               //-------- initial registrations ----------//
               //-------- initial registrations ----------//


                             // public.

  // these functions apply only to the current windowbox being
  //   created, and assert if no windowbox is in the process of
  //   being created.


#define DBOX(dbox_set_itrap, WboxIvarTrap, registerIvarTrap)   \
                                                               \
void dbox_set_itrap(int ident, WboxIvarTrap *trap)             \
{                                                              \
  WboxBox *box = get_growing_box();                            \
  box->registerIvarTrap(ident, trap);                          \
}


DBOX(dbox_set_generic_trap  , WboxGenericTrap, registerGenericTrap)
DBOX(dbox_set_fortran_trap  , WboxFortranTrap, registerFortranTrap)
DBOX(dbox_set_simple_trap   , WboxFortranTrap, registerSimpleTrap)
DBOX(dbox_set_ezed_trap     , WboxEzedTrap   , registerEzedTrap)
DBOX(dbox_set_clanguage_trap, WboxClangTrap  , registerClanguageTrap)
DBOX(dbox_set_itrap         , WboxIvarTrap   , registerIvarTrap)
DBOX(dbox_set_ftrap         , WboxFvarTrap   , registerFvarTrap)
DBOX(dbox_set_dtrap         , WboxDvarTrap   , registerDvarTrap)
DBOX(dbox_set_ctrap         , WboxCvarTrap   , registerCvarTrap)

DBOX(dbox_set_npoint        , long           , registerNPoint)
DBOX(dbox_set_nmaxpoint     , long           , registerNmaxPoint)
DBOX(dbox_set_spoint        , long           , registerSwitchPoint)
DBOX(dbox_set_rpoint        , long           , registerRadioPoint)
DBOX(dbox_set_ipoint        , long           , registerIvarPoint)
DBOX(dbox_set_fpoint        , float          , registerFvarPoint)
DBOX(dbox_set_dpoint        , double         , registerDvarPoint)
DBOX(dbox_set_cpoint        , char           , registerCvarPoint)

DBOX(ibox_set_npoint        , int            , registerNPointInt)
DBOX(ibox_set_nmaxpoint     , int            , registerNmaxPointInt)
DBOX(ibox_set_spoint        , int            , registerSwitchPointInt)
DBOX(ibox_set_rpoint        , int            , registerRadioPointInt)
DBOX(ibox_set_ipoint        , int            , registerIvarPointInt)

DBOX(dbox_set_nfun          , WboxNupdateFun , registerNFun)
DBOX(dbox_set_nmaxfun       , WboxNupdateFun , registerNmaxFun)
DBOX(dbox_set_sfun          , WboxIupdateFun , registerSwitchFun)
DBOX(dbox_set_ifun          , WboxIupdateFun , registerIvarFun)
DBOX(dbox_set_ffun          , WboxFupdateFun , registerFvarFun)
DBOX(dbox_set_dfun          , WboxDupdateFun , registerDvarFun)
DBOX(dbox_set_cfun          , WboxCupdateFun , registerCvarFun)

void dbox_set_index_behavior(int ident)
{                                    
  WboxBox *box = get_growing_box();
  box->registerIndexBehavior(ident);
}



            //--------- later registrations --------/
            //--------- later registrations --------/
            //--------- later registrations --------/
            //--------- later registrations --------/
            //--------- later registrations --------/
            //--------- later registrations --------/
            //--------- later registrations --------/
            //--------- later registrations --------/
            //--------- later registrations --------/
            //--------- later registrations --------/
            //--------- later registrations --------/
            //--------- later registrations --------/
            //--------- later registrations --------/


                          // public.

  // these functions take a windowbox pointer.
  // these functions can be called at any time to change registrations.


void wbox_reg_hardcopy (void *box2, WboxHardcopy *hardcopy)
{
  WboxBox *box = (WboxBox*)box2;
  assert(box);
  box->registerHardcopy(hardcopy);
}


#define WBOX(wbox_reg_itrap, WboxIvarTrap, registerIvarTrap)       \
                                                                   \
void wbox_reg_itrap(void *box2, int ident, WboxIvarTrap *trap)     \
{                                                                  \
  WboxBox *box = (WboxBox*)box2;                                   \
  assert(box);                                                     \
  box->registerIvarTrap(ident, trap);                              \
}


WBOX(wbox_reg_fortran_trap  , WboxFortranTrap, registerFortranTrap)
WBOX(wbox_reg_simple_trap   , WboxFortranTrap, registerSimpleTrap)
WBOX(wbox_reg_ezed_trap     , WboxEzedTrap   , registerEzedTrap)
WBOX(wbox_reg_clanguage_trap, WboxClangTrap  , registerClanguageTrap)
WBOX(wbox_reg_itrap         , WboxIvarTrap   , registerIvarTrap)
WBOX(wbox_reg_ftrap         , WboxFvarTrap   , registerFvarTrap)
WBOX(wbox_reg_dtrap         , WboxDvarTrap   , registerDvarTrap)
WBOX(wbox_reg_ctrap         , WboxCvarTrap   , registerCvarTrap)

WBOX(wbox_nnewreg           , long           , registerNPoint)
WBOX(wbox_inewreg           , long           , registerIvarPoint)
WBOX(wbox_fnewreg           , float          , registerFvarPoint)
WBOX(wbox_dnewreg           , double         , registerDvarPoint)
WBOX(wbox_cnewreg           , char           , registerCvarPoint)

WBOX(wbox_reg_npoint        , long           , registerNPoint)
WBOX(wbox_reg_nmaxpoint     , long           , registerNmaxPoint)
WBOX(wbox_reg_spoint        , long           , registerSwitchPoint)
WBOX(wbox_reg_rpoint        , long           , registerRadioPoint)
WBOX(wbox_reg_ipoint        , long           , registerIvarPoint)
WBOX(wbox_reg_fpoint        , float          , registerFvarPoint)
WBOX(wbox_reg_dpoint        , double         , registerDvarPoint)
WBOX(wbox_reg_cpoint        , char           , registerCvarPoint)

WBOX(ibox_reg_npoint        , int            , registerNPointInt)
WBOX(ibox_reg_nmaxpoint     , int            , registerNmaxPointInt)
WBOX(ibox_reg_spoint        , int            , registerSwitchPointInt)
WBOX(ibox_reg_rpoint        , int            , registerRadioPointInt)
WBOX(ibox_reg_ipoint        , int            , registerIvarPointInt)

WBOX(wbox_reg_nfun          , WboxNupdateFun , registerNFun)
WBOX(wbox_reg_nmaxfun       , WboxNupdateFun , registerNmaxFun)
WBOX(wbox_reg_sfun          , WboxIupdateFun , registerSwitchFun)
WBOX(wbox_reg_ifun          , WboxIupdateFun , registerIvarFun)
WBOX(wbox_reg_ffun          , WboxFupdateFun , registerFvarFun)
WBOX(wbox_reg_dfun          , WboxDupdateFun , registerDvarFun)
WBOX(wbox_reg_cfun          , WboxCupdateFun , registerCvarFun)

void wbox_reg_index_behavior(void *box2, int ident)
{                                    
  WboxBox *box = (WboxBox*)box2; 
  assert(box);                     
  box->registerIndexBehavior(ident);
}



//---------------------------- end ------------------------------------//
//---------------------------- end ------------------------------------//
//---------------------------- end ------------------------------------//

