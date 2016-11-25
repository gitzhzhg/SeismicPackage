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
#ifndef WPROCFORMPOP_H
#define WPROCFORMPOP_H
//#include <stdio.h>
#include <Xm/Form.h>
#include "sl/sl_shell_container.hh"

typedef void SLFormCfunc(void*, long);
typedef SLFormCfunc *SLFormCfuncPtr;

class SLpPush;
class SLBB;

typedef struct _ExtraElement {
                            SLpPush         *obj;
                            SLFormCfuncPtr  func;   // not used currently
                            void            *data;  // not used currently
                          } ExtraElement, *ExtraAry;



#ifdef FP_DOOK
#undef FP_DOOK
#endif
#ifdef FP_DOCANCEL
#undef FP_DOCANCEL
#endif
#ifdef FP_DOAPPLY
#undef FP_DOAPPLY
#endif
#ifdef FP_DOHELP
#undef FP_DOHELP
#endif
 /*
  * -----------------------------------------------------------------------
  * these constants specify which button was pushed in a form popup
  * in the activate callback.  It is also the ident for each button;
  * They are defined are in the parent class.
  * -----------------------------------------------------------------------
  */
 /*
  *      #define FP_OK         0
  *      #define FP_CANCEL     1
  *      #define FP_APPLY      2
  *      #define FP_HELP       3
  *      #define FP_REMOVE     4
  *      #define FP_OTHER      5
  */
 /*
  * -----------------------------------------------------------------------
  * the following flags are sent to the form popup constructor to
  * specify which buttons to create.  The can be bit or'ed together.
  * -----------------------------------------------------------------------
  */
#define FP_DOOK        (1<<0)
#define FP_DOCANCEL    (1<<1)
#define FP_DOAPPLY     (1<<2)
#define FP_DOHELP      (1<<3)
#define FP_DOREMOVE    (1<<0x8)
#define FP_DOALL       0xFF       // only - OK APPLY CANCEL HELP
#define FP_DOSTANDARD  0xFF       // only - OK APPLY CANCEL HELP



class SLFormPop : public SLShellContainer {
  private:
// ------- new
     static void cntrlResize( Widget w, XtPointer, XEvent *);
     static void startResize( Widget w, XtPointer, XtPointer );
//   static void doDestroy( Widget w,   XtPointer, XmAnyCallbackStruct *cbs);
// ------- new

     char            *_overview_str;
  protected:
     virtual void    applyButton();
     virtual void    removeButton();
     virtual void    okButton();
     virtual void    cancelButton();
     virtual void    helpButton();
     virtual void    extraButton(int ident);
     virtual void    DoAction();
     virtual Boolean ValidInput();
     virtual void    UndoInput();
     virtual void    closing(){ cancelButton();}
     virtual void    managing();
     void   loadPopSysDefaults(String xdefres[], Boolean do_method);
     void            new_and_copy();
     void            disButtons();
     Widget          crePopup(Widget p);
     SLpPush*        addStandardButton( int          ident,
                                        unsigned int flag,
                                        char         *name);
     virtual Boolean notify(SLPrim *obj);
     void           init( const Display *dpy);
     Boolean        canIAlloc(int number, 
                              int num_planes, 
                              Widget w =NULL);

     SLBB            *_button_ctl;             // container for the buttons
     SLpPush         *_ok, *_can, *_app, *_help, *_remove;// button at bottom
     ExtraAry        _extra_push_ary;
     int             _num_extra_buttons;
     int             _total_buttons;
     unsigned long   _buttons;            // flags for button to create
     Boolean         _small_on_dpi;       // flag to reduce size on <75 dpi
     Boolean         _default_button_ok; 
     Boolean         _is_really_popup;    // true if popup, flase if just form
     SLFormCfuncPtr  _completionFunc;
     void           *_completionData;
     int            _last_button;
     

  public:
     SLFormPop(  const Widget        p, 
                 char                *name, 
                 const unsigned long buttons, 
                 const HelpCtx       hctx,
                 const Boolean       small_on_dpi    =True,
                 const Boolean       make_now        =True,
                 const Boolean       is_really_popup =True,
                 const int           num_colors      =0,
                 const int           may_icon        =UseResource,
                 const int           screen_number   =UseResource);

     SLFormPop(  SLDelay             *contain,
                 char                *name, 
                 const unsigned long buttons, 
                 const HelpCtx       hctx,
                 const Boolean       small_on_dpi    =True,
                 const Boolean       make_if_can     =True,
                 const Boolean       is_really_popup =True,
                 const int           num_colors      =0,
                 const int           may_icon        =UseResource,
                 const int           screen_number   =UseResource);

     SLFormPop(  const PsuedoWidget  *pw,
                       char          *name, 
                 const unsigned long buttons, 
                 const HelpCtx       hctx,
                 const Boolean       small_on_dpi    =True);
     void setCompletionFunc( SLFormCfuncPtr f =NULL, void *p =NULL)
                         { _completionFunc= f; _completionData= p; };
     virtual ~SLFormPop();
//   void update() {};  // 3/29/96 KC killed when updateChildren() not called
     virtual Widget make(Widget p =NULL);
     virtual WidgetClass topClass() { return(xmFormWidgetClass); };
     virtual Boolean isDialog();
     virtual void reloadSystemDefaults(Boolean do_method =True)
                                                      {do_method=True;}
     int whichButton() { return _last_button;}
     void defaultButtonOK(Boolean set =True);
     void defaultButton(int ident= FP_OK, Boolean set =True);
     void addExtraButton(char *name, const int ident);
     Widget buttonContainer();
     Widget getWidget(int ident);
     Colormap cMap() {return _cmap;}
     Boolean isPrivateCmap();
};




class SLFPopSep : public SLFormPop {
  protected:
     Widget  _lowsep;                // separator above buttons
  public:
     SLFPopSep(  Widget        p, 
                 char          *name, 
                 unsigned long buttons, 
                 HelpCtx       hctx,
                 Boolean       small_on_dpi    =True,
                 Boolean       make_now        =True,
                 const Boolean is_really_popup =True,
                 const int     num_colors      =0,
                 const int     may_icon        =UseResource,
                 const int     screen_number   =UseResource);
     SLFPopSep(  SLDelay       *contain,
                 char          *name, 
                 unsigned long buttons, 
                 HelpCtx       hctx,
                 Boolean       small_on_dpi    =True,
                 Boolean       make_if_can     =True,
                 const Boolean is_really_popup =True,
                 const int     num_colors      =0,
                 const int     may_icon        =UseResource,
                 const int     screen_number   =UseResource);
     SLFPopSep(  PsuedoWidget  *pw,
                 char          *name, 
                 unsigned long buttons, 
                 HelpCtx       hctx,
                 Boolean       small_on_dpi    =True);
     virtual Widget bottomSeparator();
     virtual Widget make(Widget p);
};

#endif
