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
#include <Xm/Text.h>
#include "sl/sl_form_help_pop.hh"
#include <Xm/Separator.h>
#include "sl/psuedo_widget.hh"

static String  defres[]= {
    NULL,
    ".helpline.marginHeight: 1",
    ".helpline.marginWidth:  1",
    ".helpline.editable:     False",
    ".helpline.editMode:     XmMULTI_LINE_EDIT",
    ".helpline.fontList:     -*-*-bold-r-*-*-*-100-*-*-*-*-*-*",
    NULL };

static char ROWSRESSTR[]=  ".helpline.rows:   %1d";




SLFormHelpPop::SLFormHelpPop(  Widget        p,
                               char          *name,
                               unsigned long buttons,
                               HelpCtx       hctx,
                               Boolean       small_on_dpi,
                               const int     help_rows,
                               Boolean       make_now,
                               const Boolean is_really_popup,
                               const int     num_colors,
                               const int     may_icon,
                               const int     screen_number) :
             SLFPopSep(p,name,buttons,hctx,small_on_dpi,False,
                       is_really_popup,num_colors, may_icon, screen_number),
                       _helpline(NULL)
{
 init(XtDisplay(p), name, help_rows);
 if (make_now) make(p);
}

SLFormHelpPop::SLFormHelpPop(  SLDelay       *contain,
                               char          *name,
                               unsigned long buttons,
                               HelpCtx       hctx,
                               Boolean       small_on_dpi,
                               const int     help_rows,
                               Boolean       make_if_can,
                               const Boolean is_really_popup,
                               const int     num_colors,
                               const int     may_icon,
                               const int     screen_number) :
             SLFPopSep(contain,name,buttons,hctx,small_on_dpi,False,
                       is_really_popup,num_colors, may_icon, screen_number),
                       _helpline(NULL)
{
 init(contain->pW()->display(), name, help_rows);
 if ((contain->made())&&(make_if_can)) make(contain->topWidget());
}

SLFormHelpPop::~SLFormHelpPop()
{
  if (_helpline) removeMsgArea(_helpline);
}

void SLFormHelpPop::init( const Display *dpy, 
                          char          *name, 
                          const int      help_rows)
{
 char rows_res[100];
 sprintf( rows_res, ROWSRESSTR, help_rows );
 defres[0]= rows_res;
 setDefaultResources( dpy, name, defres);
}


Widget SLFormHelpPop::make(Widget p)
{
  if ( !made() ) {
     SLFPopSep::make(p);
     if (_is_really_popup) {
          _helpline= XtVaCreateManagedWidget( "helpline", xmTextWidgetClass,
                                        topWidget(),
                                        XmNrightOffset,      5,
                                        XmNleftOffset,       5,
                                        XmNrightAttachment,  XmATTACH_FORM,
                                        XmNleftAttachment,   XmATTACH_FORM,
                                        XmNbottomAttachment, XmATTACH_WIDGET,
                                        XmNbottomOffset,     2,
                                        XmNbottomWidget,     SLFPopSep::bottomSeparator(), 
                                        NULL );
          addMsgArea(_helpline);

          _low2sep= XtVaCreateManagedWidget( "low2sep", xmSeparatorWidgetClass, 
                                        topWidget(),
                                        XmNrightOffset,      5,
                                        XmNleftOffset,       5,
                                        XmNrightAttachment,  XmATTACH_FORM,
                                        XmNleftAttachment,   XmATTACH_FORM,
                                        XmNbottomAttachment, XmATTACH_WIDGET,
                                        XmNbottomOffset,     2,
                                        XmNbottomWidget,     _helpline, NULL );
     } // end if

  }
  return topWidget();
}

Widget SLFormHelpPop::bottomSeparator()
{ 
   if (isDialog()) return _low2sep;
   else            return topWidget();
}
