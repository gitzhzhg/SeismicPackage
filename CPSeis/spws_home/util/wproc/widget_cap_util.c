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

#include "x_util_rd.h"

/****************************************************************/
/*                                                              */
/*  XutilTopLevel                                               */
/*    This functions returns the top level widget of a widget,  */
/*    which is the widget whose parent is a shell widget. If the*/
/*    widget is a shell, then the function returns the widget.  */
/****************************************************************/
Widget XutilTopLevel(Widget widget)
{
  if (XtIsShell(widget)) return(widget);
 
  while (!XtIsShell(XtParent(widget))) widget = XtParent(widget);

  return(widget);
}

/****************************************************************/
/*                                                              */
/*  XutilScreenNumber                                           */
/*    This functions returns the screen number of a widget      */
/*                                                              */
/****************************************************************/
int XutilScreenNumber(Widget widget)
{
  return(DefaultScreen(XtDisplay(widget)));
}

/****************************************************************/
/*                                                              */
/*  XutilGetShell                                               */
/*    This functions returns the shell widget of a widget.      */
/*                                                              */
/****************************************************************/
Widget XutilGetShell(Widget widget)
{
  if (XtIsShell(widget)) return(widget);
 
  while (!XtIsShell(widget)) widget = XtParent(widget);

  return(widget);
}

/****************************************************************/
/*                                                              */
/*  XutilWidgetHasChild                                         */
/*    This function returns True if the widget is composite and */
/*    has a child.                                              */
/*                                                              */
/****************************************************************/
Boolean XutilWidgetHasChild(CompositeWidget widget)
{
  if (!XtIsComposite(widget)) return(False);
 
  if (widget->composite.num_children > 0) 
    return(True);
  else
    return(False);
}

/****************************************************************/
/*                                                              */
/*  XutilWidgetColormap                                         */
/*   to return the colormap of a widget.                        */
/*                                                              */
/****************************************************************/
Colormap XutilWidgetColormap(Widget widget)
{
  return(widget->core.colormap);
}

/****************************************************************/
/*                                                              */
/*  XutilWidgetWidth                                            */
/*  XutilWidgetHeight                                           */
/*  XutilWidgetX                                                */
/*  XutilWidgetY                                                */
/*    These functions return the width, height, x or y          */
/*    coordinate of a widget.                                   */
/****************************************************************/
Dimension XutilWidgetWidth(Widget widget)
{
  return(widget->core.width);
}

Dimension XutilWidgetHeight(Widget widget)
{
  return(widget->core.height);
}

Position XutilWidgetX(Widget widget)
{
  return(widget->core.x);
}

Position XutilWidgetY(Widget widget)
{
  return(widget->core.y);
}

/****************************************************************/
/*                                                              */
/*  XmSetWidgetUserData                                         */
/*  XmGetUSerData                                               */
/*    These functions set and get the user data field in the    */
/*    manager part of a widget. (won't work for a gadget)       */
/****************************************************************/

/*
void XmSetWidgetUserData(XmPrimitiveWidget widget,caddr_t data)
{
  widget->primitive.user_data = data;
}

caddr_t XmGetWidgetUserData(XmPrimitiveWidget widget)
{
  return(widget->primitive.user_data);
}

*/
