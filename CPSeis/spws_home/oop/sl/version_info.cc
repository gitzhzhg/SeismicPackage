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
// Purpose: Make a PushButton widget to display version info
// Author: Michael L. Sherrill 3/94


#include "sl/sl_base.hh"
#include "sl/version_info.hh"
#include "wproc.h"
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <X11/Intrinsic.h>
#include <stdlib.h>
#include <stdio.h>

#include "sl/paintset_collection.hh"


VersionInfo::VersionInfo(    Widget           p,
                             char             *name,
                             char             *label_strings[100],
                             int              num_strings,
                             Pixel            foreground_pix,
                             Pixel            background_pix)
{
 char *defstr;
 char *infostr;
 int  length = 1;
 int i;  

//////////////////////////// new /////////////////////////////////////
  Arg arglist[22];
  int n=0;
  PaintsetCollection::addResources (arglist, &n, p);

  _vinfow = XtCreateManagedWidget( name, xmPushButtonWidgetClass, p, arglist, n);
//////////////////////////// new /////////////////////////////////////

  for(i=0;i < num_strings; i++)
    length += strlen(label_strings[i]) + 1;

  defstr  = get_simp_labelstrptr( _vinfow );

  if((infostr = (char *)calloc(1,(strlen(defstr)+length) * sizeof(char)))==NULL)
     {
     printf("couldnt allocate string for version widget\n");
     return;
     }

  for(i=0;i < num_strings; i++) {
      strcat(infostr,label_strings[i]);
      strcat(infostr,"\n");
  }

  strcat(infostr,defstr);

  show_msg(_vinfow,infostr);

  XtVaSetValues(_vinfow,  XmNleftAttachment,XmATTACH_POSITION,
                          XmNtopAttachment, XmATTACH_POSITION,
                          XmNleftPosition,  5,
                          XmNtopPosition,   5,
                          XmNforeground,    foreground_pix,
                          XmNbackground,    background_pix,
                          XmNalignment,     XmALIGNMENT_BEGINNING,
                          XmNtraversalOn,   False,
                          XmNshadowThickness,5,NULL);

  setTopWidget(_vinfow);

  XtAddCallback( _vinfow, XmNactivateCallback,
                     (XtCallbackProc)&DoPushCallback, (XtPointer)this );

  free(defstr);
  free(infostr);

}

void VersionInfo::DoPushCallback(Widget /* w */,
                                 XtPointer udata,
                                 XmPushButtonCallbackStruct * /* CBdata */)
{
 VersionInfo *obj = (VersionInfo *)udata;
 //May want to do a delete of this object later when cbyt is c++
 if(XtIsManaged(obj->_vinfow)) obj->unmanage();
}

 VersionInfo::~VersionInfo()
{
}

