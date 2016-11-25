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
#include "sl/sl_option_menu.hh"
#include "sl/sl_font_select.hh"
#include "sl/psuedo_widget.hh"
#include "cprim.h"
#include <ctype.h>
#include <Xm/Label.h>



static String  defres[]= {
    "*famlab.labelString:   Font:",
    "*ptlab.labelString:    Point Size:",
    NULL };


#define FONTSTR     "-*-%s-bold-r-normal-*-*-%1d-*-*-*-*-iso8859-1"
#define FONTSTR_FAM "-*-%s-bold-r-normal-*-*-*-*-*-*-*-iso8859-1"

static int get_pt(char *font);
static char *get_family(char *font);
static int cmp(const void *i1, const void *i2);
static void addPt( const int pt, int plist[], int &cnt);




SLFontSelect::SLFontSelect(const Widget  p,
                           const char    *name,
                           const HelpCtx hctx,
                           const Boolean doframe,
                           const Boolean make_now ) : 
           SLForm(p,(char*)name,hctx,doframe,False), _pt_cnt(0), _fam_cnt(0)
{
  supportUnmadeDefaults(p);
  init(XtDisplay(p));
  if (make_now) make(p);
}



SLFontSelect::SLFontSelect(SLDelay *contain,
                           const char    *name,
                           const Boolean doframe,
                           const Boolean make_if_can) :
           SLForm(contain,(char*)name,NULL,doframe,False),
                     _pt_cnt(0), _fam_cnt(0)
{

  supportUnmadeDefaults(contain);
  init((contain->pW())->display());
  if ((contain->made())&&(make_if_can)) make(contain->topWidget());
}

SLFontSelect::~SLFontSelect()
{
  delete _pt_option; 
  delete _fam_option;
}

void SLFontSelect::init(const Display *dpy)
{
  char **fonts;
  int  i, j, cnt;
  char wkstr[30];
  SLPush *pt_pushes;
  SLPush *fam_pushes;

  setDefaultResources( dpy, _name, defres);
  fonts= XListFonts((Display*)dpy, 
                     "-*-*-bold-r-normal-*-*-*-*-*-*-*-iso8859-1", 
                     1000, &cnt);
  
  scalablePtSizes(); 
  for(i=0; (i<cnt); i++) {
     addPt( get_pt(fonts[i]) ,_pt_list,_pt_cnt);
     addFam( get_family(fonts[i]) );
  }
  qsort( (void*)_pt_list, _pt_cnt, sizeof(int), cmp );

  pt_pushes=  (SLPush*)calloc(_pt_cnt, sizeof(SLPush) );
  fam_pushes= (SLPush*)calloc(_fam_cnt, sizeof(SLPush) );

  for(i=0, j=0; (i<_pt_cnt); i++) {
       if (_pt_list[i] != 0) {
          sprintf(wkstr, "%2d", _pt_list[i]/10);
          pt_pushes[j].name= newstr(wkstr);
          pt_pushes[j].ident= _pt_list[i];
          j++;
       }
  }

  for(i=0; (i<_fam_cnt); i++) {
       fam_pushes[i].name= newstr(_fam_list[i]);
       fam_pushes[i].ident= i;
  }

  _pt_option= new SLOptionMenu(this, "pt_option", getHelpCtx(), pt_pushes,  
                   (_pt_list[0]==0 ? _pt_cnt-1 :  _pt_cnt), NULL, False, False);
  _fam_option= new SLOptionMenu(this, "fam_option", getHelpCtx(), fam_pushes,  
                                _fam_cnt, NULL, False, False);
  _fam_option->setAltPushAction(famChange, this);
  _pt_option->setAltPushAction(ptChange, this);
   
  for(i=0; (i<_pt_cnt); i++) {
       if (pt_pushes[i].name) free(pt_pushes[i].name);
  }
  for(i=0; (i<_fam_cnt); i++) {
      if (fam_pushes[i].name) free(fam_pushes[i].name);
      //printf("_fam_list[%2d]= %s\n", i, _fam_list[i]);
  }
  free(pt_pushes);
  free(fam_pushes);
  XFreeFontNames(fonts);

 
}

void SLFontSelect::scalablePtSizes()
{
  int i, j;
  for (i=0, j=100; (i<12); i++, j+=20)
         _pt_list[i]= j;
  _pt_list[i++]= 360;
  _pt_list[i++]= 400;
  _pt_list[i++]= 500;
  _pt_list[i++]= 600;
  _pt_list[i++]= 720;
  _pt_list[i++]= 800;
  _pt_cnt= i;
}




Widget SLFontSelect::make(Widget p)
{

  if ( made() ) return topWidget();
  SLForm::make(p);
  p= wParent();
  makeChildren();
  _famlab= XtVaCreateManagedWidget("famlab", xmLabelWidgetClass, 
                                   topWidget(), NULL); 
  _ptlab=  XtVaCreateManagedWidget("ptlab", xmLabelWidgetClass, topWidget(),  
                                    XmNtopAttachment,  XmATTACH_WIDGET,
                                    XmNtopWidget,      _famlab,
                                    XmNtopOffset,      15,
                                    NULL); 
  XtVaSetValues( _fam_option->W(),  XmNtopAttachment,  XmATTACH_OPPOSITE_WIDGET,
                                    XmNtopWidget,      _famlab,
                                    XmNleftAttachment, XmATTACH_WIDGET,
                                    XmNleftWidget,     _ptlab,
                                    NULL );
  XtVaSetValues( _pt_option->W(),  XmNtopAttachment,  XmATTACH_OPPOSITE_WIDGET,
                                   XmNtopWidget,      _ptlab,
                                   XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                                   XmNleftWidget,     _fam_option->W(),
                                   NULL );

  famChange((void*)this, 0);
  /*
   * find a good size initial point size - around 18
   */
  for(int i=0, found= False; ((i<_curr_pt_cnt)&&(!found)); i++) {
        if ( (_curr_pt_list[i] > 170) && (_curr_pt_list[i] < 310) ) {
               found= True;
               _pt_option->setButton(_curr_pt_list[i]);
        }
  }
  return topWidget();
}




static char *get_family(char *font)
{
   static char retfam[200];
   char numstr[20];
   int i=0, loc=0;

   numstr[0]= '\0';

   for(;(loc<2)&&(*font); font++ ) {
         if (*font == '-') loc++;
   }
   for(i=0; (*font != '-');font++ )
       retfam[i++]= *font;
   retfam[i]= '\0';
 
   return retfam;
}

static int get_pt(char *font)
{
   char numstr[20];
   int i=0, loc=0;
   int retval;

   numstr[0]= '\0';

   for(;(loc<8)&&(*font); font++ ) {
         if (*font == '-') loc++;
   }
   for(; isdigit(*font);font++ )
       numstr[i++]= *font;

   numstr[i]= '\0';
 
   if (strlen(numstr))
       retval= (int)strtol(numstr,NULL,0);
   else
       retval= 0;

   return retval;
}

static int cmp(const void *v1, const void *v2)
{
  int i1= *(int*)v1;
  int i2= *(int*)v2;

  if ( i1 == i2) return  0;
  if ( i1 <  i2) return -1;
  if ( i1 >  i2) return  1;
  return 0;
}

static void addPt( const int pt, int plist[], int &cnt)
{
  Boolean found= False;

  for(int i=0; ((i<cnt)&&(!found)); i++) {
        if (pt == plist[i]) found= True; 
  }
  if (!found) plist[cnt++]= pt;
}


void SLFontSelect::addFam( const char *fam)
{
  Boolean found= False;

  for(int i=0; ((i<_fam_cnt)&&(!found)); i++) {
        if (strcmp(fam,_fam_list[i]) == 0) found= True; 
  }
  if (!found)  strcpy(_fam_list[_fam_cnt++], fam);
}


int SLFontSelect::pointSize() const
{
 return _pt_option->whichSelected();
}

const char *SLFontSelect::familyName() const
{
  return ( _fam_list[_fam_option->whichSelected()] );
}


char *SLFontSelect::selectedFont() const
{
  char newfontstr[200];
  char *retstr;
  int  cnt;
  char **fonts;
  
  // comment
  sprintf(newfontstr, FONTSTR, familyName(), pointSize() );
  fonts= XListFonts( XtDisplay(topWidget()), newfontstr, 1000, &cnt);
  
  retstr= newstr(fonts[0]);
  XFreeFontNames(fonts);
  return (retstr);
}


void SLFontSelect::ptChange(void *,long )
{
}



void SLFontSelect::famChange(void *data,long)
{
  SLFontSelect *obj= (SLFontSelect*)data;
  obj->changePtList();
}


void SLFontSelect::changePtList()
{
  char newfontstr[200];
  char **fonts;
  int cnt, i, j;
  Boolean found;


  // see if we have a scalable font in this family
  sprintf(newfontstr, FONTSTR, familyName(), 0 );
  fonts= XListFonts(XtDisplay(topWidget()), newfontstr, 1000, &cnt);

  if ( fonts ) {   // scalable fonts on R5 or more system
     XFreeFontNames(fonts);
     for(i=0; (i<_pt_cnt); i++) 
           XtManageChild( _pt_option->buttonW(_pt_list[i]));
     _curr_pt_cnt= _pt_cnt;
     for(i=0; (i<_pt_cnt); i++) _curr_pt_list[i]= _pt_list[i];
  }
  else {
     _curr_pt_cnt= 0;
     sprintf(newfontstr, FONTSTR_FAM, familyName() );
     fonts= XListFonts(XtDisplay(topWidget()), newfontstr, 1000, &cnt);
     XFreeFontNames(fonts);
     for(i=0; (i<cnt); i++) {
        addPt( get_pt(fonts[i]),_curr_pt_list,_curr_pt_cnt);
     }
     qsort( (void*)_curr_pt_list, _curr_pt_cnt, sizeof(int), cmp );
     for(i=0; (i<_pt_cnt); i++) 
           XtUnmanageChild( _pt_option->buttonW(_pt_list[i]));
     for(i=0, j=0; (j<_curr_pt_cnt); j++) {
        for(; ( (_pt_list[i] <= _curr_pt_list[j]) && (i<_pt_cnt) ); i++) {
              if ( _pt_list[i] == _curr_pt_list[j] )
                     XtManageChild( _pt_option->buttonW(_pt_list[i]) );
        }
     }
     for(i=0, found= False; (i<_curr_pt_cnt); i++)
        if (_pt_option->whichSelected() == _curr_pt_list[i]) found= True;
     if (!found) _pt_option->setButton(_curr_pt_list[_curr_pt_cnt-1]);
  }

}
