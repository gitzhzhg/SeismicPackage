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
#include "sl/psuedo_widget.hh"
#include "sl/slp_file.hh"
#include <Xm/Text.h>
#include <Xm/ToggleB.h>
#include <Xm/List.h>
#include <Xm/Scale.h>
#include <stdarg.h>


PsuedoWidget::PsuedoWidget(const Widget w)
   : _w(NULL), _istr(NULL), _cstr(NULL)
{
 _w= w;
 _istr= "";
 _cstr= "";
}

PsuedoWidget::PsuedoWidget(const Widget w, ...) 
   : _w(NULL), _istr(NULL), _cstr(NULL)
{
 va_list   args;
 char      *name;
 WidgetClass classptr;
 char iwkstr[1000];
 char cwkstr[1000];

 _w= w;
#ifdef __GNUC__
 //va_start(((va_list)args), w);
 va_start(args, w);
#else
 va_start(args, w);
#endif


 iwkstr[0]= '\0';
 cwkstr[0]= '\0';
 
 for( name= va_arg(args, char*); (name); name= va_arg(args, char*) ) {

     classptr= va_arg(args, WidgetClass);

     strcat(iwkstr, ".");
     strcat(iwkstr, name);
     strcat(cwkstr, "." );
     strcat(cwkstr, classptr_name(classptr) );
 }
     
 _istr= newstr(iwkstr);
 _cstr= newstr(cwkstr);

 va_end(args);

}

PsuedoWidget::PsuedoWidget( const PsuedoWidget *pw, 
                                  char         *name, 
                            const WidgetClass classptr)
   : _w(NULL), _istr(NULL), _cstr(NULL)

{
 _w= pw->_w;
 if (pw->_istr)
        _istr= newstrcat( pw->_istr, ".", name, NULL);
 else
        _istr= newstr( name);

 if (pw->_cstr)
        _cstr= newstrcat( pw->_cstr, ".", classptr_name(classptr) , NULL);
 else
        _cstr= newstr(  classptr_name(classptr) );
}

PsuedoWidget::PsuedoWidget (const PsuedoWidget *pw, char *name,
  const SLDelay *classptr)
{
  _w = pw->_w;
  if (pw->_istr) {
    _istr = newstrcat (pw->_istr, ".", name, NULL);
  }
  else {
    _istr = newstr (name);
  }

  if (pw->_cstr) {
    _cstr = newstrcat (pw->_cstr, ".", classptr->instanceName() , NULL);
  }
  else {
    _cstr = newstr (classptr->instanceName());
  }
}

PsuedoWidget::~PsuedoWidget()
{
 if (_istr) free (_istr);
 if (_cstr) free (_cstr);
}

Boolean PsuedoWidget::togDef() const
{
 char instance_res[1000];
 char class_res[1000];
 char c_wstr[800];
 char i_wstr[800];
 Boolean retval= False;
 char *res;
 

 make_wctree( _w, i_wstr, c_wstr, wprocALL);
 
 sprintf(instance_res, "%s%s.set", i_wstr, _istr);
 sprintf(class_res, "%s%s.Set", c_wstr, _cstr);

 res= DefGetStrValue( XtDisplay(_w), instance_res, class_res);

 if (res) {
     strcpy(instance_res, res);
     retval= (strcmp( strToLower(instance_res),"true") == 0) ? True : False;
 }

 return (retval);
}



Boolean PsuedoWidget::childTogDef(char *name) const
{
   PsuedoWidget pw( this, name, xmToggleButtonWidgetClass);
   return ( pw.togDef() );
}


#define OTHER_SCREEN_STR "otherscreen"

int PsuedoWidget::formScreenDef() const
{
  char instance_res[1000];
  char class_res[1000];
  char c_wstr[800];
  char i_wstr[800];
  short retval= -1;
  char *res;
  char *lower_res;
 

  make_wctree (_w, i_wstr, c_wstr, wprocALL);
 
  sprintf (instance_res, "%s%s.screenNumber", i_wstr, _istr);
  sprintf (class_res, "%s%s.ScreenNumber", c_wstr, _cstr);

  res = DefGetStrValue (XtDisplay(_w), instance_res, class_res);

  if (res) {
    lower_res = strToLower (newstr(res));
    if (strcmp(lower_res,OTHER_SCREEN_STR) == 0) { 
      retval = OtherScreen; 
    }
    else if (!check_sint(res, &retval)) {
      retval = NoScreenResource;
    } // end if check_sint
    free (lower_res);
  } // end if res
  return (int)retval;
}



int PsuedoWidget::scaleDef() const
{
  char instance_res[1000];
  char class_res[1000];
  char c_wstr[800];
  char i_wstr[800];
  short retval = 0;
  char *res;
 

  make_wctree (_w, i_wstr, c_wstr, wprocALL);
 
  sprintf (instance_res, "%s%s.value", i_wstr, _istr);
  sprintf (class_res, "%s%s.Value", c_wstr, _cstr);

  res = DefGetStrValue (XtDisplay(_w), instance_res, class_res);

  if (res) {
    if (!check_sint(res,&retval)) retval = 0;
  }
  return (int)retval;
}



int PsuedoWidget::childScaleDef(char *name) const
{
   PsuedoWidget pw( this, name, xmScaleWidgetClass);
   return (pw.scaleDef());
}


char *PsuedoWidget::childOptionDef(char *name) const
{
  return childTextDef(name);
}

char *PsuedoWidget::optionDef() const
{
  return textDef();
}

Boolean PsuedoWidget::anyBooleanDef(const char *inst_res_str,
                                    const char *class_res_str) const
{
 char instance_res[1000];
 char class_res[1000];
 char c_wstr[800];
 char i_wstr[800];
 Boolean retval= False;
 char *res;

 make_wctree( _w, i_wstr, c_wstr, wprocALL);
 
 sprintf(instance_res, "%s%s.%s", i_wstr, _istr, inst_res_str);
 sprintf(class_res, "%s%s.%s", c_wstr, _cstr, class_res_str);

 res= DefGetStrValue( XtDisplay(_w), instance_res, class_res);

 if (res) {
     strcpy(instance_res, res);
     retval= (strcmp( strToLower(instance_res),"true") == 0) ? True : False;
 }

 return (retval);
}


int PsuedoWidget::anyIntDef(const char *inst_res_str,
                           const char *class_res_str) const
{
  char instance_res[1000];
  char class_res[1000];
  char c_wstr[800];
  char i_wstr[800];
  short retval= 0;
  char *res;

  make_wctree (_w, i_wstr, c_wstr, wprocALL);
 
  sprintf (instance_res, "%s%s.%s", i_wstr, _istr, inst_res_str);
  sprintf (class_res, "%s%s.%s", c_wstr, _cstr, class_res_str);

  res = DefGetStrValue (XtDisplay(_w), instance_res, class_res);

  if (res) {
    if (!check_sint(res,&retval)) retval= 0;
  }
  return (int)retval;
}



char *PsuedoWidget::anyDef(const char *inst_res_str,
                           const char *class_res_str) const
{
 char instance_res[1000];
 char class_res[1000];
 char c_wstr[800];
 char i_wstr[800];
 char *res;
 

 make_wctree( _w, i_wstr, c_wstr, wprocALL);
 
 sprintf(instance_res, "%s%s.%s", i_wstr, _istr, inst_res_str);
 sprintf(class_res, "%s%s.%s", c_wstr, _cstr, class_res_str);

 res= DefGetStrValue( XtDisplay(_w), instance_res, class_res);

 return (res);
}




char *PsuedoWidget::textDef() const
{
 char instance_res[1000];
 char class_res[1000];
 char c_wstr[800];
 char i_wstr[800];
 char *res;
 

 make_wctree( _w, i_wstr, c_wstr, wprocALL);
 
 sprintf(instance_res, "%s%s.value", i_wstr, _istr);
 sprintf(class_res, "%s%s.Value", c_wstr, _cstr);

 res= DefGetStrValue( XtDisplay(_w), instance_res, class_res);

 return (res);
}

char *PsuedoWidget::childTextDef(char *name) const
{
  PsuedoWidget pw(this, name, xmTextWidgetClass );
  return (pw.textDef());
}



char *PsuedoWidget::fileChoiceDef() const
{
 char instance_res[1000];
 char class_res[1000];
 char c_wstr[800];
 char i_wstr[800];
 char *res;
 

 make_wctree( _w, i_wstr, c_wstr, wprocALL);
 
 sprintf(instance_res, "%s%s.text.value", i_wstr, _istr);
 sprintf(class_res, "%s%s.Text.Value", c_wstr, _cstr);

 res= DefGetStrValue( XtDisplay(_w), instance_res, class_res);

 return (res);
}

/*
char *PsuedoWidget::childFileChoiceDef(char *name) const
{

  PsuedoWidget *pw;
  char *str;

  pw= new PsuedoWidget( this, name, fileChoiceWidgetClass );
  str= pw->fileChoiceDef();
  delete pw;

  return (str);
}
*/

char *PsuedoWidget::childFileChoiceDef(char *name) const
{
  SLpFile *file = new SLpFile (_w, name);
  PsuedoWidget pw (this, name, file);
  delete file;
  return (pw.fileChoiceDef());
}




XmStringTable PsuedoWidget::itemsDef() const
{
 char instance_res[1000];
 char class_res[1000];
 char c_wstr[800];
 char i_wstr[800];
 char *res;
 Boolean stat;
 static XrmValue to_desc ={0,NULL};
 static XrmValue from_desc ={2000,NULL};
 XmStringTable retval;
 

 make_wctree( _w, i_wstr, c_wstr, wprocALL);
 
 sprintf(instance_res, "%s%s.items", i_wstr, _istr);
 sprintf(class_res, "%s%s.Items", c_wstr, _cstr);

 res= DefGetStrValue( XtDisplay(_w), instance_res, class_res);
 from_desc.addr= res;
 if (res) {
    from_desc.size= strlen(res) +1;
    stat= XtConvertAndStore(_w, XmRString, &from_desc, 
                            XmRXmStringTable, &to_desc);
    if (!stat) to_desc.addr= NULL;
 }

 if (to_desc.addr) 
       retval= *(XmStringTable*)to_desc.addr;
 else 
       retval= (XmStringTable)NULL;

 return retval;
}


XmStringTable PsuedoWidget::childItemsDef(char *name) const
{
  PsuedoWidget pw(this,name,xmListWidgetClass);
  return ( pw.itemsDef() );
}


XmStringTable PsuedoWidget::selectedItemDef() const
{
 char instance_res[1000];
 char class_res[1000];
 char c_wstr[800];
 char i_wstr[800];
 char *res;
 Boolean stat;
 static XrmValue to_desc ={0,NULL};
 static XrmValue from_desc ={2000,NULL};
 XmStringTable retval;
 

 make_wctree( _w, i_wstr, c_wstr, wprocALL);
 
 sprintf(instance_res, "%s%s.selectedItems", i_wstr, _istr);
 sprintf(class_res, "%s%s.SelectedItems", c_wstr, _cstr);

 res= DefGetStrValue( XtDisplay(_w), instance_res, class_res);
 from_desc.addr= res;
 if (res) {
    from_desc.size= strlen(res) +1;
    stat= XtConvertAndStore(_w, XmRString, &from_desc, 
                            XmRXmStringTable, &to_desc);
    if (!stat) to_desc.addr= NULL;
 }

 if (to_desc.addr) 
       retval= *(XmStringTable*)to_desc.addr;
 else 
       retval= (XmStringTable)NULL;

 return retval;
}



XmStringTable PsuedoWidget::childSelectedItemDef(char *name) const
{
  PsuedoWidget pw(this,name,xmListWidgetClass);
  return ( pw.selectedItemDef() );
}




int PsuedoWidget::listCountDef() const
{
  char instance_res[1000];
  char class_res[1000];
  char c_wstr[800];
  char i_wstr[800];
  short retval = 0;
  char *res;
 
  make_wctree (_w, i_wstr, c_wstr, wprocALL);
 
  sprintf (instance_res, "%s%s.itemCount", i_wstr, _istr);
  sprintf (class_res, "%s%s.ItemCount", c_wstr, _cstr);

  res = DefGetStrValue (XtDisplay(_w), instance_res, class_res);

  if (res) {
    if (!check_sint(res,&retval)) retval= 0;
  }
  return (int)retval;
}


int PsuedoWidget::childListCountDef(char *name) const
{
  PsuedoWidget pw(this,name,xmListWidgetClass);
  return ( pw.listCountDef() );
}


int PsuedoWidget::selectedCountDef() const
{
  char instance_res[1000];
  char class_res[1000];
  char c_wstr[800];
  char i_wstr[800];
  short retval= 0;
  char *res;
 
  make_wctree (_w, i_wstr, c_wstr, wprocALL);
 
  sprintf (instance_res, "%s%s.selectedItemCount", i_wstr, _istr);
  sprintf (class_res, "%s%s.SelectedItemCount", c_wstr, _cstr);

  res = DefGetStrValue (XtDisplay(_w), instance_res, class_res);

  if (res) {
    if (!check_sint(res,&retval)) retval= 0;
  }
  return (int)retval;
}


int PsuedoWidget::childSelectedCountDef(char *name) const
{
  PsuedoWidget pw(this,name,xmListWidgetClass);
  return ( pw.selectedCountDef() );
}

