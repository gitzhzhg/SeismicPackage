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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/Scale.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeBG.h>
#include "file_choice.h"
#include "cprim.h"



#if ( (_AIX) || (__hpux) || (unix) || defined(_POSIX_SOURCE))
#define SEPCHAR '/'
#elif ((DOS) || (WINNT))
#define SEPCHAR '/'
#endif


#define TEXTRES   ".value: "
#define TEXTFRES  ".value: "
#define TOGRES    ".set: "
#define SCALERES  ".value: "
#define OPTIONRES ".value: "
#define FILECRES  ".filename: "


#define TRUESTR  "True"
#define FALSESTR "False"

#define OPTIONERR "DefSaveWidget: No cascade button found in option menu\n"


#define UDESC "-userdefs.ad"


struct DefaultInfo {
            FILE *fp;
            int  level;
                   };


static void loadres( Display *dpy, char *filename);
static char *determine_file(Widget w, char *filename);
static void retw(Widget w, void *data);
static void retw_load(Widget w, void *data);
static FILE *do_open( char *fname, char *how);
static void standard_blurb(DefInfo def);



/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void DefSaveWidget( DefInfo def, 
                    Widget w )
{
  char wstr[500];
  char *valuestr= NULL;
  char *resstr= NULL;
  char *outstr= NULL;
  Boolean free_val= False;
  Boolean succ= True;
 
  if ( (w) && (def) ) {

     make_wtree(w, wstr, def->level);


     if ( XtClass(w) == xmTextFieldWidgetClass) {
          Boolean is_editable;
          XtVaGetValues(w, XmNeditable, &is_editable, NULL);
          if (is_editable) {
                 valuestr= XmTextFieldGetString( w);
                 free_val= True;
                 resstr= TEXTRES;
          }
          else   succ= False;
     }

     else if ( XtClass(w) == xmTextWidgetClass) {
          Boolean is_editable;
          XtVaGetValues(w, XmNeditable, &is_editable, NULL);
          if (is_editable) {
                 valuestr= XmTextGetString( w);
                 free_val= True;
                 resstr= TEXTFRES;
          }
          else   succ= False;
     }

     else if ( XtClass(w) == xmScaleWidgetClass) {
          int val;
          resstr= SCALERES;
          XmScaleGetValue(w,&val);
          valuestr= (char*)malloc(50);
          sprintf(valuestr, "%d", val);
          free_val= True;
     }

     else if ( XtClass(w) == xmToggleButtonWidgetClass) {
          valuestr= XmToggleButtonGetState(w) ? TRUESTR : FALSESTR;
          resstr= TOGRES;
     }

     else if ( XtClass(w) == xmToggleButtonGadgetClass) {
          valuestr= XmToggleButtonGadgetGetState(w) ? TRUESTR : FALSESTR;
          resstr= TOGRES;
     }

     else if ( XtClass(w) == xmRowColumnWidgetClass) {
           unsigned char rctype;
           XtVaGetValues(w, XmNrowColumnType, &rctype, NULL);
           succ= False;
           if (rctype == XmMENU_OPTION) {
                  WidgetList wary;
                  Cardinal   numc;
                  int i;
                  Boolean found= False;
                  XtVaGetValues(w, XmNchildren,    &wary, 
                                   XmNnumChildren, &numc, NULL);
                  for(i=0; (!found)&&(i<numc); i++) {
                      if (XtClass(wary[i]) == xmCascadeButtonGadgetClass) {
                           found= True;
                           resstr= SCALERES;
                           valuestr= get_simp_labelstrptr(wary[i]);
                           free_val= True;
                           succ= True;
                      }
                  }/* end loop */
                  if (!found) printf(OPTIONERR);
           } /* end if */
     } /* end else if */

     else if ( XtClass(w) == fileChoiceWidgetClass) {
          valuestr= wprocFileChoiceGetFile(w);
          free_val= True;
          resstr= FILECRES;
     } /* end else if */

     else {
          succ= False;
     }

     if (succ) {
          assert(resstr);
          assert(valuestr);
          outstr=  (char *)malloc( strlen(wstr) + strlen(resstr) + 
                                   strlen(valuestr) + 5 );
          sprintf( outstr, "%s%s%s", wstr, resstr, valuestr);
          fprintf( def->fp, "%s\n", outstr );
          free(outstr);
     }


     if (free_val) XtFree( valuestr);

  } /* End if (w) && (def) */
  else if (!def) 
     fprintf(stderr,"DefSaveWidget: ERROR: DefInfo pointer NULL.\n");

}




/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void DefSave( DefInfo def, Widget w)
{
  if (def) wprocTravWTree(w, retw, def);
  else     fprintf(stderr, "DefSave: ERROR: DefInfo pointer NULL.\n");
}



/*
 * ------------------------------ FUNCTION ----------------------------------
 */
DefInfo DefFileInit( char *fname, int level, Boolean in_stand_dir)
{
  DefInfo def;
  char pathname[400];
 
  def= (DefInfo)malloc( sizeof (struct DefaultInfo) );

  if (def) {
      def->level= level;
      if (in_stand_dir)
             def->fp= do_open( Deffile_path(fname, pathname), "w+" );
      else {
             def->fp= do_open( fname, "w+" );
      }
      if (def->fp) {
            standard_blurb(def);
      }
      else {
            free(def);
            def= NULL;
      }
  }

  return (def);
}

/*
 * ------------------------------ FUNCTION ----------------------------------
 */
DefInfo DefStandardInit( Widget w, int level)
{
  char filename[400];
  DefInfo def;

  def= (DefInfo)malloc( sizeof (struct DefaultInfo) );
  if (def) {
      def->level= level;
      def->fp= do_open( determine_file(w,filename), "w+" );
      if (def->fp) {
            standard_blurb(def);
      }
      else {
            free(def);
            def= NULL;
      }
  }

  return (def);
}

/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void DefLevel( DefInfo def, int level)
{
   if (def) def->level= level;
   else fprintf(stderr, "DefLevel: ERROR: DefInfo pointer NULL.\n");
}


/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void DefEnd( DefInfo def)
{
   if (def) {
       if (def->fp) fclose(def->fp);
       free (def);
   }
   else fprintf(stderr, "DefEnd: ERROR: DefInfo pointer NULL.\n");
}


/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void DefLoadFileRes( Widget   w,
                     char     *fname,
                     Boolean  in_stand_dir)
{
  char pathname[500];

  if (fname) {
      if (in_stand_dir) {
             Deffile_path(fname, pathname);
             loadres( XtDisplay(w), pathname);
      }
      else {
             loadres( XtDisplay(w), fname);
      }
  }
}

/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void DefLoadStandardRes(Widget w)
{
  char filename[500];
  loadres( XtDisplay(w), determine_file(w,filename) );
}

/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void DefLoadValue( Widget w)
{
  wprocTravWTree(w, retw_load, NULL);
}


/*
 * ------------------------------ FUNCTION ----------------------------------
 */
char *DefgetWValue( Widget w)
{
  return ( DefLoadGetWValue(w,False,False) );
}


/*
 * ------------------------------ FUNCTION ----------------------------------
 */
char *DefLoadWValue( Widget w)
{
  return ( DefLoadGetWValue(w,True,False) );
}


/*
 * ------------------------------ FUNCTION ----------------------------------
 */
char *DefLoadGetWValue( Widget w, Boolean doload, Boolean inform)
{
  char wstr[500];
  char cstr[500];
  char class_resstr[550];
  char insta_resstr[550];
  char *retstr= NULL;

  make_wctree(w, wstr, cstr, wprocALL);

  if ( ( XtClass(w) == xmTextFieldWidgetClass) ||
       ( XtClass(w) == xmTextWidgetClass) ) {
          sprintf( class_resstr, "%s.Value", cstr);
          sprintf( insta_resstr, "%s.value", wstr);
  }

  else if ( XtClass(w) == xmScaleWidgetClass) {
          sprintf( class_resstr, "%s.Value", cstr);
          sprintf( insta_resstr, "%s.value", wstr);
  }

  else if ( ( XtClass(w) == xmToggleButtonWidgetClass) ||
            ( XtClass(w) == xmToggleButtonGadgetClass) ) {
          sprintf( class_resstr, "%s.Set", cstr);
          sprintf( insta_resstr, "%s.set", wstr);
  }

  else if ( XtClass(w) == fileChoiceWidgetClass) {
          sprintf( class_resstr, "%s.Filename", cstr);
          sprintf( insta_resstr, "%s.filename", wstr);
  }
  
  retstr= DefGetStrValue( XtDisplay(w), insta_resstr, class_resstr);


  if ( (doload)&&(retstr) ) {
     if ( XtClass(w) == xmTextFieldWidgetClass) {
          XmTextFieldSetString( w, retstr);
          if (inform) {
          }
     }

     else if ( XtClass(w) == xmTextWidgetClass) {
          XmTextSetString( w, retstr);
          if (inform) {
          }
     }

     else if ( XtClass(w) == xmScaleWidgetClass) {
          long val;
          if (check_sint(retstr,&val))  {
                 XmScaleSetValue(w, (int)val);
                 if (inform) {
                 }
          }
     }

     else if ( XtClass(w) == xmToggleButtonWidgetClass) {
          XmToggleButtonSetState(w, boolstr_value(retstr), inform);
     }

     else if ( XtClass(w) == xmToggleButtonGadgetClass) {
          XmToggleButtonGadgetSetState(w, boolstr_value(retstr), inform);
     }

     else if ( XtClass(w) == fileChoiceWidgetClass) {
          wprocFileChoiceSetFile(w, retstr, inform);
     }
  } /* end if (doload) */

  return (retstr);

}

/*
 * ------------------------------ FUNCTION ----------------------------------
 */
Boolean boolstr_value( char *str)
{
  char lowstr[10];
  Boolean retval;

  strncpy(lowstr, str, 9);
  lowstr[9]= '\0';

  if (strcmp( strToLower(str), "true") == 0 )
          retval= True;
  else
          retval= False;

  return (retval);
}

/*
 * ------------------------------ FUNCTION ----------------------------------
 */
char *DefGetStrValue( Display *dpy, char *istr, char *cstr)
{
  XrmDatabase db;
  Bool result;
  char *str_type;
  XrmValue value;
  char *retval;

# if (XlibSpecificationRelease>=5)
     db= XtDatabase(dpy);
# else
     db= dpy->db;
# endif

  result= XrmGetResource(db, istr, cstr, &str_type, &value); 

  if (result) retval= value.addr;
  else        retval= NULL;

  return (retval);
}


#define LINE1 "====== This file is automaticly generated. ======"
#define LINE2 "======   Any changes will be destroyed.    ======"
/*
 * ----------------------- static FUNCTION ----------------------------------
 */
static void standard_blurb(DefInfo def) 
{
  fprintf( def->fp, "!\n!%s\n!%s\n!\n", LINE1, LINE2);
}


/*
 * ----------------------- static FUNCTION ----------------------------------
 */
static void loadres( Display *dpy, char *filename)
{

   XrmDatabase rdb, db;
   rdb = XrmGetFileDatabase ( filename ); 
   /*
    * Merge them into the Xt database, with highest precendence
    */
   if ( rdb ) {
#        if (XlibSpecificationRelease>=5)
              db = XtDatabase(dpy);
              XrmCombineDatabase(rdb, &db, True);
#        else
              XrmMergeDatabases ( rdb, &dpy->db);
#        endif
   } /* End if */
}


/*
 * ----------------------- static FUNCTION ----------------------------------
 */
char *Deffile_path(char *infile, char *outfile)
{

  char *appdir;
  char pathchar;
#if (VMS)
   pathchar= '[';
#else
   pathchar= '/';
#endif

  if (strlen(infile)>0) {
     if (strchr(infile, pathchar)) {
         strcpy(outfile,infile);
         return (outfile);
     }
  }
#if (VMS)
  sprintf(outfile, "sys$login:%s", infile);
#else
  appdir= getenv( "XAPPLRESDIR");
  if (!appdir)
      appdir= getenv( "HOME");

  if (appdir)
      sprintf(outfile, "%s%c%s", appdir, SEPCHAR, infile);
  else
      sprintf(outfile, "%s", infile);
#endif
  return (outfile);
}



/*
 * ----------------------- static FUNCTION ----------------------------------
 */
static char *determine_file(Widget w, char *filename)
{

  char *wname;

  wname= XtName( get_toplevel_shell(w) );

  Deffile_path(wname, filename);
  strcat( filename, UDESC);
#if !( (unix) || defined(_POSIX_SOURCE))
  strcat( filename, ".dat");
#endif 

 return (filename);
}

/*
 * ----------------------- static FUNCTION ----------------------------------
 */
static FILE *do_open( char *fname, char *how)
{
  FILE *fp;

#if (VMS)
     fp= fopen(fname, how, "ctx = stm" );
#else
     fp= fopen(fname, how);
#endif

 return (fp);

}

/*
 * ----------------------- static FUNCTION ----------------------------------
 */
static void retw(Widget w, void *data)
{
    DefInfo def= (DefInfo)data;
    DefSaveWidget (def, w);
}


/*
 * ----------------------- static FUNCTION ----------------------------------
 */
static void retw_load(Widget w, void *data)
{
   DefLoadGetWValue( w, True, True);
}
