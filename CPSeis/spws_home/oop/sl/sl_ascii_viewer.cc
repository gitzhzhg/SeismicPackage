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
// SeisPlot's history viewer class
// ONLY SUPPORTS CPS TROT AND BYTE FILES FOR NOW
// Author: Michael L. Sherrill
// 11/94

#include "sl/sl_ascii_viewer.hh"
#include "sl/slp_file.hh"
#include "sp/seis_plot.hh"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <Xm/ScrolledW.h>
#include <Xm/Text.h>

#define MAX_FILE_LENGTH 250

static String  defres[]= {
    "*file.topAttachment:              ATTACH_POSITION",
    "*file.topPosition:                5",
    "*file.leftAttachment:             ATTACH_FORM",
    "*file.rightAttachment:            ATTACH_FORM",
    "*file.leftOffset:                 5",
    "*file.rightOffset:                5",
    "*button_control.OK.labelString:   Remove",
    NULL };




SLAsciiViewer::SLAsciiViewer( Widget        p, 
                              char          *name, 
                              HelpCtx       hctx,
			      SLpFileData   *slp_file_data,
                              Boolean       small_on_dpi,
                              Boolean       make_now,
                              SeisPlot      *sp)

         : SLFormFCPop( p,name,(FP_DOOK | FP_DOHELP),
                        slp_file_data, hctx,small_on_dpi,False) 
{

 _sp = sp;

 init( XtDisplay(p) );
 if (make_now) 
    make(p);
 else 
    supportUnmadeDefaults(p);


}



Widget SLAsciiViewer::make(  Widget        p)
{
 Widget scrolled;
 int screen; 
 int height;
 int width;
 Widget extension_widget, junk1, junk2, junk3;


   screen = DefaultScreen(XtDisplay(wParent()));
   height = (int)(DisplayHeight(XtDisplay(wParent()), screen) * .65);
   width  = (int)(DisplayWidth (XtDisplay(wParent()), screen) * .75);

    if(!made()) 
       {
       setDefaultResources( XtDisplay(wParent()), _name, defres);
       SLFormFCPop::make(p);

       XtVaSetValues (_slp_file->W(),
		      XmNtopAttachment,    XmATTACH_FORM,
		      XmNtopOffset,        5,
		      XmNleftAttachment,   XmATTACH_FORM,
		      XmNleftOffset,       5,
		      XmNrightAttachment,  XmATTACH_FORM,
		      XmNrightOffset,      5,
		      NULL);

       scrolled= XtVaCreateManagedWidget("scrolled",xmScrolledWindowWidgetClass,
                              topWidget(),
                              XmNtopAttachment,          XmATTACH_WIDGET,
                              XmNtopWidget,              _slp_file->W(), 
                              XmNtopOffset,              10,
                              XmNleftAttachment,         XmATTACH_FORM,
                              XmNrightAttachment,        XmATTACH_FORM,
                              XmNbottomAttachment,       XmATTACH_WIDGET,
                              XmNbottomWidget,           buttonContainer(),
                              XmNbottomOffset,           20, 
                              XmNscrollBarDisplayPolicy, XmSTATIC,
                              XmNscrollingPolicy,        XmAPPLICATION_DEFINED,
                              XmNvisualPolicy,           XmVARIABLE,
                              XmNwidth,                  width,
                              XmNheight,                 height,
                              NULL);

  
       _text_widget = XtVaCreateManagedWidget("text_widget",xmTextWidgetClass,
                              scrolled,
                              XmNeditMode,               XmMULTI_LINE_EDIT,
                              XmNeditable,               False, NULL);

       }

   defaultButton(FP_OK, False);

   return topWidget();
}

void SLAsciiViewer::managing()
{
  char defaultfile[MAX_FILE_LENGTH];
  char *extension = ".glbl";
  int length = 0;
  long stat;

  if (_slp_file->filename() != NULL) {
    presentText (_slp_file->filename());
    length = strlen (_slp_file->filename());
  }
  else {
    setFilename (_sp->filename());
    length = strlen (_slp_file->filename());
  }

  if (length) {   
    strcpy (defaultfile, _slp_file->filename());
    if (!strcmp(_sp->fileType(), "CBYTE")) { //Get the .glbl file
      addGlobalExtension (defaultfile,extension,&stat);
    }
    _slp_file->setFilename (defaultfile);
  }
}

SLAsciiViewer::~SLAsciiViewer()
{
//should I free text in _text_widget here?
}

Boolean SLAsciiViewer::validateFile (char *filename)
{
  return presentText (filename);
}

//Read the file. Warning this is very weak since it depends on
//keywords and only supports cps type files for now
Boolean SLAsciiViewer::presentText (char *filename)
{
  assert (_slp_file->type() == SLpFile::_INPUT);

  FILE *filep;
  int csize = MAXLENGTH;
  long pos = 0;
  char str[MAXLENGTH], str2[MAXLENGTH], *num;
  Boolean quit = False, have_beg = False, have_end = False;
  int type = -1;
  int ib1 = 0, ie1 = 0, ib2 = 0, ie2 = 0;

  strcpy (str," ");
  if (made()) XmTextSetString (_text_widget, str);
  if (filename == NULL || strlen(filename) < 1) return False;


  filep = fopen (filename, "r");
  if (filep) {
    fseek (filep, 0, 0);
  }
  else { // fopen failed
    strcpy (str, ERR_MSG);
    if (made()) XmTextInsert (_text_widget, 10, str);
    return False;
  }

  
  if (fgets(str,csize,filep) == NULL) { //error getting data
    strcpy (str, ERR_MSG);
    if (made()) XmTextInsert (_text_widget, 10, str);
    return False;
  }

  //Only support trot and byte file globals for now
  if (strstr(str,"CPS") != NULL) {
    type = TROT_TYPES;
  }
  else if (strstr(str,"ndpt") != NULL) {
    type = BYTE_TYPES;
  }
  else {
    strcpy (str, ERR_MSG);
    if (made()) XmTextInsert (_text_widget, 10, str);
    return False;
  }

  //If the history record does not exist then the hist_start_pos
  //and the hist_end_pos will be equivalent. In this case skip search.
  //Read a trcio trot type file. The history is contained between the
  //keywords #<DTA_history> and #</history>
  if (type == TROT_TYPES) {
    while (fgets(str,csize,filep) != NULL) {
      if (quit) break;
      if (!have_beg && strstr(str,"# hist_start_pos = (") != NULL) {
        strcpy (str2, str); // get a working copy
        num = strtok (str2, " ");  // get the "#"
        num = strtok (NULL, " ");  // get the variable name
        num = strtok (NULL, " ");  // get the equals sign
        num = strtok (NULL, " ("); // get the number appended by ","
        num[strlen(num)-1] = '\0'; // overwrite the ending ","
        sscanf (num, "%d", &ib1);  // get beginning of index
        num = strtok (NULL, " ");  // get the number appended by ")"
        num[strlen(num)-1] = '\0'; // overwrite the ending ")"
        sscanf (num, "%d", &ie1);  // get ending of index
        have_beg = True;
      }
      if (!have_end && strstr(str,"# hist_end_pos = (") != NULL) {
        strcpy (str2, str); // get a working copy
        num = strtok (str2, " ");  // get the variable name
        num = strtok (NULL, " ");  // get the equals sign
        num = strtok (NULL, " ("); // get the number appended by ","
        num[strlen(num)-1] = '\0'; // overwrite the ending ","
        sscanf (num, "%d", &ib2);  // get beginning of index
        num = strtok (NULL, " ");  // get the number appended by ")"
        num[strlen(num)-1] = '\0'; // overwrite the ending ")"
        sscanf (num, "%d", &ie2);  // get ending of index
        have_end = True;
      }
      if (have_beg && have_end) {
        if (ib1 == ib2 && ie1 == ie2) {
          quit = True; // no history in file
        }
      }
      if (!quit && strstr(str,"#<DTA_history>\n") != NULL) {
        while (fgets(str,csize,filep) != NULL &&
	  strstr(str,"#</history>\n") == NULL) {
          if (made()) XmTextInsert (_text_widget, pos, str);
          pos += strlen (str);
	}
        quit = True;
      }
    }
  }
  else { //Old .glbl file 
    while (fgets(str,csize,filep) != NULL) {
      if (made()) XmTextInsert (_text_widget, pos, str);
      pos += strlen (str);
    }
  }

  fclose (filep);
  return True;
}

void SLAsciiViewer::addGlobalExtension( char *name, char *ext, long *istat )
{
  char *the_dir, *closebracket, *the_slash, *the_dot;
  char *the_semicolon, *the_extension;
  char file_byt[MAX_FILE_LENGTH];
  unsigned int lenp;


  *istat=0;
  if(strlen(ext) < 2 )  return;


// Parse any occurences of leading "." in the extension
  the_extension = ext;
  if(ext[0]=='.') the_extension = ext + 1;

// Add extension to file name if it is missing
  closebracket=strstr(name,"]");
  the_slash=strrchr(name,'/');
  the_dir=NULL;
  if(closebracket!=NULL) the_dir= closebracket;
  if(the_slash!=NULL) the_dir= the_slash; 
  if(the_dir==NULL)
   { the_dot = strrchr(name,'.');
     the_semicolon = strrchr(name,';');
     if(the_dot != NULL)
       {  lenp = the_dot-name+1;
          strncpy(file_byt,name,lenp);
          file_byt[lenp]='\0';
          strcat(file_byt,the_extension); }
     else
       {  strcpy(file_byt,name);
          strcat(file_byt,".");
          strcat(file_byt,the_extension);}
     if(the_semicolon != NULL ) strcat(file_byt,the_semicolon);
   }
  else
   { the_dot = strrchr(the_dir,'.');
     the_semicolon = strrchr(the_dir,';');
     if(the_dot != NULL)
       {  lenp = the_dot-name+1;
          strncpy(file_byt,name,lenp);
          file_byt[lenp]='\0';
          strcat(file_byt,the_extension); }
     else
       {  strcpy(file_byt,name);
          strcat(file_byt,".");
          strcat(file_byt,the_extension);}
     if(the_semicolon != NULL ) strcat(file_byt,the_semicolon);
   }
 strcpy(name, file_byt);

}
