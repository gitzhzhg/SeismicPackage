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

//---------------------- slp_file.cc ----------------------------------//
//---------------------- slp_file.cc ----------------------------------//
//---------------------- slp_file.cc ----------------------------------//

//           implementation file for the SLpFile class
//               derived from the SLSmartForm class
//                       subdirectory sl

#include "sl/slp_file.hh"
#include "sl/slp_file_data.hh"
#include "sl/slp_push.hh"
#include "sl/slp_label.hh"
#include "sl/slp_text.hh"
#include "sl/sl_file_selection_pop.hh"
#include "sl/psuedo_widget.hh"
#include "ls_lstat.h"
#include "cprim.h"
#include "trciof77.h"
#include <Xm/Label.h>
#include <Xm/Label.h>
#include <Xm/Label.h>
#include <assert.h>
#include "workstation.h"


//-----------------constructor----------------------------------------//
//-----------------constructor----------------------------------------//
//-----------------constructor----------------------------------------//


SLpFile::SLpFile (SLDelay *slparent, char *name, long ident,
                                const char *label,
                                const char *filetype,
                                const char *extension,
                                int io,
                                const char *pattern)
    : SLSmartForm(slparent, name),
               _ident              (0),
               _label              (NULL),
               _filetype           (NULL),
               _extension          (NULL),
               _io                 (io),
               _pattern            (NULL),
               _slp_push           (NULL),
               _slp_label          (NULL),
               _slp_text           (NULL),
               _sl_filebox         (NULL)
{
  constructorHelper(ident, label, filetype, extension, pattern);
  if(slparent->topWidget()) make();
}


SLpFile::SLpFile (Widget wparent, char *name, long ident,
                                const char *label,
                                const char *filetype,
                                const char *extension,
                                int io,
                                const char *pattern)
    : SLSmartForm(wparent, name),
               _ident              (0),
               _label              (NULL),
               _filetype           (NULL),
               _extension          (NULL),
               _io                 (io),
               _pattern            (NULL),
               _slp_push           (NULL),
               _slp_label          (NULL),
               _slp_text           (NULL),
               _sl_filebox         (NULL)
{
  constructorHelper(ident, label, filetype, extension, pattern);
  //make();
}


SLpFile::SLpFile (SLDelay *slparent, SLpFileData *data)
    : SLSmartForm(slparent, data->name()),
               _ident              (0),
               _label              (NULL),
               _filetype           (NULL),
               _extension          (NULL),
               _io                 (data->io()),
               _pattern            (NULL),
               _slp_push           (NULL),
               _slp_label          (NULL),
               _slp_text           (NULL),
               _sl_filebox         (NULL)
{
  constructorHelper(data->ident(), data->label(), data->filetype(),
    data->extension(), data->pattern());
  if (slparent->topWidget()) make();
}


SLpFile::SLpFile (Widget wparent, SLpFileData *data)
    : SLSmartForm(wparent, data->name()),
               _ident              (0),
               _label              (NULL),
               _filetype           (NULL),
               _extension          (NULL),
               _io                 (data->io()),
               _pattern            (NULL),
               _slp_push           (NULL),
               _slp_label          (NULL),
               _slp_text           (NULL),
               _sl_filebox         (NULL)
{
  constructorHelper(data->ident(), data->label(), data->filetype(),
    data->extension(), data->pattern());
  //make();
}


//------------------ destructor -------------------------//
//------------------ destructor -------------------------//
//------------------ destructor -------------------------//

SLpFile::~SLpFile(void)
{
  if(_label)     free(_label);
  if(_filetype)  free(_filetype);
  if(_extension) free(_extension);
  if(_pattern)   free(_pattern);

  if (_sl_filebox) delete _sl_filebox;
}


//------------------------- yes fun ---------------------------//
//------------------------- yes fun ---------------------------//
//------------------------- yes fun ---------------------------//

          // static.

void SLpFile::yesFun (void *data, const char *newvar)
{
  SLpFile *pop = (SLpFile*)data;

  char *filename;
  if (isAnEmptyFilename(newvar)) {
    filename = newstr("NONE");
  }
  else {
    int length = strlen(newvar);
    if (newvar[length-1] == '/') {
      filename = newstr("NONE");
    }
    else {
      filename = newstr(newvar);
    }
  }

  char *old_filename = newstr (pop->_slp_text->cvar());
  pop->_slp_text->setCvar ((char*)filename);
  if (pop->_trap) {
    pop->_trap(pop->_data, pop->_ident, old_filename, (char*)filename);
  }
  if (old_filename) free (old_filename);

  if (filename) free (filename);
}


//------------------------- trap fun ---------------------------//
//------------------------- trap fun ---------------------------//
//------------------------- trap fun ---------------------------//

          // static.

void SLpFile::trapFun(void *data, long ident, char *oldvar, char *newvar)
{
  SLpFile *pop = (SLpFile*)data;

  char *filename;
  if (isAnEmptyFilename(newvar))
      {
      filename = newstr("NONE");
      }
  else
      {
      int length = strlen(newvar);
      if(newvar[length-1] == '/')
          {
          filename = newstr("NONE");
          }
      else
          {
          int j = FALSE;
          for(int i = 0; i < length; i++)
              {
              if     (newvar[i] == '.') j = TRUE;
              else if(newvar[i] == '/') j = FALSE;
              }
          if(j) filename = newstr(newvar);
          else  filename = newstrcat(newvar,".",pop->_extension,NULL);
          }
      }

  pop->_slp_text->setCvar(filename);
  if(pop->_trap) pop->_trap(pop->_data, pop->_ident, oldvar, filename);

  if(filename) free(filename);
}


//-------------------- isAnEmptyFilename -----------------------//
//-------------------- isAnEmptyFilename -----------------------//
//-------------------- isAnEmptyFilename -----------------------//

          // static.

Boolean SLpFile::isAnEmptyFilename (const char *filename)
{
  Boolean retval = !filename                ||
                   filename[0] == '\0'      ||
                   filename[0] == ' '       ||
                   !strcmp(filename,"NONE") ||
                   !strcmp(filename,"none")   ;
  return retval;
}


//---------------- constructor helper -----------------------//
//---------------- constructor helper -----------------------//
//---------------- constructor helper -----------------------//

       // private.

void SLpFile::constructorHelper(long ident,
                                const char *label,
                                const char *filetype,
                                const char *extension,
				const char *pattern)
{
  assert (_io == _INPUT || _io == _OUTPUT);
  char *search;

  _ident = ident;

  if (label)              _label = newstr (label);
  else if (_io == _INPUT) _label = newstr ("Input File...");
  else                    _label = newstr ("Output File:");

  if (filetype)  _filetype  = newstr (filetype);
  else           _filetype  = newstr ("File");

  if (extension) _extension = newstr (extension);
  else           _extension = newstr ("file");

  if (pattern)   _pattern   = newstr (pattern);
  else           _pattern   = newstr ("*.");

  if (extension) search     = newstrcat (_pattern, extension, NULL);
  else           search     = newstrcat (_pattern, "file", NULL);

  if (_io == _INPUT) {
    _slp_push  = new SLpPush (this, "label", -ident, (char*)_label);
    _slp_text  = new SLpText (this, "text" ,  ident);
    attach (_slp_push ,       this, NULL, this, this);
    attach (_slp_text , _slp_push , this, this, this, 5);
    _sl_filebox  = new SLFileSelectionPop (this, (char*)_filetype, search,
      yesFun, this);
    _slp_push->manageShellWhenPressed (_sl_filebox);
  }
  else {
    _slp_label = new SLpLabel (this, "label", -ident, (char*)_label);
    _slp_text  = new SLpText  (this, "text" ,  ident);
    attach (_slp_label,       this, NULL, this, this);
    attach (_slp_text , _slp_label, this, this, this);
  }
  _slp_text->setCvar  ("NONE");
  _slp_text->setCtrap (trapFun, this);

  free (search);
}



//------------------------- reset extension ---------------------------//
//------------------------- reset extension ---------------------------//
//------------------------- reset extension ---------------------------//

     // public.

void SLpFile::resetLabel (const char *label)
{
  if (_label) free (_label);

  if (label)              _label = newstr(label);
  else if (_io == _INPUT) _label = newstr("Input File...");
  else                    _label = newstr("Output File:");

  if(_io == _INPUT) {
    _slp_push->setupCvarValue (_label);
  }
  else {
    _slp_label->setupCvarValue (_label);
  }
}

void SLpFile::resetExtension(const char *extension)
{
  if (_extension) free (_extension);

  if(extension)      _extension = newstr(extension);
  else               _extension = newstr("file");
}


void SLpFile::resetPattern (const char *pattern)
{
  if (_sl_filebox) {
    if (_pattern) {
      free (_pattern);
      _pattern = NULL;
    }

    char *search = NULL;

    if (pattern) {
      _pattern = newstr (pattern);
    }
    else {
      _pattern = newstr ("*.");
    }

    if (_extension) search = newstrcat (_pattern, _extension, NULL);
    else            search = newstrcat (_pattern, "file");

    _sl_filebox->setPattern (search);

    free (search);
  }
}

time_t SLpFile::timeStamp (char *filename)
{
  time_t retval;

  char *fname = NULL;
  assert (filename != NULL);
  fname = workstation_get_jseis_fprop (filename);
  assert (fname != NULL);
  retval = timeLastModified (fname);
  if (fname != filename) {
    free (fname);
    fname = NULL;
  }
  return retval;
}

//---------------------- make ------------------------------------//
//---------------------- make ------------------------------------//
//---------------------- make ------------------------------------//


Widget SLpFile::make(Widget p)
{
  if(!made())
     {
     Widget w = SLSmartForm::make(p);
     XmRemoveTabGroup(w);
     }
  makeChildren();
  return topWidget();
}



/*
//----------------------- char callback --------------------------//
//----------------------- char callback --------------------------//
//----------------------- char callback --------------------------//

     // static member function.
     // identical to SLpBase::integerCallback except for type
     //   change, and for the assert.

void SLpFile::charCallback(Widget w, XtPointer user, XtPointer call)
{
}
*/



//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
