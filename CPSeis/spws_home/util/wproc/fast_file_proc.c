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
#include "fast_file_proc.h"
#include "ls_lstat.h"
#include "file_choice.h"
#include "cprim.h"

#include <X11/IntrinsicP.h> 
#include <X11/StringDefs.h>

static FileList *file_list = 0;

void fast_file_proc (Widget w, XmFileSelectionBoxCallbackStruct *cbs )
{
  unsigned char stat;
  XmStringTable updated_list = NULL;

  char *path, *pattern;
  int k2;

  path = get_string_from_xmstr (cbs->dir);
  pattern = get_string_from_xmstr (cbs->pattern);
  if (!haveFileList(file_list,path,pattern)) {
    if (file_list) deleteFileList (file_list);
    file_list = createFileList (path, pattern);
  }
  /*
   * build file mask
   */
  if (file_list->pattern_count > 0) {
    updated_list = malloc (sizeof(XmString)*file_list->pattern_count);
    for (k2 = 0; k2 < file_list->pattern_count; k2++) {
      updated_list[k2] = XmStringCreateLtoR (file_list->pattern_files[k2],
	XmSTRING_DEFAULT_CHARSET);
    }
  }
  XtVaSetValues (w, XmNfileListItems ,  updated_list,
	         XmNfileListItemCount,  file_list->pattern_count,
		 XmNlistUpdated      ,  True,
		 NULL);
  free (path);
  free (pattern);
}

void fast_dir_proc (Widget w, XmFileSelectionBoxCallbackStruct *cbs)
{
  unsigned char stat;
  XmStringTable updated_list = NULL;

  char *path, *pattern;
  int k2;

  path = get_string_from_xmstr (cbs->dir);
  pattern = get_string_from_xmstr (cbs->pattern);
  if (!haveFileList(file_list,path,pattern)) {
    if (file_list) deleteFileList (file_list);
    file_list = createFileList (path, pattern);
  }

  if (file_list->directory_count > 0) {
    updated_list = malloc (sizeof(XmString)*file_list->directory_count);
    for (k2 = 0; k2 < file_list->directory_count; k2++) {
      updated_list[k2] = XmStringCreateLtoR (file_list->directories[k2],
        XmSTRING_DEFAULT_CHARSET);
    }
  }
  XtVaSetValues (w, XmNdirListItems,   updated_list,
                 XmNdirListItemCount,  file_list->directory_count,
	         XmNdirectoryValid,    True,
		 XmNlistUpdated,       True, NULL);
  free (path);
  free (pattern);
}

void reset_file_list ()
{
  if (file_list) deleteFileList (file_list);
  file_list = 0;
}
