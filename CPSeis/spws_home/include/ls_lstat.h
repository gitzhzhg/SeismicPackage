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
#ifndef LS_LSTAT_H
#define LS_LSTAT_H

#include <time.h>

typedef struct _tokens
{
  char **toks;
  char *pattern;
  int num_toks,
      wild_beg,
      wild_end;
} Tokens;

typedef struct _file_list
{
  Tokens *tokens;
  char **all_files;
  char **files_and_links;
  char **directories;
  char **pattern_files;
  char *path;
  int all_count,
    file_count,
    directory_count,
    pattern_count;
} FileList;

#ifdef __cplusplus  
extern "C" {                          /* for C++ */
#endif
/*
 * Declaration of public functions
 */
  FileList *createFileList
    (char *path,
     char *pattern);

  void deleteFileList
    (FileList *dis);

  int haveFileList
    (FileList *dis,
     char *path,
     char *pattern);

  time_t timeLastModified
    (char *full_path);

#ifdef __cplusplus  
}
#endif

#endif /* LS_LSTAT_H */
