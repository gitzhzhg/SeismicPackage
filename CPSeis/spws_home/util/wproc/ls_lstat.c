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
#include "ls_lstat.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include <assert.h>
#include <pthread.h>
#include <unistd.h>
#include <math.h>
#include <errno.h>

#define INCR 200

typedef struct _stat_data
{
  char *full_path;
  struct stat *status;
  int error;
} StatData;

/*
 * Declarations of private functions
 */
int *getFileTypes
  (int num,
   char **files);

char **getAllDirectories
  (int *file_types,
   int num,
   char **files,
   int *num_dirs);

char **getAllFilesAndLinks
  (int *file_types,
   int num,
   char **files,
   int *num_no_dirs);

char **getPatternFiles
  (Tokens *tokens,
   int num,
   char **files,
   int *num_patts);

int fileType
  (char *full_path);

StatData *createStatData ();

void deleteStatData
  (StatData *stat_data);

char **executeCommand
  (char *cmd,
   int *num);

void deleteFileListTokens
  (FileList *dis);

Tokens *createTokens
  (char *path,
   char *pattern);

void deleteTokens
  (Tokens *dis);

int haveTokens
  (Tokens *dis,
   char *pattern);

int withinPattern
  (Tokens *dis,
   char *file);

int quickStat
  (StatData *status);

void doLstat
  (void *data);

char *lstatErrorString
  (int error);

char *statErrorString
  (int error);

char *errorString
  (const char *statg_fcn,
   int error);

int *getFileTypes (int num, char **files)
{
  int *retval;

  int k2, k3;
  int size = 0;

  assert (num > 0 && files);

  retval = (int *)malloc (num*sizeof(int));

  for (k2 = 0; k2 < num; k2++) {
    retval[k2] = fileType (files[k2]);
  }
  return retval;
}

char **getAllDirectories (int *file_types, int num, char **files,
  int *num_dirs)
{
  char **retval = 0;

  int k2, k3;
  int size = 0;
  char **tmp;

  assert (file_types && num > 0 && files && num_dirs);

  *num_dirs = 0;

  for (k2 = 0; k2 < num; k2++) {
    switch (file_types[k2]) {
    case 'd' :
      if (*num_dirs == size) {
	tmp = (char **)calloc ((size_t)(size+INCR), sizeof(char*));
	for (k3 = 0; k3 < size; k3++) {
	  tmp[k3] = retval[k3];
	}
	free (retval);
	retval = tmp;
	size += INCR;
      }
      retval[*num_dirs] =
        (char *)malloc ((strlen(files[k2])+1)*sizeof(char));
      strcpy (retval[*num_dirs], files[k2]);
      (*num_dirs)++;
      break;
    case  0  :
    case 'f' :
    case 'l' :
    case 'p' :
    case 's' :
    case 'o' :
    default:
      break;
    }
  }
  return retval;
}

char **getAllFilesAndLinks (int *file_types, int num, char **files,
  int *num_files)
{
  char **retval = 0;

  int k2, k3;
  int size = 0;
  char **tmp;

  assert (file_types && num > 0 && files && num_files);

  *num_files = 0;

  for (k2 = 0; k2 < num; k2++) {
    switch (file_types[k2]) {
    case 'f' :
    case 'l' :
      if (*num_files == size) {
	tmp = (char **)calloc (size+INCR, sizeof(char*));
	for (k3 = 0; k3 < size; k3++) {
	  tmp[k3] = retval[k3];
	}
	free (retval);
	retval = tmp;
	size += INCR;
      }
      retval[*num_files] =
        (char *)malloc ((strlen(files[k2])+1)*sizeof(char));
      strcpy (retval[*num_files], files[k2]);
      (*num_files)++;
      break;
    case  0  :
    case 'd' :
    case 'p' :
    case 's' :
    case 'o' :
    default:
      break;
    }
  }
  return retval;
}

char **getPatternFiles (Tokens *tokens, int num, char **files,
  int *num_files)
{
  char **retval = 0;

  int k2, k3;
  int size = 0;
  char **tmp;

  assert (tokens && num > 0 && files && num_files);

  *num_files = 0;

  for (k2 = 0; k2 < num; k2++) {
    if (withinPattern(tokens,files[k2])) {
      if (*num_files == size) {
	tmp = (char **)calloc ((size_t)(size+INCR), sizeof(char*));
	for (k3 = 0; k3 < size; k3++) {
	  tmp[k3] = retval[k3];
	}
	free (retval);
	retval = tmp;
	size += INCR;
      }
      retval[*num_files] =
        (char *)malloc ((strlen(files[k2])+1)*sizeof(char));
      strcpy (retval[*num_files], files[k2]);
      (*num_files)++;
    }
  }
  return retval;
}

FileList *createFileList (char *path, char *pattern)
{
  FileList *retval = (FileList *)malloc (sizeof(FileList));

  char *full_path;
  int *types;
  int k2, plen;
  char cmd[BUFSIZ];

  retval->path = (char *)malloc ((strlen(path)+1)*sizeof(char));
  strcpy (retval->path, path);
  strcpy (cmd, "ls -a1 ");
  strcat (cmd, retval->path);
  strcat (cmd, " | sort -f");
  retval->all_files = executeCommand (cmd, &(retval->all_count));
  
  /* give the retval array a full path */
  plen = strlen (retval->path) + 1;
  for (k2 = 0; k2 < retval->all_count; k2++) {
    full_path = (char *)malloc (strlen(retval->all_files[k2])+plen);
    strcpy (full_path, retval->path);
    strcat (full_path, retval->all_files[k2]);
    free (retval->all_files[k2]);
    retval->all_files[k2] = full_path;
  }

  /* initialize variables */
  retval->tokens = createTokens (path, pattern);
  retval->files_and_links = 0;
  retval->file_count = 0;
  retval->directories = 0;
  retval->directory_count = 0;
  retval->pattern_files = 0;
  retval->pattern_count = 0;

  if (retval->all_count > 0) {
    types = getFileTypes (retval->all_count, retval->all_files);

    retval->files_and_links = getAllFilesAndLinks (types, retval->all_count,
      retval->all_files, &(retval->file_count));

    retval->directories = getAllDirectories (types, retval->all_count,
      retval->all_files, &(retval->directory_count));

    free (types);

    if (retval->file_count > 0) {
      retval->pattern_files = getPatternFiles (retval->tokens,
        retval->file_count, retval->files_and_links,
        &(retval->pattern_count));
    }
  }
  return retval;
}

void deleteFileList (FileList *dis)
{
  int k2;

  assert (dis);

  free (dis->path);

  for (k2 = 0; k2 < dis->all_count; k2++) {
    free (dis->all_files[k2]);
  }
  free (dis->all_files);

  for (k2 = 0; k2 < dis->file_count; k2++) {
    free (dis->files_and_links[k2]);
  }
  free (dis->files_and_links);

  for (k2 = 0; k2 < dis->directory_count; k2++) {
    free (dis->directories[k2]);
  }
  free (dis->directories);

  deleteFileListTokens (dis);
}

void deleteFileListTokens (FileList *dis)
{
  int k2;

  assert (dis);

  deleteTokens (dis->tokens);
  dis->tokens = 0;

  for (k2 = 0; k2 < dis->pattern_count; k2++) {
    free (dis->pattern_files[k2]);
  }
  free (dis->pattern_files);
  dis->pattern_files = 0;
  dis->pattern_count = 0;
}

int haveFileList (FileList *dis, char *path, char *pattern)
{
  int retval;

  if (dis && dis->file_count > 0) {
    retval = !strcmp (dis->path, path);
    if (retval) {
      retval = haveTokens (dis->tokens, pattern);
      if (!retval) {
	deleteFileListTokens (dis);
	dis->tokens = createTokens (path, pattern);
	dis->pattern_files = getPatternFiles (dis->tokens, dis->file_count,
	  dis->files_and_links, &(dis->pattern_count));
	retval = 1;
      }
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

/*
 * 'f' => a regular file
 * 'l' => an existing link
 * 'd' => a directory
 * 'p' => a FIFO stream
 * 's' => a socket
 * 'o' => other
 *  0  => not accessible
 */
int fileType (char *full_path)
{
  int retval;

  StatData *stat_data = createStatData ();

  assert (full_path);

  stat_data->full_path = full_path;
  stat_data->error = 0;

  if (quickStat(stat_data)) {
    retval = 0;
  }
  else {
    assert (stat_data->status);
    switch (stat_data->status->st_mode & S_IFMT) {
    case S_IFREG :
      retval = 'f';
      break;
    case S_IFDIR :
      retval = 'd';
      break;
    case S_IFLNK :
      retval = 'l';
      break;
    case S_IFIFO :
      retval = 'p';
      break;
    case S_IFSOCK :
      retval = 's';
      break;
    case S_IFCHR :
    case S_IFBLK :
      retval = 'o';
      break;
    default:
      retval = 'o';
      break;
    }
  }
  deleteStatData (stat_data);
  return retval;
}

StatData *createStatData ()
{
  StatData *stat_data = (StatData *)malloc (sizeof(StatData));
  struct stat *stats = (struct stat *)malloc (sizeof(struct stat));
  stat_data->status = stats;
  return stat_data;
}

void deleteStatData (StatData *stat_data)
{
  free (stat_data->status);
  free (stat_data);
  stat_data = 0;
}

char **executeCommand (char *cmd, int *num)
{
  char **retval = 0;
  int size = 0;
  char *tok = "\n";
  char *result, *string;
  char buf[BUFSIZ];
  FILE *stream;
  int k2;

  *num = 0;

  if ((stream = popen(cmd,"r")) != NULL) {
    while (fgets(buf,BUFSIZ,stream) != NULL) {
      if (*num == size) {
	char **tmp = (char **)calloc (size+INCR, sizeof(char*));
	for (k2 = 0; k2 < size; k2++) {
	  tmp[k2] = retval[k2];
	}
	free (retval);
	retval = tmp;
	size += INCR;
      }
      result = (char *)strtok (buf, tok);
      if (result) {
	string = (char *)malloc ((strlen(result)+1)*sizeof(char));
	strcpy (string, result);
	retval[*num] = string;
	(*num)++;
      }
    }
    pclose (stream);
  }
  return retval;
}

Tokens *createTokens (char *path, char *pattern)
{
  Tokens *tokens;

  int k2;
  char *atok, *pattern0;
  const char *tok = "*";
  char *tmp = 0;

  tokens = (Tokens *)malloc (sizeof(Tokens));
  assert (tokens);

  if (!pattern || strlen(pattern) <= 0) {
    tokens->num_toks = 0;
    tokens->wild_beg = 0;
    tokens->wild_end = 0;
    tokens->pattern = 0;
  }
  else {
    tokens->wild_beg = pattern[0] == '*';
    tokens->wild_end = pattern[strlen(pattern)-1] == '*';
    tokens->pattern = (char *)malloc ((strlen(pattern)+1)*sizeof(char));
    strcpy (tokens->pattern, pattern);
    pattern0 = (char *)malloc ((strlen(pattern)+1)*sizeof(char));
    strcpy (pattern0, tokens->pattern);
    // count the number of tokens in between
    for (tokens->num_toks = 0, atok = strtok(pattern0, tok); atok;
      (tokens->num_toks)++) {
      atok = strtok (NULL, tok);
    }
    // now store the tokens
    strcpy (pattern0, tokens->pattern);
    tokens->toks = (char **)malloc (tokens->num_toks*sizeof(char*));
    for (k2 = 0, atok = strtok(pattern0,tok); k2 < tokens->num_toks;
      k2++) {
      assert (atok);
      if ((k2 == 0) && !tokens->wild_beg) {
	tmp = (char *)malloc ((strlen(atok)+strlen(path)+1)*sizeof(char));
	strcpy (tmp, path);
	strcat (tmp, atok);
	atok = tmp;
      }
      tokens->toks[k2] = (char *)malloc ((strlen(atok)+1)*sizeof(char));
      strcpy (tokens->toks[k2], (const char *)atok);
      atok = strtok (NULL, tok);
    }
  }
  if (tmp) free (tmp);
  return tokens;
}

void deleteTokens (Tokens *dis)
{
  int k2;

  assert (dis);

  free (dis->pattern);

  for (k2 = 0; k2 < dis->num_toks; k2++) {
    free (dis->toks[k2]);
  }
  free (dis->toks);
  free (dis);
  dis = 0;
}

int haveTokens (Tokens *dis, char *pattern)
{
  int retval;

  if (dis) {
    retval = !strcmp (dis->pattern, pattern);
  }
  else {
    retval = 0;
  }
  return retval;
}

int withinPattern (Tokens *dis, char *file)
{
  int k2, len0, len1;
  int retval = 1;
  char *atok = 0;
  int itok = '/';
  char tok = (char)itok;

  if (file) {
    if (dis->num_toks > 0) {
      if (!dis->wild_beg) {
	len0 = strlen (dis->toks[0]);
	if (strncmp(file,dis->toks[0],len0)) {
	  retval = 0;
	}
	atok = file; /* include path when initial wild card not present */
      }
      else {
	atok = strrchr (file, tok);
	atok++;      /* exclude path when initial wild card present */
      }
      if (!dis->wild_end) {
	len0 = strlen (dis->toks[dis->num_toks-1]);
	len1 = strlen (file);
	if (len1 >= len0) {
	  if (strncmp(&(file[len1-len0]),dis->toks[dis->num_toks-1],
            len0)) {
	    retval = 0;
	  }
        }
        else {
	  retval = 0;
	}
      }
      for (k2 = 0; retval && k2 < dis->num_toks; k2++) {
        atok = strstr (atok, (const char *)dis->toks[k2]);
	if (!atok) {
	  retval = 0;
	}
      }
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

int quickStat (StatData *status)
{
  int retval;

  doLstat (status);

  retval = status->error;
  return retval;
}

void doLstat (void *data)
{
  StatData *status = (StatData *)data;
  char *filename;
  char buf[BUFSIZ];
  int error;

  filename = (char *)malloc ((strlen(status->full_path)+1)*sizeof(char));
  strcpy (filename, status->full_path);

  error = lstat (filename, status->status);
  if (error) {
    sprintf (buf, "On %s: ", filename);
    strcat (buf, lstatErrorString(errno));
    strcat (buf, strerror(errno));
    printf ("%s\n", buf);
  }
  free (filename);

  status->error = error;
}

char *lstatErrorString (int error)
{
  return errorString ("lstat", error);
}

char *statErrorString (int error)
{
  return errorString ("stat", error);
}

char *errorString (const char *stat_fcn, int error)
{
  char retval[BUFSIZ];

  strcpy (retval, stat_fcn);
  switch (error) {
  case 0 :
    strcat (retval, ": Normal return ");
    break;
  case EOVERFLOW :
    strcat (retval, " error: EOVERFLOW - file size overflow ");
    break;
  case EACCES :
    strcat (retval, " error: EACCES - inadequate search permission ");
    break;
  case EFAULT :
    strcat (retval, " error: EFAULT - illegal address ");
    break;
  case EINTR :
    strcat (retval, " error: EINTR - signal caught ");
    break;
  case ELOOP :
    strcat (retval, " error: ELOOP - too many symbolic links in path ");
    break;
  case ENAMETOOLONG :
    strcat (retval, " error: ENAMETOOLONG - path too long ");
    break;
  case ENOENT :
    strcat (retval, " error: ENOENT - file does not exist ");
    break;
  case ENOLINK :
    strcat (retval, " error: ENOLINK - link is no longer active ");
    break;
  case ENOTDIR :
    strcat (retval, " error: ENOTDIR - path prefix not a directory ");
    break;
  default:
    strcat (retval, " error: ? ");
    break;
  }
  return retval;
}

time_t timeLastModified (char *full_path)
{
  time_t retval;
  char buf[BUFSIZ];
  int error;
  StatData *stat_data = createStatData ();

  error = stat (full_path, stat_data->status);
  if (error) {
    sprintf (buf, "On %s: ", full_path);
    strcat (buf, statErrorString(errno));
    strcat (buf, strerror(errno));
    printf ("%s\n", buf);
    retval = (time_t)NULL;
  }
  else {
    retval = stat_data->status->st_mtime;
  }
  deleteStatData (stat_data);
  return retval;
}

/*
int main (int argc, char *argv[]) {
  int k2, num, num_dirs, num_no_dirs, num_patts;
  int *file_types;
  char **files;
  char **directories;
  char **files_no_dirs;
  char **pattern_files;
  char *path;
  char *def_path = "./";
  char *def_pattern = "*";
  char *pattern;
  char buf[BUFSIZ];
  Tokens *tokens;

  if (argc > 2) {
    pattern = (char *)malloc ((strlen(argv[2])+1)*sizeof(char));
    strcpy (pattern, argv[2]);
    path = (char *)malloc ((strlen(argv[1])+1)*sizeof(char));
    strcpy (path, argv[1]);
  }
  else if (argc > 1) {
    pattern = (char *)malloc ((strlen(def_pattern)+1)*sizeof(char));
    strcpy (pattern, def_pattern);
    path = (char *)malloc ((strlen(argv[1])+1)*sizeof(char));
    strcpy (path, argv[1]);
  }
  else {
    pattern = (char *)malloc ((strlen(def_pattern)+1)*sizeof(char));
    strcpy (pattern, def_pattern);
    path = (char *)malloc ((strlen(def_path)+1)*sizeof(char));
    strcpy (path, def_path);
  }

  files = listAllFiles (path, &num);

  if (num > 0) {
    file_types = getFileTypes (path, num, files);
    directories = getAllDirectories (file_types, num, files, &num_dirs);
    files_no_dirs = getAllFilesAndLinks (file_types, num, files,
      &num_no_dirs);

    tokens = createTokens (path, pattern);

    if (num_no_dirs > 0) {
      pattern_files = getPatternFiles (tokens, num_no_dirs, files_no_dirs,
        &num_patts);

      if (num_patts) {
	for (k2 = 0; k2 < num_patts; k2++) {
	  printf ("%s\n", pattern_files[k2]);
	  free (pattern_files[k2]);
	}
	free (pattern_files);
      }

      for (k2 = 0; k2 < num_no_dirs; k2++) {
	free (files_no_dirs[k2]);
      }
      free (files_no_dirs);
    }

    for (k2 = 0; k2 < num; k2++) {
      free (files[k2]);
    }
    free (files);

    free (file_types);

    if (num_dirs) {
      for (k2 = 0; k2 < num_dirs; k2++) {
	free (directories[k2]);
      }
      free (directories);
    }

    deleteTokens (tokens);
  }

  free (path);
  free (pattern);
  return 0;
}
*/
