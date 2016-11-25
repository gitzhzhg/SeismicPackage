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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "cprim.h"
#include "c2f_interface.h"

#ifdef NEED_CAPITALS
#define rmod_unix RMOD_UNIX
#endif

#ifdef NEED_UNDERSCORE
#define rmod_unix rmod_unix_
#endif

#ifdef __cplusplus
extern "C" {                 // for C++
#endif

void rmod_unix(int *m_work, char *work,
     int *m_grid, char *grid, char *z_lay);
void rmod_main_usage(int argc, char **argv);
void rmod_main_help(int argc, char **argv);
void rmod_main_parms(int *mem_work, int *mem_grid);

#ifdef __cplusplus
}                 // for C++
#endif

void main(int argc, char **argv) {

 static int m_work=100000, m_grid=100000, m_var=10000;
 static char *work, *grid;
 int i,n;

 
 i=1;
 while(i < argc) {
   if(argc>1) {
    if(strcmp(argv[i],"-h") == 0) {
      rmod_main_help(argc, argv);
      exit(0);
    }
    else if(strcmp(argv[i],"-m") == 0) {
      rmod_main_parms(&m_work, &m_grid);
      i++;
    }
    else if(strcmp(argv[i],"-s") == 0) {
      sscanf(argv[i+1],"%d", &m_grid);
      m_work= m_grid;
      printf(" m_work=%d \n m_grid=%d\n",m_work,m_grid);
      i += 2;
    }
    else {
      rmod_main_usage(argc, argv);
      exit(1);
    }
  }
 }

  work = (char *) malloc(m_work*sizeof(float));
  grid = (char *) malloc(2*m_grid*sizeof(float));
  if(!work || !grid) {
     printf("failed to allocate work space\n");
     exit(1);
  }

  n= m_grid*sizeof(float);
  rmod_unix(&m_work, work, &m_grid, grid, grid+n);
 free(work);
 free(grid);
 exit(0);
}

void rmod_main_usage(int argc, char **argv)
{
 printf("      **** %s usage ****\n",argv[0]);
 printf("       %s [-h][-m] \n",argv[0]);
 printf("       [-h] is for help\n");
 printf("       [-m] is for memory options\n");

}

void rmod_main_help(int argc, char **argv)
{
 if(argc >1)
  printf("      **** Tutorial for %s ****\n",argv[0]);
 printf("       Purpose: run the RMOD utilities.\n");
 printf("       Options:\n");
 printf("         -h  prints this help message.\n");
 printf("         -m  queries user for memory limits.\n");
 printf("       Memory input parameters\n");
 printf("         Input is from standard in, and is ended by cntrl-D.\n");
 printf("         Input is in the form of a dcode list.\n");
 printf("         i.e. KEYWORD1=VALUE1 KEYWORD2=VALUE2 ...\n");
 printf("         Keyword 1: \'mem_work=100000\'\n");
 printf("          mem_work = amount of working memory to allocate\n");
 printf("         Keyword 2: \'mem_grid=100000\'\n");
 printf("          mem_grid = amount of grid memory to allocate\n");

}


void rmod_main_parms(int *mem_work, int *mem_grid)
{
 char strin[240],*s;
 int   i,j,nkeys=2;
 char *keywords[2] = {"mem_work","mem_grid"};
 void *keyvals[2]  = {NULL,NULL};
 long  keytypes[2] = {'d','d'};

 *mem_work = 100000;
 *mem_grid = 100000;
 printf("Default Parameters\n");
 printf(" %s=%d \n %s=%d\n",keywords[0],*mem_work,keywords[1],*mem_grid);
 while( (s = fgets(strin,80,stdin))!= NULL )
  {
   for(j=0;j<nkeys;j++)
    { i = dcdut_dcode(&keyvals[j],keywords[j],keytypes[j],s);
      if(i && (j==0))  *mem_work = *(int *) keyvals[0];
      if(i && (j==1))  *mem_grid = *(int *) keyvals[1];
      if(keyvals[j]) { free(keyvals[j]); keyvals[j]=NULL; }
    }
  }
 printf("Actual Parameters\n");
 printf(" %s=%d \n %s=%d\n",keywords[0],*mem_work,keywords[1],*mem_grid);
}

