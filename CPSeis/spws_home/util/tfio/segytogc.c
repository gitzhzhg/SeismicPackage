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
#include "segytogc.h"


/*
                             segytogc.c
C************************ COPYRIGHT NOTICE ****************************
C      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
C       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
C************************ COPYRIGHT NOTICE ****************************
C\USER DOC
C----------------------------------------------------------------------
C                   SEISMIC PROCESSING WORKSTATION
C                         C-LANGUAGE UTILITY 
C            designed to be called from C or C++
C
C Program name:  segytogc    construct a Gocad header for SEGY
C                            trace data -OR- scan segy traces
C                            and gather header information 
C
C Subdirectory:  tfio 
C Libraries:     libtfio.a libcpsprim.a libcprim.a libtransf.a
C Header file:   segytogc.h
C                ( tfdefs.h tf_hstat.h tf_global.h tf3d.h )
C Source file:   segytogc.c
C
C Written:       98/05/18  by:  R.S.Day
C Last revised:  98/05/18
C
C Purpose:       Create a Gocad Voxet header for a SEGY file.
C
C----------------------------------------------------------------------
C                      GENERAL INFORMATION
C
C segytogc is a stand-alone program that scans data from a segy trace
C file and outputs a Gocad Voxet header to stdout. 
C
C segytogc assumes that the seismic traces lie on a regular grid. If this
C is not the case, then the Voxet header file will be inaccurate.
C
C----------------------------------------------------------------------
C                  COMMAND LINE OPTIONS
C
C -p    Option to print out header statisitcs
C -h    Option to print out help message
C -d    Option to force prompting for dcode input of variables
C 
C                  INPUT AND OUTPUT PARAMETERS
C
C segytogc can take its arguments from a dcode list on stdin or
C from command line arguments.
C
C  Parameter   Default   Legend
C  file        NONE      Input file name.
C  xhdr          7       CPS header word to use for x-bin description.
C  yhdr          8       CPS header word to use for y-bin description.
C  nscan       1000      Traces to scan for determining cube grid.
C----------------------------------------------------------------------
C
C----------------------------------------------------------------------
C                    TO DO SUCH AND SUCH
C
C  1. To get help type:
C
C    segytogc -h
C
C  2. To read in a SEGY file & output a voxet header
C
C    segytogc < tparms
C            or
C    segytogc file_name
C
C    The file tparms should contain the dcode list. For example
C
C    file=your_input_file nscan=600
C
C  3. To print header statistics type:
C
C    segytogc -p file_name
C----------------------------------------------------------------------
C                       REVISION HISTORY
C
C    Date      Author     Description
C    ----      ------     -----------
C 2.
C 1. 98/05/18  R.S.Day    Initial version.
C----------------------------------------------------------------------
C\END DOC
*/

void main(int argc, char **argv)
{
 char  seisin[96],voxname[64];
 int   i,xhdr=7, yhdr=8;
 int   aok, ftyp, istat;
 int   nscan=1000, traces;
 Grid3DDesc *g3d=0;
 TF_Global  *global=0;
 HdrStats   *stats  =0;

/* process command line arguments */
 strcpy(seisin,"NONE");
 strcpy(voxname,"sgyvox");
 aok=0;
 if(argc > 1 ) {
   if(strcmp(argv[1],"-h") == 0) aok=1;
   if(strcmp(argv[1],"-d") == 0) aok=1;
   if(strcmp(argv[1],"-p") == 0) aok=1;
   if(aok==1) {
    if(strcmp(argv[1],"-h") == 0) {
     segytogc_help(argc, argv);
     exit(0);
    }
    if(strcmp(argv[1],"-d") == 0) {
     segytogc_dcode(seisin, &xhdr,&yhdr,&nscan);
    }
    if(strcmp(argv[1],"-p") == 0) {
     if(argc > 2) strcpy(seisin,argv[2]);
     else segytogc_dcode(seisin, &xhdr,&yhdr,&nscan);
    }
   } else {
    strcpy(seisin,argv[1]);
   }
 } else { /* assume decode style input */
     segytogc_dcode(seisin, &xhdr,&yhdr,&nscan);
 }

/*
 *  Check the file type.
 *  SEGY and GOCAD file types are acceptable.
 *  No further action needed if a GOCAD file
 */ 
 if(strcmp(seisin,"NONE")==0) {
  printf("# File=%s\n",seisin);
  segytogc_usage( argc, argv);
  exit(0);
 }
 ftyp = getgl_ftype(seisin);
 if(ftyp!= GOCAD_TYPE && ftyp != SEGY_TYPE) {
  printf("# file type has to be GOCAD or SEGY\n"); 
  exit(0);
 }
 if(ftyp== GOCAD_TYPE) exit(0);

/*
 *  Lets get some information about our file!
 */
 global  = get_global_info(seisin);
 if(!global) {
    printf("# segytogc: get_global_info call failed\n");
    goto error;
 }

/*
 *  Scan the file and extract information about the headers.
 *  returns min, max, and  min bin size for each header
 */
 segytogc_comments(seisin , voxname);
 traces = tf_global_get_ntrfil(global);
 nscan = (nscan > traces) ? traces: nscan;

 stats = tf_hstats_ginfo1(seisin, global, nscan);
 if(argc > 1 && strcmp(argv[1],"-p")==0)
  {tf_hstats_pr(seisin,stats);
   exit(0);
  }

/*
 * Write the ascii header file for the voxet.
 */
 g3d = segytogc_grid(seisin,voxname, stats,global,xhdr,yhdr);

 segytogc_hdr( g3d);



 tf_hstats_del(stats);
 if(g3d) free(g3d);
 if(global) free(global);
 exit(0);
error:
 tf_hstats_del(stats);
 if(g3d) free(g3d);
 if(global) free(global);
 exit(1);

}

void segytogc_help(int argc, char **argv)
{
 if(argc >1)
 printf("#     **** Tutorial for %s ****\n",argv[0]);
 printf("#      Build Gocad Voxet Header for SEGY file.\n");
 printf("#                 -OR-\n");
 printf("#      Scan SEGY file for header statistic.\n");
 printf("#      Takes its input from stdin or argument list.\n");
 printf("#      Input is in the form of a dcode list.\n");
 printf("#      i.e. KEYWORD1=VALUE1 KEYWORD2=VALUE2 ...\n");
 printf("#      Keyword 1: \'file\'   (NONE)\n");
 printf("#       file = the name of the input seismic file\n");
 printf("#      Keyword 2: \'xhdr\'     (17)\n");
 printf("#       xhdr   = use this header word for the x binning\n");
 printf("#      Keyword 3: \'yhdr\'     (18)\n");
 printf("#       yhdr   = use this header word for the y binning\n");
 printf("#      Keyword 4: \'nscan\'     (18)\n");
 printf("#       nscan  = number of traces for header scan\n");

}

void segytogc_usage(int argc, char **argv)
{
 printf("      **** %s usage ****\n",argv[0]);
 printf("       %s [-d] | [-h] | [-p file_name] | [file_name]\n",argv[0]);
 printf("       [-h] is for help\n");
 printf("       [-p file_name] for printing header statistics to standard out\n");
 printf("       [-d] for getting parameters via decode\n");

}

Grid3DDesc *segytogc_grid(char *fname, char *oname,
            HdrStats *stats,TF_Global *global,
            int xhdr, int yhdr)
{/* determine the output grid size */
 Grid3DDesc *g3d;
 int   n1,n2,n3,ntrfil;
 float o1,o2,o3, d1,d2,d3;
 
 if(!stats || !global) return NULL;
 g3d = (Grid3DDesc *)  malloc(sizeof(Grid3DDesc));
 ntrfil = tf_global_get_ntrfil(global);

 n1 = tf_global_get_ndptr(global);
 o1 = (float) tf_global_get_tstrt(global);
 d1 = global->srval;
/*tf_global_get_srval(global);*/

 d2 = stats->hbin[xhdr-1];
 o2 = stats->hmin[xhdr-1];
 n2 = 1 + (stats->hmax[xhdr-1] - o2 + 0.01*d2)/d2;
 if(n2<1) n2 = 1;

 n3 = 1 + (ntrfil-1)/n2;
 o3 = stats->hmin[yhdr-1];
 d3 = stats->hbin[yhdr-1];

 g3d = tf3d_create_desc(fname, fname,
       "segyamp", oname,
       o1, o2, o3,
       n1, n2, n3,
       d1, d2, d3,
       "U","V","W",
       2, 3, 1,
       SEGY_TYPE, tf_global_get_wdtyp(global));

 return g3d;
}

void segytogc_hdr(Grid3DDesc *g3d)
{/* needs linked with libmodel.a */
 float b1,b2,b3;
 Pt3Di *N;
 Pt3Df *O,*D;
 Pt3Ds *axis;
 if(!g3d) return;


 N = tf3d_getN(g3d);
 O = tf3d_getO(g3d);
 D = tf3d_getD(g3d);
 axis = tf3d_getAxisNames(g3d);

 b1 = O->v[0] + (N->v[0]-1)*D->v[0];
 b2 = O->v[1] + (N->v[1]-1)*D->v[1];
 b3 = O->v[2] + (N->v[2]-1)*D->v[2];
 printf("GOCAD VOXET 1.0\n");
 printf("HEADER {\n");
 printf("name:%s\n",tf3d_getObjectName(g3d));
 printf("*axis:on\n");
 printf("}\n");
 printf("AXIS_O %f %f %f\n",O->v[0],O->v[1],O->v[2]);
 printf("AXIS_U %f %f %f\n",0.0,0.0,b1);
 printf("AXIS_V %f %f %f\n",b2,0.0,0.0);
 printf("AXIS_W %f %f %f\n",0.0,b3,0.0);
 printf("AXIS_N %d %d %d\n",N->v[0],N->v[1],N->v[2]);
/* printf("AXIS_D %f %f %f\n",d1,d2,d3); */
 printf("AXIS_MIN 0  0  0\n");
 printf("AXIS_MAX 1  1  1\n");
 printf("AXIS_NAME \"%s\" \"%s\" \"%s\"\n",
    axis->v[0],axis->v[1],axis->v[2]);
 printf("AXIS_TYPE even even even\n");
 printf("\n");
 printf("PROPERTY 1 \"%s\"\n",tf3d_getPropertyName(g3d));
 printf("PROP_FILE 1 %s\n",tf3d_getDataFile(g3d));
 printf("PROP_FORMAT 1 SEGY\n");
 printf("PROP_ETYPE 1 IBM\n");
 printf("PROP_ESIZE 1 4\n");
 printf("END\n");
 
}

void segytogc_dcode(char *fnamei,int  *xhdr,int  *yhdr, int *nscan) {
 char strin[240],*s,*fin;
 int  i, *hdr=NULL;
 strcpy(fnamei,"NONE");
 printf("# Default Parameters\n");
 printf("#  file=%s\n",fnamei);
 printf("#  xhdr=%d yhdr=%d\n",*xhdr,*yhdr);
 printf("#  nscan=%d\n",*nscan);
 while( (s = gets(strin))!= NULL )
  {i = dcdut_dcode((void **) &fin,"file",'c',s);
   if(i) strcpy(fnamei,fin);
   if(fin) free(fin);
   i = dcdut_dcode((void **) &hdr,"xhdr",'i',s);
   if(i) *xhdr = (int) *hdr;
   if(hdr) { free(hdr); hdr=0; }
   i = dcdut_dcode((void **) &hdr,"yhdr",'i',s);
   if(i) *yhdr = (int) *hdr;
   if(hdr) { free(hdr); hdr=0; }
   i = dcdut_dcode((void **) &hdr,"nscan",'i',s);
   if(i) *nscan = (int) *hdr;
   if(hdr) { free(hdr); hdr=0; }
  }
 printf("# Actual Parameters\n");
 printf("#  file=%s \n",fnamei);
 printf("#  xhdr=%d yhdr=%d\n",*xhdr,*yhdr);
 printf("#  nscan=%d\n",*nscan);
}


void segytogc_comments(char *file, char *oname) {
 size_t it;
 time_t current;
 struct tm *now;
 char  date[16],*nn;
 char bname[32],path[80],*ext;
 current = time(NULL);
 now     = localtime(&current);
 it      = strftime(date,11,"%Y/%m/%d",now);
 printf("# Gocad Voxet header construction\n");
 printf("# FILE: %s\n",file);
 printf("# Date: %s\n",date);
 nn = dskio_parse_file(file,bname,path);
 oname[0]='\0';
 strcpy(oname,bname);
 ext = strstr(bname,".");
 if(ext) {
  strncpy(oname,bname,ext-bname);
  oname[ext-bname]='\0';
 }
 printf("# Name: %s\n",oname);
}


