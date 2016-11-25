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
#include "transform.h"

/* 
c        1         2         3         4         5         6         7
|
C\USER DOC
C----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                       C P S   P R I M I T I V E
C
C   Primitive names: ErsTransName(), ErsTransHeader(),
C                    ErsTransSet(),  ErsTransGet(),
C                    ErsTransGetnth()
C                    ErsTransGetTran()
C                    ErsTrans_phys_name()
C
C                    new_coord(),    destroy_coord()
C                    Set_nc(),       Get_nc()
C                    Set_uc(),       Get_uc()
C                    Set_dc(),       Get_dc()
C                    
C  Source directory: ~spws/util/transform/
C           Library: NEWLIB & libtransf.a
C            Author: R.S.Day
C      Date Written: 92/10/12
C      Last revised: 97/06/05  by:  R.S.Day
C
C  Purpose:      Manipulate the coordinate transformation objects
C                that are represented by the structures ErsCoordInf,
C                ErsTransforms, and ErsTransform. The ErsCoordInf
C                structure contains information for normal, device
C                and user coordinates(i.e. GKS like). The ErsTransfoms
C                structure carries the transformation information
C                that is contained in CPS RMOD files.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                CALLING SEQUENCE
C  int transforms_wr(char *file,ErsTransforms *T);
C  int transforms_wr_stream(FILE *fpntr,ErsTransforms *T);
C  ErsTransforms *transforms_rd(char *file);
C  ErsTransforms *transforms_parse(char *buf);
C  ErsTransforms *new_transforms();
C  void destroy_transforms(ErsTransforms *);
C  ErsTransform *transforms_getnth(ErsTransforms *,int );
C  void ErsTrans_phys_name(char *name, char *outname);
C  int transforms_nametopos(ErsTransforms *tdata,char *name);
C  int transforms_pntrtopos(ErsTransforms *tdata,ErsTransform *);
C
C  ErsTransform *new_transform();
C  void  destroy_transform(ErsTransform *trans);
C  char *transform_getname(ErsTransform *t);
C  char *transform_getunits(ErsTransform *t);
C  void  transform_setname(ErsTransform *t,char *name);
C  void  transform_setunits(ErsTransform *t,char *name);
C  void  transform_getx(ErsTransform *t, float *x1,float *x2);
C  void  transform_setx(ErsTransform *t, float x1,float x2);
C  void  transform_prt(ErsTransform *t);
C  float transform_cnvrt(ErsTransform *ti,float xi, ErsTransform *to)
C  long  transform_mcnvrt(ErsTransform *ti,float *xi, int n, ErsTransform *to)
C  float transform_scale(ErsTransform *ti,float delta, ErsTransform *to)
C  int   transform_coeff(ErsTransform *ti,ErsTransform *to,float *sc,
C        float *io, float *oo)
C
C
C  Type    Name    I/O     Valid     Description
C  ----    ----    ---     -----     -----------
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. ErsTransName & ErsTransHeader match a CPS header value with a
C     transformation name and vice-versa.
C  2. ErsTransSet() & ErsTransGet() set or get the components of a
C     transformation structure(see transform.h for details)
C  3. ErsTransGetnth gets the nth transform from an ErsTransforms struct
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2. 97/06/05  R. Day     Added functions for I/O of RMOD files
C  1. 92/10/12  R. Day     Initial version
C-----------------------------------------------------------------------
C  NOTES
C
C      Language:      C
C  Header files:      transform.h
C-----------------------------------------------------------------------
C               EXTERNALS REFERENCED BY THIS PRIMITIVE
C
C                                none
C-----------------------------------------------------------------------
C\END DOC
C***********************************************************************/

/* output data to an RMOD ascii file */
int transforms_wr(char *file,ErsTransforms *T) {
 FILE *fpntr;
 int i=0;
 if(!file || !T) return i;
 if(strcmp(file,"NONE")==0) return i;
 if(ErsTrans_count(T)==0) return i;
 fpntr = fopen(file,"w+");
 if(!fpntr) return i;
 i = transforms_wr_stream(fpntr,T);
 fclose(fpntr);
 return i;
}

/* output data to an RMOD ascii file */
int transforms_wr_stream(FILE *fpntr,ErsTransforms *T) {
 ErsTransform *t;
 char line[120],*name,s1[24],s2[24];
 float x1,x2;
 int i,n;
 if(!fpntr || !T) return 0;
 n = (int) ErsTrans_count(T);
 if(n==0) return 0;
 fprintf(fpntr,"%s\n"," *TRANSFORM SECTION");
 fprintf(fpntr," NUMBER OF TRANSFORMATION COORDINATES = %d\n",n);
 for(i=0;i<n;i++) {
  t = transforms_getnth(T,i);
  if(t) {
  name =  transform_getname(t);
  sprintf(s1,"%s1",name);
  sprintf(s2,"%s2",name);
  transform_getx(t , &x1,&x2);
  sprintf(line," %-16s =  %-15g     %-16s =   %-15g",s1,x1,s1,x2);
  fprintf(fpntr,"%s\n",line);
  }
 }
 return 1;
}

/* read in the *TRANSFORM SECTION of an RMOD file */
ErsTransforms *transforms_rd(char *file) {
  ErsTransforms *T=0;
  FILE *fpntr=0;
  char *buf=0,line[120];

  if(!file) return T;
  if(strcmp(file,"NONE")==0) return T;
  buf = (char *) malloc(5000*sizeof(char));
  if(!buf) return T;
  buf[0]='\0';
  fpntr = fopen(file,"r");
  if(!fpntr) {free(buf); return T;}
  while(fgets(line,120,fpntr)!= 0) {
   if(strstr(line,"*TRANSFORM")!= NULL) {
    strcpy(buf,line);
    while(fgets(line,120,fpntr) != 0) {
     if(strchr(line,'*') == 0) {
       strcat(buf,line);
     } else {
      goto LL;
     }
    }
   }
  }
 LL:
 fclose(fpntr);
 strcat(buf,"*\n");

 T = transforms_parse(buf);
 if(buf) free(buf);
 return T;
}

/* parse a char string that contains RMOD TRANSFORM data */
ErsTransforms *transforms_parse(char *buf) {
  ErsTransforms *T=0;
  int nc=0,l,hd;
  char *un=0;
  char *t=0,*s=0,*e=0,line[120];
  char l1[16],l2[16];
  float x1,x2;
  char *jnk;

  t = strstr(buf,"*TRANSFORM");
  if(!t) return T;
  s = strstr(t,"NUMBER OF TRANSFORMATION");
  if(s) s=strstr(s,"=");
  sscanf(s+1,"%d",&nc);
  if(nc < 1) return T;

  T = new_transforms();
  transforms_adddef(T);
 L1:
  s = strstr(s,"\n");
  s +=1;
  e = strstr(s,"\n");
  strncpy(line,s,e-s+1);
  line[e-s+2]='\0';
  if((jnk = strchr(line,'*')) !=0) return T;
  sscanf(line,"%s%*s%g%s%*s%g",l1,&x1,l2,&x2);
  l = strlen(l1);
  l1[l-1]='\0';
  ErsTransHeader(&hd,l1);
  ErsTransSet(T,l1,x1,x2,hd,un);
  goto L1;

 return T;/*Solaris 6.2 complains this is never reached*/
}

void ErsTransName(int header, char *name)
{ /* return the name given the header value */
   name[0]='\0';
   if(header ==SEQHDR_)  { strcpy(name,"XTRACE"); return; }
   if(header ==OFFSET_)  { strcpy(name,"OFFSET"); return; }
   if(header ==XGRIDHDR_){ strcpy(name,"XGRID"); return; }
   if(header ==XBASEHDR_){ strcpy(name,"XBASEMENT"); return; }
   if(header ==XANNOHDR_){ strcpy(name,"XANNOTATION"); return; }
   if(header ==YGRIDHDR_){ strcpy(name,"YGRID"); return; }
   if(header ==YBASEHDR_){ strcpy(name,"YBASEMENT"); return; }
   if(header ==YANNOHDR_){ strcpy(name,"YANNOTATION"); return; }
   if(header ==UNDEFHDR){ strcpy(name,"UNDEF"); return; }
   if(header ==TIMEHDR_) { strcpy(name,"TIME"); return; }
   if(header ==DPTHHDR_) { strcpy(name,"DEPTH"); return; }
   if(header ==METER_) { strcpy(name,"METER"); return; }
   if(header ==KMETER_) { strcpy(name,"KILOMETER"); return; }
   if(header ==FEET_) { strcpy(name,"FEET"); return; }
   if(header ==KFEET_) { strcpy(name,"KILOFEET"); return; }
   sprintf(name,"HDR:%-2.2d",header);
  return;
}

void ErsTransHeader(int *header, char *name)
{ /* return the header given the transformation name */
    *header = 0;
    if(strcmp(name,"XTRACE")==0)     {*header =SEQHDR_;    return; }
    if(strcmp(name,"OFFSET")==0)     {*header =OFFSET_;    return; }
    if(strcmp(name,"XGRID")==0)      {*header =XGRIDHDR_;  return; }
    if(strcmp(name,"YGRID")==0)      {*header =YGRIDHDR_;  return; }
    if(strcmp(name,"XBASEMENT")==0)  {*header =XBASEHDR_;  return; }
    if(strcmp(name,"YBASEMENT")==0)  {*header =YBASEHDR_;  return; }
    if(strcmp(name,"XANNOTATION")==0){*header =XANNOHDR_;  return; }
    if(strcmp(name,"YANNOTATION")==0){*header =YANNOHDR_;  return; }
    if(strcmp(name,"UNDEF")==0)      {*header =UNDEFHDR ;  return; }
    if(strcmp(name,"TIME")==0)       {*header=TIMEHDR_;    return; }
    if(strcmp(name,"DEPTH")==0)      {*header=DPTHHDR_;    return; }
    if(strcmp(name,"FEET")==0)       {*header=FEET_;    return; }
    if(strcmp(name,"KILOFEET")==0)   {*header=KFEET_;    return; }
    if(strcmp(name,"METER")==0)      {*header=METER_;    return; }
    if(strcmp(name,"KILOMETER")==0)  {*header=KMETER_;    return; }
    if(strncmp(name,"HDR:",4)==0)
     { sscanf(name+4,"%d", header );}
 return;
}
/*****************************************
 * Methods for ErsTransforms structure  ***
 ****************************************/
void ErsTransSet(ErsTransforms *transdata,char *tname,
        float x1,float x2,long header,char *units)
{ ErsTransform *transform;
  int i,ntrans;
/* Overwrites a match or adds to end */
 
 if(transdata == NULL || tname == NULL) return;
 ntrans    = transdata->ntrans;

 i=0;
 for(i=0;i<ntrans;i++)
   { if(strcmp(tname,transdata->transforms[i].tname)==0) break; }

 transform = &transdata->transforms[i];
 strcpy(transform->tname,tname);
 if(units != NULL) strcpy(transform->units,units);
 transform->x1 = x1;
 transform->x2 = x2;
 transform->header = header;

 if(i==ntrans) { transdata->ntrans +=1; }

 return;
}

Bool ErsTransGet(ErsTransforms *transdata,char *tname,
        float *x1,float *x2,long *header,char *units)
{ ErsTransform *transform;
  int i,ntrans;
/* Returns data from a match to tname */
 
 if(transdata == NULL || tname == NULL) return False;
 ntrans    = transdata->ntrans;

 i=0;
 for(i=0;i<ntrans;i++)
   { if(strcmp(tname,transdata->transforms[i].tname)==0) break; }
 if(i==ntrans) { return False; }

 transform = &transdata->transforms[i];
 *x1 = transform->x1;
 *x2 = transform->x2;
 *header = transform->header;
 if(units != NULL) strcpy(units,transform->units);


 return True;
}

ErsTransform *ErsTransByID(ErsTransforms *tdata, int id)
{ ErsTransform *transform=NULL;
  int i=0,ntrans;
/* Returns data from a match to id */
 if(tdata == NULL ) return transform;
 ntrans    = tdata->ntrans;

 for(i=0;i<ntrans;i++)
   { if(tdata->transforms[i].header == id) break; }
 if(i==ntrans) { return NULL; }

 transform = &tdata->transforms[i];
 return transform;
}

ErsTransform *ErsTransGetTran(ErsTransforms *transdata,char *tname)
{ ErsTransform *transform;
  int i,ntrans;
/* Returns data from a match to tname */

 if(transdata == NULL || tname == NULL) return NULL;
 ntrans    = transdata->ntrans;

 i=0;
 for(i=0;i<ntrans;i++)
   { if(strcmp(tname,transdata->transforms[i].tname)==0) break; }
 if(i==ntrans) { return NULL; }

 transform = &transdata->transforms[i];
 return transform;
}


Bool ErsTransGetnth(ErsTransforms *transdata,int n, char *tname,
        float *x1,float *x2,long *header, char *units)
{ ErsTransform *transform;

 if(transdata == NULL ) return False;
 if(n<1 || n >transdata->ntrans) return False;

 transform = &transdata->transforms[n-1];
 *x1 = transform->x1;
 *x2 = transform->x2;
 *header = transform->header;
 strcpy(tname,transform->tname);
 if(units != NULL) strcpy(units,transform->units);
 return True;
}

int  ErsTransAccess(ErsTransforms *tdata, int mcoord, float *xcoord,
            char *coord)
{int   ncoord;
 long  header;
 char  tname[24],string[24],units[16];
 float x1,x2;
 if(!tdata) return 0;
 if(!xcoord || !coord) return 0;
/* Copy the data from the ErsTansforms structure to
   ncoord, xcoord & coord. mcoord is the max size allowed.
 */
 ncoord  = 0;
 coord[0]='\0';
 while(ErsTransGetnth(tdata,ncoord+1,tname,&x1,&x2,&header,units)==True)
  { if(x2 == x1) x2   = x1+1.;
    xcoord[2*ncoord]  = x1;
    xcoord[2*ncoord+1]= x2;
    strcpy(string,"                ");
    strncpy(string,tname,strlen(tname));
    strcat(coord,string);
    ncoord++;
    if(ncoord==mcoord) break;
  }
 
 return ncoord;
}


long ErsTrans_count(ErsTransforms *transdata)
{ if(transdata == NULL) return 0;
  return transdata->ntrans;
}

void ErsTrans_phys_name(char *inname, char *outname)
{/* Map an input name, to a physical units output name*/
 /* NONE returned when no mapping is determined.    */
 char tmp[32];
 if(outname != NULL) strcpy(outname,"NONE");
 if(inname == NULL) return;
 if(strlen(inname)==0) return;
 if(strlen(inname) > 16) { strncpy(tmp,inname,15); tmp[16]='\0'; }
 else strcpy(tmp,inname);
 if (tmp[0] == 'X')
  {strcpy(outname,"XBASEMENT");}
 else if (tmp[0] == 'Y')
  {strcpy(outname,"YBASEMENT");}
 else
  {strcpy(outname,"DEPTH");
   if(strcmp(tmp,"UNKNOWN")==0) strcpy(outname,"NONE");
   if(strcmp(tmp,"NONE")==0) strcpy(outname,"NONE");
  }
}


ErsTransforms *new_transforms()
{ErsTransforms *tdata;
 ErsTransform  *tran;
 int i;
 tdata = (ErsTransforms *) calloc(1,sizeof(ErsTransforms));
 for(i=0;i<32;i++)
  {tran = TdatGet_nth(tdata,i);
   strcpy(TGet_name(tran),"UNKNOWN");
   TGet_x1(tran) = 0.0;
   TGet_x2(tran) = 1.0;
  }
 return tdata;
}

void destroy_transforms(ErsTransforms *tdata)
{int i;
 ErsTransform  *tran;
 if(tdata == NULL) return;
 for(i=0;i<tdata->ntrans;i++)
  {if(&tdata->transforms[i] != NULL)
    {
     tran = &tdata->transforms[i];
     transform_setname(tran,"UNKNOWN");
     transform_setx(tran,0.0,1.0);
    }
  }
 tdata->ntrans = 0;
 free(tdata);
 return;
}

ErsTransform *transforms_getnth(ErsTransforms *tdata,int n)
{
 if(tdata == NULL) return NULL;
 if(n >= tdata->ntrans) return NULL;
 if(n < 0) return NULL;
 return &tdata->transforms[n];
}

int transforms_nametopos(ErsTransforms *tdata,char *name)
{int i;
 if(tdata == NULL || name ==NULL) return 0;
 i=0;
 for(i=0;i<tdata->ntrans;i++)
   { if(strcmp(name,tdata->transforms[i].tname)==0) break; }
 if(i==tdata->ntrans) { return 0; }

 return i+1;
}

int transforms_pntrtopos(ErsTransforms *tdata,ErsTransform *t)
{int i;
 if(tdata == NULL || t ==NULL) return 0;
 i=0;
 for(i=0;i<tdata->ntrans;i++)
   { if(t==&tdata->transforms[i]) break; }
 if(i==tdata->ntrans) { return 0; }

 return i+1;
}

void transforms_set_count(ErsTransforms *tdata, long count)
{ if(tdata) tdata->ntrans = count;
}
long transforms_count(ErsTransforms *tdata)
{ if(tdata) return tdata->ntrans;
  return -1;
}

void transforms_adddef(ErsTransforms *tdata)
{ErsTransform *t;
 char *namelist[11] = {"DEPTH","KILOMETER","METER",
                      "TIME","SECOND",
                      "XANNOTATION","XBASEMENT","XGRID",
                      "YANNOTATION","YBASEMENT","YGRID"};
 int i,header;
 float x1,x2;

 if(tdata == NULL) return;
 for(i=0;i<11;i++)
  {
   t = ErsTransGetTran(tdata,namelist[i]);
   if(!t)
    {x1 = 0.0; x2 = 1.0;
     if(strcmp(namelist[i],"DEPTH")==0) x2 = 1000;
     if(strcmp(namelist[i],"METER")==0) x2 = 1000;
     ErsTransHeader(&header, namelist[i]);
     ErsTransSet(tdata,namelist[i], x1,x2,(long ) header,NULL);
    }
  }

}

int  transforms_rmname(ErsTransforms *tdata, char *name)
{int i,pos;
 ErsTransform *to,*from;
 if(tdata==NULL || name ==NULL) return -1;
 pos=0;
 for(i=0;i<tdata->ntrans;i++)
   { pos = i;
     if(strcmp(name,tdata->transforms[i].tname)==0) break;
   }
 if(pos==tdata->ntrans) { return tdata->ntrans; }
 for(i=pos;i<tdata->ntrans-1;i++)
   {to = &tdata->transforms[i];
    from = &tdata->transforms[i+1];
    memcpy(to,from,sizeof(ErsTransform));
   }
 tdata->ntrans -= 1;
 strcpy(tdata->transforms[tdata->ntrans].tname,"UNKNOWN");
 return tdata->ntrans; 
 
}

void transforms_copy(ErsTransforms *from, ErsTransforms *to)
{ErsTransform *tranto, *tranfrom;
 int i;
 if(from==NULL || to==NULL) return;
 for(i=0;i<from->ntrans;i++)
  { tranfrom = &from->transforms[i];
    tranto = &to->transforms[i];
    memcpy(tranto,tranfrom,sizeof(ErsTransform));
  }
 to->ntrans = from->ntrans;
}


/*****************************************
 * Methods for ErsTransform structure  ***
 ****************************************/
ErsTransform *new_transform()
{ErsTransform *trans;
 trans = (ErsTransform *) calloc(1,sizeof(ErsTransform));
 if(trans != NULL) strcpy(trans->tname,"UNKNOWN");
 return trans;
}

void destroy_transform(ErsTransform *trans)
{
 if(trans == NULL) return;
 free(trans);
 return;
}

char *transform_getname(ErsTransform *t)
{
 if(t==NULL) return NULL;
 return t->tname;
}
char *transform_getunits(ErsTransform *t)
{
 if(t==NULL) return NULL;
 return t->units;
}
long transform_gethdr(ErsTransform *t)
{
 if(t==NULL) return 0; /* corresponds to NILKEY, see modbase_data */
 return (long) t->header;
}
void transform_sethdr(ErsTransform *t, long hd)
{if(t==NULL) return;
 t->header = hd;
}


void transform_setname(ErsTransform *t,char *name)
{
 if(t==NULL || name==NULL) return;
 if(strlen(name)>15)
  { strncpy(t->tname,name,15); t->tname[15]='\0';}
 else strcpy(t->tname,name);
 return;
}
void transform_setunits(ErsTransform *t,char *name)
{
 if(t==NULL || name==NULL) return;
 if(strlen(name)>15)
  { strncpy(t->units,name,15); t->units[15]='\0';}
 else strcpy(t->units,name);
 return;
}

void transform_getx(ErsTransform *t, float *x1,float *x2)
{
 *x1 =0;
 *x2 =0;
 if(t==NULL ) return;
 *x1 = t->x1;
 *x2 = t->x2;
 return;
}
void transform_setx(ErsTransform *t, float x1,float x2)
{
 if(t==NULL ) return;
 t->x1 = x1;
 t->x2 = x2;
 return;
}
void transform_prt(ErsTransform *t)
{if(t==NULL)
  { printf("transform_prt: NULL transform\n");
    return;
  }
 printf("transform_prt:   name=%s units=%s\n",t->tname,t->units);
 printf("transform_prt: header=%d\n",t->header);
 printf("transform_prt:     x1=%g\n",t->x1);
 printf("transform_prt:     x2=%g\n",t->x2);
}

long transform_mcnvrt(ErsTransform *ti,float *xi, int n, ErsTransform *to)
{float x1i,x2i,x1o,x2o,scale;
 int   i;

 if(ti == NULL || to == NULL) return 0;
 if(ti == to) return 0;
 if(xi == NULL) return 1;
 x1i= ti->x1;
 x2i= ti->x2;
 if(x2i==x1i) return 1;
 x1o= to->x1;
 x2o= to->x2;
 scale =(x2o-x1o)/(x2i-x1i);
 
 for(i=0;i<n;i++)
  { xi[i] = x1o + scale*(xi[i]- x1i); }

 return 0;
}
float transform_cnvrt(ErsTransform *ti,float xi, ErsTransform *to)
{float x1i,x2i,x1o,x2o,xo,scale;

 if(ti == NULL || to == NULL) return xi;
 if(ti == to) return xi;
 x1i= ti->x1;
 x2i= ti->x2;
 if(x2i==x1i) return xi;
 x1o= to->x1;
 x2o= to->x2;
 scale =(x2o-x1o)/(x2i-x1i);

 xo = x1o + scale*(xi- x1i);
 return xo;
}
float transform_scale(ErsTransform *ti,float delta, ErsTransform *to)
{float x1i,x2i,x1o,x2o,scaled;

 if(ti == NULL || to == NULL) return delta;
 if(ti == to) return delta;
 x1i= ti->x1;
 x2i= ti->x2;
 if(x2i==x1i) return delta;
 x1o= to->x1;
 x2o= to->x2;

 scaled = ((x2o-x1o)/(x2i-x1i))*delta;
 return scaled;
}
 
int transform_coeff(ErsTransform *ti,ErsTransform *to,float *sc,
    float *io, float *oo)
{/* return val = 0 if all is OK */
 /* new_val = oo + sc*(old_val - io) :how to use the coeficients */
 float x1i,x2i,x1o,x2o;
 *oo = 0.;
 *io = *oo;
 *sc = 1.0;
 if(ti == NULL || to == NULL) return 1;
 if(ti == to) return 0;
 x1i= ti->x1;
 x2i= ti->x2;
 *io = x1i;
 if(x2i==x1i) return 0;
 x1o= to->x1;
 x2o= to->x2;
 *oo = x1o;
 *sc=(x2o - x1o)/(x2i - x1i);
 return 0;
}

/*****************************************
 * Methods for ErsCoordInf structure   ***
 ****************************************/
ErsCoordInf * new_coord()
{ErsCoordInf *coord;
 coord = (ErsCoordInf *) calloc(1,sizeof(ErsCoordInf));
 coord->xn1 = 0.0; coord->xn2 = 1.0;
 coord->yn1 = 0.0; coord->yn2 = 1.0;
 coord->xu1 = 0.0; coord->xu2 = 1.0;
 coord->yu1 = 0.0; coord->yu2 = 1.0;
 return coord;
}
void destroy_coord(ErsCoordInf *coord)
{if(coord == NULL) return;
 free(coord);
 return;
}

void Get_nc(ErsCoordInf *T, float *x)
{ /* returns xn1,xn2,yn1,yn2 in x */
 if(T == NULL || x == NULL) return;
 x[0] = T->xn1; x[1] = T->xn2;
 x[2] = T->yn1; x[3] = T->yn2;
}
void Set_nc(ErsCoordInf *T, float *x)
{ /* set  xn1,xn2,yn1,yn2 in T */
  /* upper left = (xn1,yn1), lower right = (xn2,yn2) */
 float temp;
 if(x[0] > x[1])
  { temp = x[0];
    x[0] = x[1];
    x[1] = temp;
  }
 if(x[2] < x[3])
  { temp = x[2];
    x[2] = x[3];
    x[3] = temp;
  }
 if(T == NULL || x == NULL) return;
 T->xn1 = x[0]; T->xn2 = x[1];
 T->yn1 = x[2]; T->yn2 = x[3];
}

void Get_uc(ErsCoordInf *T, float *x)
{ /* returns xu1,xu2,yu1,yu2 in x */
 if(T == NULL || x == NULL) return;
 x[0] = T->xu1; x[1] = T->xu2;
 x[2] = T->yu1; x[3] = T->yu2;
}
void Set_uc(ErsCoordInf *T, float *x)
{ /* set  xu1,xu2,yu1,yu2 in T */
 if(T == NULL || x == NULL) return;
 T->xu1 = x[0]; T->xu2 = x[1];
 T->yu1 = x[2]; T->yu2 = x[3];
}
void Get_dc(ErsCoordInf *T, float *x)
{ /* returns xdc1,xdc2,ydc1,ydc2 in x */
 if(T == NULL || x == NULL) return;
 x[0] = T->xdc1; x[1] = T->xdc2;
 x[2] = T->ydc1; x[3] = T->ydc2;
}
void Set_dc(ErsCoordInf *T, float *x)
{ /* set  xdc1,xdc2,ydc1,ydc2 in T */
 if(T == NULL || x == NULL) return;
 T->xdc1 = x[0]; T->xdc2 = x[1];
 T->ydc1 = x[2]; T->ydc2 = x[3];
}

void wctodc(ErsCoordInf *T,float *xi,float *yi,float *xo,float *yo)
{float scx,scy;
 if(T == NULL) return;

 scx = 1.0;
 if(T->xu1 != T->xu2) scx = (T->xdc2 - T->xdc1)/(T->xu2 - T->xu1);
 scy = 1.0;
 if(T->yu1 != T->yu2) scy = (T->ydc2 - T->ydc1)/(T->yu2 - T->yu1);
 *xo = T->xdc1 + (*xi- T->xu1)*scx;
 *yo = T->ydc1 + (*yi- T->yu1)*scy;

 return;
}

void dctowc(ErsCoordInf *T,float *xi,float *yi,float *xo,float *yo)
{
 float scx,scy;
 if(T == NULL) return;

 scx = 1.0;
 if(T->xdc1 != T->xdc2) scx = (T->xu2 - T->xu1)/(T->xdc2 - T->xdc1);
 scy = 1.0;
 if(T->ydc1 != T->ydc2) scy = (T->yu2 - T->yu1)/(T->ydc2 - T->ydc1);
 *xo = T->xu1 + (*xi- T->xdc1)*scx;
 *yo = T->yu1 + (*yi- T->ydc1)*scy;

 return;
}

void wctonc(ErsCoordInf *T,float *xi,float *yi,float *xo,float *yo)
{float scx,scy;
 if(T == NULL) return;

 scx = 1.0;
 if(T->xu1 != T->xu2) scx = (T->xn2 - T->xn1)/(T->xu2 - T->xu1);
 scy = 1.0;
 if(T->yu1 != T->yu2) scy = (T->yn2 - T->yn1)/(T->yu2 - T->yu1);
 *xo = T->xn1 + (*xi- T->xu1)*scx;
 *yo = T->yn1 + (*yi- T->yu1)*scy;

 return;
}

void nctowc(ErsCoordInf *T,float *xi,float *yi,float *xo,float *yo)
{
 float scx,scy;
 if(T == NULL) return;

 scx = 1.0;
 if(T->xn1 != T->xn2) scx = (T->xu2 - T->xu1)/(T->xn2 - T->xn1);
 scy = 1.0;
 if(T->yn1 != T->yn2) scy = (T->yu2 - T->yu1)/(T->yn2 - T->yn1);
 *xo = T->xu1 + (*xi- T->xn1)*scx;
 *yo = T->yu1 + (*yi- T->yn1)*scy;

 return;
}

void dctonc(ErsCoordInf *T,float *xi,float *yi,float *xo,float *yo)
{
 float scx,scy;
 if(T == NULL) return;

 scx = 1.0;
 if(T->xdc1 != T->xdc2) scx = (T->xn2 - T->xn1)/(T->xdc2 - T->xdc1);
 scy = 1.0;
 if(T->ydc1 != T->ydc2) scy = (T->yn2 - T->yn1)/(T->ydc2 - T->ydc1);
 *xo = T->xn1 + (*xi- T->xdc1)*scx;
 *yo = T->yn1 + (*yi- T->ydc1)*scy;

 return;
}
void nctodc(ErsCoordInf *T,float *xi,float *yi,float *xo,float *yo)
{float scx,scy;
 if(T == NULL) return;

 scx = 1.0;
 if(T->xn1 != T->xn2) scx = (T->xdc2 - T->xdc1)/(T->xn2 - T->xn1);
 scy = 1.0;
 if(T->yn1 != T->yn2) scy = (T->ydc2 - T->ydc1)/(T->yn2 - T->yn1);
 *xo = T->xdc1 + (*xi- T->xn1)*scx;
 *yo = T->ydc1 + (*yi- T->yn1)*scy;

 return;
}

int Inside_Box(ErsCoordInf *T, float *x, float *y)
{/* assumes x points to right and y points down */
 int inside;
 inside = 1; /* 1= inside, 0= outside */
 if(T==NULL) { inside=0; return inside;}
 if(*x < T->xdc1) inside = 0;
 if(*x > T->xdc2) inside = 0;
 if(*y < T->ydc1) inside = 0;
 if(*y > T->ydc2) inside = 0;
 return inside;
}
