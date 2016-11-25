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
#include "cprim.h"
#include "wrdcnvrt.h"
#include "dbutil.h"
#include "pcardgowr.h"
#include "model_io.h"
#include "tfio.h"
#include "str.h"

static int PUFSIZ=96;

/***********************************************************************
c        1         2         3         4         5         6         7
|
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                      designed to be called from c
C
C   Primitive names:  WriteModPik(), ReadModPik()
C  Source directory:  ~spws/util/model/
C           Library:  picklib.a
C           Written:  92/10/12   by:  R.S.Day
C      Last revised:  94/01/20   by:  R.S.Day
C
C  Purpose:      -Write or Read a Pick Card style file.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                CALLING SEQUENCE        (I/O: IN, OUT, BOTH)
C
C       Bool WriteModPik(ErsModel *model, char *filename)
C       Bool ReadModPik (ErsModel *model, char *filename)
C
C  Type    Name    I/O     Valid     Description
C  ----    ----    ---     -----     -----------
CErsModel  *model   IN               see (Model.h)  C
C  char *  filename IN               name of disk file!
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2. 94/01/20  R. Day     Added pcard_tru_fname()
C  1. 92/10/12  R. Day     Modification of Int's routines
C-----------------------------------------------------------------------
C                       ROUTINES IN THIS PRIMITIVE
C
C  Functions:          XXXX
C  Header files:       Pcard.h ,Pick.h
C-----------------------------------------------------------------------
C               EXTERNALS REFERENCED BY THIS PRIMITIVE
C
C                                none
C-----------------------------------------------------------------------
C\END DOC
C***********************************************************************/
/**********************************************/
/*      File Formats supporting picking.      */
/*  1. CVM or GWS (Conoco Model File)       Y */
/*  2. MINT (Modified INT file format )     Y */
/*  3. GWS Generic                          N */
/***** Pcard File Format **********************/
/* fields set by the program:                 */
/* -- Card 1                                  */
/* Primary, Secondary, Tertiary header        */
/* -- Card 2 and for start of each horizon    */
/* Horizon name, Horizon color, Horizon width */
/* -- After each horizon card: The Pick cards.*/
/*  columns 1-2   'P'                         */
/*  columns 4-11  Trace number or user x-value*/
/*  columns 16-25 Time in sec (FP)            */
/*  columns 27-36 Primary key value           */
/*  columns 38-47 Secondary key value         */
/*  columns 49-58 Tertiary key value          */
/*  columns 60-60 'a' for automatic pick      */
/**********************************************/
/***********************************************************************/
/*                                                                     */
/*  WriteModPik - This function saves the picks in modified  INT format*/
/*                                                                     */
/***********************************************************************/
Bool WriteModPik(ErsModel *model, char *filename)
{
  PR_ *picking_record;
  ErsPoint *point;
  FILE *fp;
  Bool error_flag = False;
  char buf[150];
  char string[16];
  char *horizon_name;
  char NONE[5];
  char HORX[8];
  float ptime;
  long  phdr,shdr,thdr;
  register int i, j;

  if(model==NULL) return False;
  picking_record = (PR_ *) model_getpdata(model);
  if ((fp=fopen(filename, "w")) == NULL) return(False);
  strcpy(NONE,"NONE");
  strcpy(HORX,"HOR");

  for (i=0; i<80; i++) buf[i] = ' ';
  buf[49] = '\n';
  buf[50] = '\0';

  phdr = PR_GetPhdr(picking_record);
  strcpy(buf,  "*HEADER_KEYS Phdr=");
  sprintf(string,"%-2d  ", phdr);
  strcat(buf,string);

  shdr = PR_GetShdr(picking_record);
  strcat(buf, "Shdr=");
  sprintf(string,"%-2d  ", shdr);
  strcat(buf,string);

  thdr = PR_GetThdr(picking_record);
  strcat(buf, "Thdr=");
  sprintf(string,"%-2d \n", thdr);
  strcat(buf,string);
  fputs(buf, fp);

  for (i=0; i<picking_record->horizon_count; i++)
    {strcpy(buf,"*HORIZON, name=");
     sprintf(HORX,"HOR%-3d",i);
     horizon_name = HORX;
     if(picking_record->horizon_list[i]->horizon_name != NULL)
        horizon_name = picking_record->horizon_list[i]->horizon_name;
     strcat(buf,horizon_name);
     strcat(buf," color=");
     strcat(buf,picking_record->horizon_list[i]->color_name);
     strcat(buf," width=");
     sprintf(string,"%-2d\n", picking_record->horizon_list[i]->line_width);
     strcat(buf,string);
     fputs(buf, fp);

     buf[0] = 'P';
     buf[61] = '\n';
     buf[62] = '\0';
     point = picking_record->horizon_list[i]->first_point;
     while (point != NULL)
      {for (j=0; j<=point->npicks; j++)
        {/* SPID field */
         if (j == point->npicks)
           sprintf(string,"P  %8d    ",point->tn);
         else
           sprintf(string, "P  %8d    ", point->pick_list[j].traceno);
         if (!PutPCardField(buf, string, 1, 15)) error_flag = True;
 
         if (j == point->npicks)
          {ptime = point->time;
           sprintf(string, "%10.5f ", point->pkey);
           PutPCardField(buf, string, 27,37);
           sprintf(string, "%10.5f ", point->skey);
           PutPCardField(buf, string, 38,48);
           sprintf(string, "%10.5f ", point->tkey);
           PutPCardField(buf, string, 49,59);
          }
         else
          {ptime = point->pick_list[j].time;
           sprintf(string, "%10.5f ", point->pick_list[j].pkey);
           PutPCardField(buf, string, 27,37);
           sprintf(string, "%10.5f ", point->pick_list[j].skey);
           PutPCardField(buf, string, 38,48);
           sprintf(string, "%10.5f ", point->pick_list[j].tkey);
           PutPCardField(buf, string, 49,59);
          }
         sprintf(string,"%10.5f ", ptime);
         if (!PutPCardField(buf, string, 16, 26)) error_flag = True;
 
         /* auto pick flag */
         if (j != point->npicks)
           PutPCardField(buf, "a",60,60);
         else
           PutPCardField(buf, " ",60,60);
         /* write line */
         fputs(buf, fp);
        }
       point = point->next;
      }
    }

  fclose(fp);

  if (error_flag)
    return(False);
  else
    return(True);
}

/************************************************************************/
/*                                                                      */
/* ReadModPik - This function read in picks in a modified INT pick file */
/*                                                                      */
/************************************************************************/
Bool ReadModPik(ErsModel *model, char *filename)
{
  FILE *fp;
  PR_ *picking_record;
  ErsHorizon       *thorizon;
  ErsPoint         *point;
  char buf[96];
  char *horizon_name, *horizon_color, *horizon_width;
  char default_color[4];
  char ctime[16], ctssn[6],  ctemp[16];
  char cauto[2];
  int line_width,n1,n2;
  long  tssn;
  float pick_time,pkey,skey,tkey;
  ErsPointList pick_list[500];
  int npicks = 0;
  char *cp1, *cp2, *cp3;

  if(model==NULL) return False;
  /* open file if there is one */
  if (filename != NULL)
    { if ((fp = fopen(filename, "r")) == NULL) return(False); }
  else return(False);
  picking_record = (PR_ *) model_getpdata(model);
  model->datafile = (char *) calloc(1,strlen(filename)+1);
  strcpy(model->datafile,filename);

  picking_record->Phdr = UNDEFINED_KEY;
  picking_record->Shdr = UNDEFINED_KEY;
  picking_record->Thdr = UNDEFINED_KEY;
  fgets(buf, PUFSIZ, fp);
  cp1 = strstr(buf,"Phdr=");
  cp2 = strstr(buf,"Shdr=");
  cp3 = strstr(buf,"Thdr=");
  if(cp1 != NULL) picking_record->Phdr = atol(cp1+5);
  if(cp2 != NULL) picking_record->Shdr = atol(cp2+5);
  if(cp3 != NULL) picking_record->Thdr = atol(cp3+5);

  /* read file and fill horizon and point info */
  strcpy(default_color,"red");
  while (fgets(buf, PUFSIZ, fp))
   {if(strstr(buf,"*HORIZON") != NULL)
     {/* get horizon name,color, width */
        horizon_name = NULL;
        horizon_color= default_color;
        horizon_width= NULL;
        line_width   = DEFAULT_LINE_WIDTH;
        cp1 = strstr(buf,"name="); 
        cp2 = strstr(buf,"color=");
        cp3 = strstr(buf,"width=");
        if(cp1 != NULL)
          { cp1 = cp1 + 5;
            n1 = 0;
            while(cp1[n1] == ' ') n1++;
            n2 = n1;
            while(cp1[n2]> ' ') n2++;
            horizon_name = cp1 + n1;  horizon_name[n2-n1]='\0';
          }
        if(cp2 != NULL)
          { cp2 = cp2 + 6;
            n1 = 0;
            while(cp2[n1] == ' ') n1++;
            n2 = n1;
            while(cp2[n2]> ' ') n2++;
            horizon_color = cp2 + n1;  horizon_color[n2-n1]='\0';
          }
        if(cp3 != NULL)
          { cp3 = cp3 + 6;
            n1 = 0;
            while(cp3[n1] == ' ') n1++;
            n2 = n1;
            while(cp3[n2]> ' ') n2++;
            horizon_width = cp3 + n1;  horizon_width[n2-n1]='\0';
            line_width = atoi(horizon_width);
          }

       n1= 1;
       if((thorizon = ErsHorizonGetSegmentN(picking_record, horizon_name,n1)) 
           == NULL)
        { thorizon = ErsHorizonCreate(picking_record,
                      horizon_name, horizon_color, line_width, False);
        }
     }
    else if(buf[0] == 'P')
     { /* get point info */

       GetPCardField(buf, ctime, 16, 25);
       pick_time = atof(ctime);
       GetPCardField(buf, ctssn, 4, 11);
       tssn = atoi(ctssn);
       pkey = UNDEFINED_VAL;
       skey = UNDEFINED_VAL;
       tkey = UNDEFINED_VAL;
       if(picking_record->Phdr != UNDEFINED_KEY)
         {GetPCardField(buf, ctemp, 27,36);
          pkey = atof(ctemp);

          if(picking_record->Shdr != UNDEFINED_KEY)
            {GetPCardField(buf, ctemp, 38,47);
             skey = atof(ctemp);

             if(picking_record->Shdr != UNDEFINED_KEY)
               {GetPCardField(buf, ctemp, 49,58);
                tkey = atof(ctemp);
               }
            }
         }


       GetPCardField(buf, cauto, 48, 48);
       if (cauto[0] != 'a')
        {
         point = ErsPointCreate(pick_time, tssn, NULL,
                  UNDEFINED_VAL, UNDEFINED_VAL,UNDEFINED_VAL);
         if (npicks != 0)
          {ErsPointAddPickList(thorizon, point, pick_list, npicks);
           npicks = 0; }
         point->pkey = pkey;
         point->skey = skey;
         point->tkey = tkey;

         ErsPointAdd(picking_record, thorizon, point, ErsSILENT);
        }
       else
        {pick_list[npicks++].time = pick_time;
         pick_list[npicks++].traceno = tssn;
         pick_list[npicks++].pkey = pkey;
         pick_list[npicks++].skey = skey;
         pick_list[npicks++].tkey = tkey;
         if (npicks == 500) 
          { printf("ReadModPik: auto pick list is too long\n"); }
        }
     }
   }
  return(True);
}

Bool GetPCardField(char *buf, char *out, int first, int last)
{
  register int i;
  int len;

  first -= 1;
  last -= 1;
  len = strlen(buf);

  /* function returns False if field is blank */

  *out = '\0';
  if (len < first) return(False);
  if (len <= last) last = len-1;

  while ((buf[first] == ' ' || buf[first] == '\n') && first<=last) first++;
  if (first > last) return(False);

  while (buf[last] == ' ') last--;

  for (i=first; i<=last; i++) *out++ = buf[i];
  *out = '\0';

  return(True);
}

Bool PutPCardField(char *buf, char *in, int first, int last)
{
  register int i;
  int nspace;
  int length;
  first -= 1;
  last -= 1;

  nspace = last-first+1;
  length = strlen(in);

  if (length <= nspace) {
    for (i=first; i<first+length; i++) buf[i] = *in++;
    for (i; i<first+nspace; i++) buf[i] = ' ';
  } else {
    for (i=first; i<first+nspace; i++) buf[i] = *in++;
    fprintf(stderr, 
      "PutPCardField: string %s written in field %d to %d has been truncated\n",
      in, first+1, last+1);
    return(False);
  }
  return(True);
}

/***********************************************************************
c        1         2         3         4         5         6         7
|
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                      designed to be called from c
C
C   Primitive names: pcardwr(),   pcardrd()  , ->  All components
C                    pcardwrhd(), pcardrdhd(), ->  *HEADER  component
C                    pcardwrhz(), pcardrdhz(), ->  *HORIZON component
C                    pcardwrpik(),pcardrdpik(),->  *PICK    component
C                    pcardwrml(), pcardrdml(), ->  *VELOCITY component
C                    pcardwrcl(), pcardrdcl(), ->  *CELL    component
C                    pcardsettran()            ->  Build transformations
C
C                    gwswrpik(),  gwsrdpik()   ->  GWS generic Pick file
C  Source directory: ~spws/util/model/
C           Library: picklib.a
C           Written: 94/06/15   by:  R.S.Day
C      Last revised:            by:  
C
C  Purpose:      -Write or Read components of a Conoco Model File pick
C                 file.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                CALLING SEQUENCE        (I/O: IN, OUT, BOTH)
C
C    Bool pcardwr(ErsModel *model, char *outfile, long outtyp);
C    Bool pcardrd(ErsModel *model, char *infile, int *intype);
C    Bool pcardwrhd(ErsModel *model,int *opstat, int *istat);
C    Bool pcardrdhd(ErsModel *model,
C                   char *cx,char *cy,char *cz, int *istat);
C    Bool pcardwrhz(ErsModel *model, int *lun, char *keyword );
C    Bool pcardrdhz(int *lun, char *keyword,  int *nhor,
C                   int *Phdr, int *Shdr, int *Thdr,
C                   int *hids, char *names[], char *colors[]);
C    Bool pcardwrpik(ErsModel *model, int *ifile);
C    Bool pcardrdpik(ErsModel *model, int *ifile);
C    Bool pcardwrml (ErsModel *model, int *ifile)
C    Bool pcardrdml (ErsModel *model, int *ifile)
C    Bool pcardwrcl (ErsModel *model, int *ifile)
C    Bool pcardrdcl (ErsModel *model, int *ifile)
C    void pcardsettran(ErsTransforms *tdata,
C                      int numv, float *hd, int nhdrs,
C                      int Phdr, int Shdr, int Thdr);
C
C    Bool gwswrpik(ErsModel *model, char *outfile);
C    Bool gwsrdpik(ErsModel *model, char *infile);
C
C  Type    Name    I/O     Valid     Description
C  ----    ----    ---     -----     -----------
C  PCARD * pcard    IN                A Pcard structure(Pcard.h)  C
C  char *  infile   IN                name of disk file!
C  char *  outfile  IN                name of disk file!
C  int *   ifile    IN&OUT int > 0    Fortran unit number
C  long    outtyp   IN     integer    Output file type.
C                                     LAYER_TYPE, GWS_TYPE
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. pcardrd & pcardwr will call all the other IO routines.
C     input types recognized = LAYER_TYPE, GWS_TYPE
C     output types supported = LAYER_TYPE, GWS_TYPE
C  2. You may want to call pcardSetDefaults prior to pcardwr if you
C     have not set any parameters in the model member of the pcard struct
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  3. 94/06/15  R. Day     Fixed gwsrdpik problem
C  2. 93/04/06  R. Day     Supporting user_data field
C  1. 92/10/12  R. Day     Initial version
C-----------------------------------------------------------------------
C                       ROUTINES IN THIS PRIMITIVE
C
C  Functions:          XXXX
C  Header files:       Pcard.h ,Pick.h
C-----------------------------------------------------------------------
C               EXTERNALS REFERENCED BY THIS PRIMITIVE
C
C                                none
C-----------------------------------------------------------------------
C\END DOC
C***********************************************************************/

/*************************************************************
 * pcardwr()
 * Purpose:  Save picks, etc. to disk in a LAYER file
 ************************************************************/
Bool pcardwr(ErsModel *model,char *outfile, long outtyp )
{
 char  *tstr, keyword[16];


 int   wdtyp=3,nrecl=0;
 int   lun, istat;
 int   iform=0;   /*formatted*/
 int   opstat=0;  /*new      */
 int   iaccess=0; /*sequential*/

 if(outfile == NULL) return False;
 if(outfile[0] == '\0') return False;
 if(model==NULL) return False;
 wdtyp = model_getwtype(model);
 if(wdtyp< 1) wdtyp = WIEEE;

/* write header to file */
 if(outtyp == LAYER_TYPE)
   { 
     model_sethfile(model,outfile); /* see model.c */
     model_setdfile(model,outfile); /* see model.c */
     model_settfile(model,outfile); /* see model.c */
     tstr = model_gettype(model); /* get address of type string in model */
     strcpy(tstr,"LAYER");

     /* Write the *HEADER section -try new first*/
     istat = 0;
     pcardwrhd(model, &opstat, &istat);
     if(istat != 0)
      { opstat=1; /* try as an old file */
        pcardwrhd(model, &opstat, &istat);
        if(istat !=0)  return False;
      }
     /* reopen the file - if open it returns the lun*/
     rmodopen_w_(&lun, outfile,&iform,&opstat,&iaccess,&nrecl,
     &wdtyp, &istat);
     if(istat != 0) goto error;

     /* write the HORIZON sections */
     strcpy(keyword,"*HORIZON");
     if(!pcardwrhz( model, &lun, keyword))
      { printf("pcardwr: error, call to pcardwrhz\n");
        goto error;}
     strcpy(keyword,"*VHORIZON");
     if(!pcardwrhz( model, &lun, keyword))
      { printf("pcardwr: error, call to pcardwrhz\n");
        goto error;}

     /* write the *PICK section  */
     if(!pcardwrpik(model, &lun) )
      { printf("pcardwr: error in call to pcardwrpik\n");
        goto error;}

     /* write the *CELL section */
     if(!pcardwrcl(model,&lun))
      { printf("pcardwr: error in call to pcardwrcl\n");
        goto error;}

     /* write the *VELOCITY section */
     if(!pcardwrml(model,&lun))
      { printf("pcardwr: error in call to pcardwrml\n");
        goto error;}

     /* close the file       */
     rmodclos_w_(&lun);
     return True;
   }
 else if (outtyp == GWS_TYPE)
   {  return gwswrpik(model,outfile); }
 else if (outtyp == GOCAD_TYPE)
   {  return pcardgowr(model,outfile); }

 error:
 rmodclos_w_(&lun);
 return False;
}


/*************************************************************
 * pcardrd()
 * Purpose:  Read data components of a model file
 ************************************************************/
Bool pcardrd(ErsModel *model, char *infile, int *intype)
{
 Bool  rstat;



 char  cxi[16], cyi[16], czi[16], card[132];
 int   wdtyp=3,nrecl=0;
 int   lun, istat, itype, i;
 int   iform=0;   /*formatted*/
 int   istatus=1; /*old      */
 int   iaccess=0; /*sequential*/

 *intype = UNKWN_TYPE;
 if(model == NULL) return False;
 if(infile[0] == ' ' || infile[0]=='\0') return True;
 if(strcmp(infile,"NONE")==0) return True;
 if(strcmp(infile,"none")==0) return True;

 /* Determine the type of input file */
 wdtyp = model_getwtype(model);
 if(wdtyp< 1) wdtyp = WIEEE;
 rmodopen_w_(&lun, infile,&iform,&istatus,&iaccess,&nrecl,
             &wdtyp,&istat);
 if(istat != 0) return False;
 itype = UNKWN_TYPE;
 for(i=0;i<2;i++)
   { rmodrdcard_(&lun, card, &istat);
     if(strstr(card,"PICKS") != 0) { itype = GWS_TYPE; break; }
     if(strstr(card,"*HEADER")!= 0 || strstr(card,"*header")!= 0)
      { itype = LAYER_TYPE; break; }
   }
 *intype = itype;
 if(itype != LAYER_TYPE && itype != GWS_TYPE) rmodclos_w_(&lun);

 model_sethfile(model,infile);
 cxi[0]='\0'; cyi[0]='\0'; czi[0]='\0';
 if(itype == GWS_TYPE)
   { return gwsrdpik(model, infile); }
 else if(itype == LAYER_TYPE )
   { rstat = True;
     pcardrdhd(model, cxi,cyi,czi, &istat);
     if(istat != 0) goto error;
     /* reopen the file - if open it returns the lun  */
     rmodopen_w_(&lun, infile,&iform,&istatus,&iaccess,&nrecl,
             &wdtyp,&istat);
     if(istat != 0) {rstat = False; goto error; }
    
     /* read in the *PICK data( which also reads the horizon section) */
     if(!pcardrdpik(model, &lun) )
       { printf("pcardrd: error in call to pcardrdpik\n");
         rstat = False;
       }
     /* read in the *VELOCITY data if there is any */
     if(!pcardrdml(model, &lun) )
       { printf("pcardrd: error in call to pcardrdml\n");
         rstat = False;
       }
     /* read the *CELL section */
     if(!pcardrdcl(model,&lun))
      { printf("pcardrd: error in call to pcardrdcl\n");
        rstat = False;}
     /* close the file       */
     rmodclos_w_(&lun);
     return True;
   }

 error:
 rmodclos_w_(&lun);
 return rstat;
}

/*************************************************************
 * pcardwrhd()
 * Purpose:  Write transform, header , & pick data to one file.
 *           
 ************************************************************/
Bool pcardwrhd(ErsModel *model, int *opstat, int *istat)
{ PR_ *pikrec;
  ModLimits        *mlimits;
  GridLimits       *glimits;
  ErsTransforms    *tdata;
  ErsTransform     *tx,*ty,*tz;
  int   Phdr, Shdr, Thdr, khdr, i;
  int   header,word_type;
  int   itrans;
  int   ncoord;
  char  coord[256], string[24];
  float xcoord[32];
  char  cxi[16],cyi[16],czi[16];
  char  cxo[16],cyo[16],czo[16];
  char  tname[24],units[16];
  float zdatum;
  float x1,x2;
 int   iform=0;   /*formatted*/
 int   iaccess=0; /*sequential*/
 
 *istat = 1;
 if(model  == NULL) return False;
 pikrec = (PR_ *) model_getpdata(model);
 if(pikrec == NULL) return False;

/* Get the header Key values  & header data */
/* Get the sample rate */
 Phdr = PR_GetPhdr(pikrec);
 Shdr = PR_GetShdr(pikrec);
 Thdr = PR_GetThdr(pikrec);

/* Default model and grid parameters */
 mlimits = model_getmlim(model);
 glimits = model_getglim(model);
/* Copy the data from the ErsTansforms structure to 
   ncoord, xcoord[] & coord         */
 ncoord  = 0;
 coord[0]='\0';
 i=1;
 tdata = model_gettdata(model);
 while(ErsTransGetnth(tdata,i,tname,&x1,&x2,(long *) &header,units)==True)
  { ncoord++;
    xcoord[2*i-2]= x1;
    if(x2 == x1) x2 = x1+1.;
    xcoord[2*i-1]= x2;
    strcpy(string,"                ");
    strncpy(string,tname,strlen(tname));
    strcat(coord,string);
    i++;
  }

/* Determine what the Pick input coordinates are */
 strcpy(cxi,"XANNOTATION");
 strcpy(cyi,"YANNOTATION");
 strcpy(czi,"KILOMETER");
 khdr = Phdr;
 if(khdr != UNDEFINED_KEY) { ErsTransName(khdr,cxi); }
 mlimits_get_trans(mlimits,&tx,&ty,&tz);
 if(tx != NULL) strcpy(cxi,transform_getname(tx));
 if(ty != NULL) strcpy(cyi,transform_getname(ty));
 if(tz != NULL) strcpy(czi,transform_getname(tz));
 strcpy(cxo , cxi);
 strcpy(cyo , cyi);
 strcpy(czo , czi);

 if(*opstat < 0) *opstat=0;
 if(*opstat > 3) *opstat=0;
 zdatum = 0;
 itrans=1;  
 word_type = model_getwtype(model);
 if(word_type < 1) word_type = WIEEE;
 rmodwrhd_w_(model_gethfile(model),model_getdfile(model),
             model_gettfile(model),model_gettype(model),
             opstat, &itrans, cxi,cyi,czi,cxo,cyo,czo,
             &ncoord,xcoord,coord,
             &mlimits->xmin,&mlimits->xmax,
             &mlimits->ymin,&mlimits->ymax,
             &mlimits->zmin,&mlimits->zmax,&zdatum,
             &glimits->n2,&glimits->o2,&glimits->d2,
             &glimits->n3,&glimits->o3,&glimits->d3,
             &glimits->n1,&glimits->o1,&glimits->d1,
             &word_type,istat);
             
 if(*istat == 0) return True;
 else return False;
}

Bool pcardrdhd(ErsModel *model,char *cxi,char *cyi,char *czi, int *istat)
{ 
  ModLimits        *mlimits;
  GridLimits       *glimits;
  ErsTransforms    *tdata;
  ErsTransform     *tx,*ty,*tz,*t;
  float            xmin,xmax,ymin,ymax,zmin,zmax;
  float            o1,o2,o3;
  float            d1,d2,d3;
  int              n1,n2,n3;
  int              word_type,m1= -1;
  int   key;
  int   i,j,header,ntrans;
  int   mcord,ncoord;
  char  coord[256], string[24],units[16];
  char  hfile[96],tfile[96],dfile[96],type[16];


  float xcoord[32];
  char  cxo[16], cyo[16], czo[16],*infile;
  float x1,x2;
  float zdatum;
 
 *istat = 1;
 if(model == NULL)           return  False;
 infile = model_gethfile(model);
 if(infile == NULL)          return  False;
 if(infile[0] == ' ' || infile[0]=='\0') return True;
 if(strcmp(infile,"NONE")==0) return True;
 if(strcmp(infile,"none")==0) return True;
 
 mlimits = model_getmlim(model);
 glimits = model_getglim(model);
 cxo[0]  = '\0';
 cyo[0]  = '\0';
 czo[0]  = '\0';
 ncoord  = 0;
 mcord   = 32;
 dfile[0]= '\0';
 tfile[0]= '\0';
 type[0] = '\0';
 strcpy(hfile,infile);
 rmodhdpr_w_(&m1);
 rmodrdhd_w_(hfile,dfile,tfile, type,
             cxi,cyi,czi,cxo,cyo,czo,
             &mcord, &ncoord,xcoord,coord,
             &xmin,&xmax, &ymin,&ymax, &zmin,&zmax,&zdatum,
             &n2,&o2,&d2, &n3,&o3,&d3, &n1,&o1,&d1,
             &word_type,istat);
 key = dbutil_rfile2key_(hfile);
 if(key < 0)
  word_type = WVMS;
 else
  word_type = dbutil_getwordtype_(&key);
 if(word_type < 1) word_type = WVMS;
 model_setwtype(model,&word_type);
 model_sethfile(model,hfile);
 model_setdfile(model,dfile);
 model_settfile(model,tfile);
 model_settype(model,type);
/* Fill in the the Transformation structures */
 tdata = model_gettdata(model);
 for(i=0;i<ncoord;i++)
  { x1 = xcoord[2*i];
    x2 = xcoord[2*i+1];
    if(x2 == x1) x2 = x1+1.;
    strncpy(string,&coord[i*16],16);
    j = 0;
    while(string[j] != ' ' && string[j] != '\0') j++;
    string[j]='\0';
    str_to_upper(string,string);
    ErsTransHeader(&header,string);
    units[0]='\0';
    ErsTransSet(tdata,string,x1,x2,header,units);
  }
             
/* Set transformation pointers in mlimits and glimits
   to the appropriate transform */
 str_to_upper(cxi,cxi);
 str_to_upper(cyi,cyi);
 str_to_upper(czi,czi);
 ntrans = 0;
 tx = NULL; ty = NULL; tz = NULL;
 if(tdata != NULL) ntrans = TdatGet_num(tdata);
 for(i=0;i< ntrans;i++)
  {  t = TdatGet_nth(tdata,i);
     if(strcmp(transform_getname(t),cxi) == 0)
      { tx = t; }
     if(strcmp(transform_getname(t),cyi) == 0)
      { ty = t; }
     if(strcmp(transform_getname(t),czi) == 0)
      { tz = t; }
  }
 mlimits_set(mlimits,&xmin,&xmax,&zmin,&zmax,&ymin,&ymax, tx, ty, tz );
 glimits_set(glimits, &n1,&o1,&d1, &n2,&o2,&d2, &n3,&o3,&d3,
             tz, tx, ty );
 if(*istat == 0) return True;
 else return False;
}
 
Bool pcardwrhz(ErsModel *model, int *lun, char *keyword )
{PR_ *pikrec;
 ErsHorizon       *uhlist[199];
 ErsMaterials     *mats;
 ErsMaterial      *mat;
 char  keyw[32];
 char  card[132], *hname, *cname;
 int   i,hid,nhor,nseg, iostat, Phdr,Shdr,Thdr;

 if(model == NULL)  return  False;

 pikrec = (PR_ *) model_getpdata(model);
 if(pikrec == NULL) return False;
 Phdr = PR_GetPhdr(pikrec);
 Shdr = PR_GetShdr(pikrec);
 Thdr = PR_GetThdr(pikrec);

 strcpy(keyw,"*HORIZON");
 if(keyword != NULL) strcpy(keyw,keyword);
 strcpy(card," ");
 strcat(card,keyw); i = strlen(card);
 sprintf(card+i,"  P-header=%d  S-header=%d  T-header=%d",Phdr,Shdr,Thdr);
 rmodwrcard_(lun, card, &iostat);
 if(iostat != 0) return False;

 if(strcmp(keyw,"*HORIZON") == 0)
  {ErsHorizonGetHList(pikrec,&nhor, uhlist);
   for(i=0;i<nhor;i++)
     { hid  = uhlist[i]->hid;
       hname= uhlist[i]->horizon_name;
       cname= uhlist[i]->color_name;
       nseg = ErsHorizonGetSegmentCount(pikrec,uhlist[i]);
       sprintf(card,"  HID=%-3.3d NAME=%-16s NSEG=%-3.3d COLOR=%s",
         hid,hname,nseg,cname);
       rmodwrcard_(lun, card, &iostat);
       if(iostat != 0) return False;
     }
  }

 
 if(strcmp(keyw,"*VHORIZON") == 0)
  { mats = model_getmdata(model);
    if(mats == NULL) return False;
    nhor = materials_count(mats);
    for(i=0;i<nhor;i++)
      { mat = mats_get_nth(mats,i);
        hid  = material_getid(mat);
        hname= material_getname(mat);
        cname= material_getcolor(mat);
        nseg = material_segcnt(mat);
        if(strlen(hname) >0)
         sprintf(card,"  HID=%-3.3d NAME=%-16s NSEG=%-3.3d COLOR=%s",
         hid,hname,nseg,cname);
        else
         sprintf(card,"  HID=%-3.3d NAME=Horizon%-3.3d NSEG=%-3.3d COLOR=%s",
         hid,i,nseg,cname);
         
        rmodwrcard_(lun, card, &iostat);
        if(iostat != 0) return False;
      }
  }

 return True;
}

/* get horizon ids and horizon names */
/* Allocates memory for the names */
Bool pcardrdhz( int *lun, char *keyword, int *nhor,
     int *Phdr, int *Shdr, int *Thdr,
     int *hids, char *names[], char *colors[])
{char  card[160], str[24];
 char  *c1,*c2,*c3,*c4;
 int   rewind;
 int   hid, iostat;

 *nhor = 0;
 if(*lun < 0) return False;
 if(keyword == NULL) return False;

/* first position to the *HORIZON section */
 rewind = 1;
 strcpy(str,keyword);
 rmodfstr_w_(lun,&rewind,str,card,&iostat);
 if(iostat != 0) return False;

/* Get the header-keys that were used for picking */
 if((c1 = strstr(card,"P-HEADER=")) != NULL)
  { sscanf(c1+9,"%d",Phdr); }
 if((c1 = strstr(card,"S-HEADER=")) != NULL)
  { sscanf(c1+9,"%d",Shdr); }
 if((c1 = strstr(card,"T-HEADER=")) != NULL)
  { sscanf(c1+9,"%d",Thdr); }

/* Get the list of unique horizon names */
 rmodrdcard_(lun, card, &iostat);
 c1 = strstr(card,"HID=");
 c2 = strstr(card,"NAME=");
 c3 = strstr(card,"COLOR=");
 c4 = strstr(card,"NSEG=");
 while(c1 != NULL && c2 != NULL)
  { sscanf(c1+4,"%d",&hid);
    hids[*nhor] = hid;
    c2 = c2+5;
    while(c2[0]==' ') c2++;
    str[0] = '\0';
    if(c2 != c4) 
     {sscanf(c2,"%s",str);
      names[*nhor] = (char *) malloc(strlen(str) + 1);
      strcpy(names[*nhor],str);
     }
    else
     {names[*nhor] = (char *) malloc(strlen(str) + 1);
      strcpy(names[*nhor],str);
     }
    colors[*nhor] = (char *) malloc(16);
    strcpy(colors[*nhor],"red");
    if(c3 != NULL) 
     { c3 = c3+6; while(c3[0]==' ') c3++;
      strcpy(colors[*nhor],c3); }
    *nhor += 1;
    rmodrdcard_(lun, card, &iostat);
    c1 = NULL;
    c2 = NULL;
    c3 = NULL;
    c4 = NULL;
    if(iostat == 0)
     {c1 = strstr(card,"HID=");
      c2 = strstr(card,"NAME=");
      c3 = strstr(card,"COLOR=");
      c4 = strstr(card,"NSEG="); }
  }

 return True;
}

Bool pcardwrpik(ErsModel *model, int *ifile)
{
  PR_ *pikrec;
  ErsHorizon       *thorizon,*slist[99];
  ErsPoint         *point;
  int i,j,ndof,hnumber,snumber,count;
  int khdr,Phdr,Shdr,Thdr;
  int  one=1;
  char cx[33],cy[33],cz[33],str[16];
  char *name;
  float xvals[8];

  if( *ifile <= 0 ) return False;
  if(model == NULL)  return  False;
  pikrec = (PR_ *) model_getpdata(model);
  strcpy(cx,"XANNOTATION");
  strcpy(cy,"YANNOTATION");
  strcpy(cz,"KILOMETER");
  Phdr = PR_GetPhdr(pikrec);
  Shdr = PR_GetShdr(pikrec);
  Thdr = PR_GetThdr(pikrec);
  khdr = 1;
  if(Phdr != UNDEFINED_KEY) khdr = Phdr;
  if(khdr != UNDEFINED_KEY)  ErsTransName(khdr,cx);
  ErsTransName(KMETER_,cz);
  if(pikrec->transx != NULL) strcpy(cx,pikrec->transx->tname);
  if(pikrec->transz != NULL) strcpy(cz,pikrec->transz->tname);
  
  ndof = 5;
  if(pikrec->Shdr != UNDEFINED_KEY) ndof = 6;
  if(pikrec->Thdr != UNDEFINED_KEY) ndof = 7;
  strcpy(str,"*PICKS");
  rmodwchd_w_(ifile,str,cx,cy,cz,&ndof);
  if( pikrec->horizon_count < 1) return True;

  for (i=0; i<pikrec->horizon_count; i++)
    {thorizon = pikrec->horizon_list[i];
     name     = thorizon->horizon_name;
     hnumber  = thorizon->hid;
     ErsHorizonMemberList(pikrec,thorizon,&count,&snumber,slist);
     if(snumber == -1) snumber = 1;
     point = pikrec->horizon_list[i]->first_point;
     while (point != NULL)
      {for (j=0; j<=point->npicks; j++)
        {
         if (j == point->npicks)
          {
           xvals[0] = point->pkey;
           xvals[1] = point->time;
           xvals[4] = point->user_data;
           xvals[5] = point->skey;
           xvals[6] = point->tkey;
          }
         else
          {
           xvals[0] = point->pick_list[j].pkey;
           xvals[1] = point->pick_list[j].time;
           xvals[4] = point->pick_list[j].user_data;
           xvals[5] = point->pick_list[j].skey;
           xvals[6] = point->pick_list[j].tkey;
          }
         xvals[2] = hnumber;
         xvals[3] = snumber;
         rmodwval_(ifile, &ndof, &one, xvals);
        }
       point = point->next;
      }
    }

 return True;
}

Bool pcardrdpik(ErsModel *model, int *ifile)
{
  PR_ *pikrec;
  ErsHorizon       *thorizon;
  ErsPoint         *point;

  ErsTransform     *tx,*ty,*tz;
  ModLimits        *mlimits;
  long tssn;
  int  i,ndof,rewind,one,istat,ncards,nhor;
  int  hnumber, snumber, old_hnumber, old_snumber;
  int  khdr,Phdr,Shdr,Thdr;
  int  hids[199];
  char *hnames[199],*colors[199], keyword[16];
  char cx[33],cy[33],cz[33],str[33];
  char horizon_name[16],horizon_color[16];
  float xvals[10],*user_data;

  if(*ifile <= 0 ) return False;
  if(model == NULL) return False;
  pikrec = (PR_ *) model_getpdata(model);
  pikrec->Phdr = Phdr = UNDEFINED_KEY;
  pikrec->Shdr = Shdr = UNDEFINED_KEY;
  pikrec->Thdr = Thdr = UNDEFINED_KEY;
  nhor = 0;
  strcpy(keyword,"*HORIZON");
  pcardrdhz( ifile, keyword, &nhor,
             &pikrec->Phdr, &pikrec->Shdr, &pikrec->Thdr,
             hids, hnames, colors);
  if(nhor > 199) nhor = 199;
  khdr = 1;
  ndof = 5;
/*
 * Tie picking record units to the model limits units */
  mlimits = model_getmlim(model);
  mlimits_get_trans(mlimits, &tx,&ty,&tz);
  PR_setx_transf(pikrec,tx);
  PR_setz_transf(pikrec,tz);

  strcpy(cx,"XANNOTATION");
  strcpy(cy,"YANNOTATION");
  strcpy(cz,"KILOMETER");
  strcpy(str,"*PICKS");
  rewind=1; /* force a rewind of the file */
  /* should hop over the initial 2 cards in an old ICP file */
  rmodfcrd_w_(ifile,&rewind,str,cx,cz,&ndof,&istat);
  if(istat != 0) goto error;

  xvals[0]    = UNDEFINED_VAL;
  xvals[1]    = UNDEFINED_VAL;
  xvals[2]    = UNDEFINED_VAL;
  xvals[3]    = UNDEFINED_VAL;
  xvals[4]    = UNDEFINED_VAL;
  xvals[5]    = UNDEFINED_VAL;
  xvals[6]    = UNDEFINED_VAL;
  one         = 1;
  istat       = 0;
  snumber     = 1;
  hnumber     = 1;
  old_snumber = 1;
  old_hnumber = 1;
  rmodrval_w_(ifile, &ndof, &one, &ncards, xvals, &istat);
  if(istat != 0 || ncards == 0) goto error;

  if(ndof >3 ) old_snumber = xvals[3] + 0.0001;
  if(ndof >2 ) old_hnumber = xvals[2] + 0.0001;
  sprintf(horizon_name,"HOR%-3d",old_hnumber);
  strcpy(horizon_color,"red");
  for(i=0;i<nhor;i++)
    { if(hids[i] == old_hnumber)
       { if(hnames[i]!= NULL) strcpy(horizon_name, hnames[i]);
         strcpy(horizon_color,colors[i]);
         break;}
    }
  thorizon = ErsHorizonCreate(pikrec,
                horizon_name, horizon_color, 2, False);
  ErsHorizonChangeHID(pikrec,thorizon,old_hnumber);
  tssn  = UNDEFINED_TN;
  if(pikrec->Phdr == UNDEFINED_KEY) tssn  = xvals[0] + 0.001;
  point = ErsPointCreate(xvals[1], tssn,&xvals[4],
          xvals[0], xvals[5],xvals[6]);
  ErsPointAdd(pikrec, thorizon, point, ErsSILENT);
    
  while(istat == 0)
   { rmodrval_w_(ifile, &ndof, &one, &ncards, xvals, &istat);
     if(ncards == 0 || istat !=0) goto bugout;
     if(ndof >3 ) snumber = xvals[3] + 0.001;
     if(ndof >2 ) hnumber = xvals[2] + 0.001;
     if( snumber != old_snumber || hnumber != old_hnumber)
       {sprintf(horizon_name,"HOR%-3d",hnumber);
        strcpy(horizon_color,"red");
        for(i=0;i<nhor;i++)
          { if(hids[i] == hnumber)
             { if(hnames[i]!= NULL) strcpy(horizon_name, hnames[i]);
               strcpy(horizon_color,colors[i]);
               break;}
          }
        thorizon    = ErsHorizonCreate(pikrec,
                       horizon_name, horizon_color, 2, False);
        ErsHorizonChangeHID(pikrec,thorizon,hnumber);
        old_hnumber = hnumber;
        old_snumber = snumber;
       }
     tssn  = UNDEFINED_TN;
     if(pikrec->Phdr == UNDEFINED_KEY) tssn  = xvals[0] + 0.001;
     point = ErsPointCreate(xvals[1], tssn,&xvals[4],
             xvals[0], xvals[5],xvals[6]);
     ErsPointAdd(pikrec, thorizon, point, ErsSILENT);
    }

bugout:
 for(i=0;i<nhor;i++) {free(hnames[i]),free(colors[i]);}
 return True;
error:
 for(i=0;i<nhor;i++) {free(hnames[i]),free(colors[i]);}
 return False;
}

/*************************************************************
 * Read in the material properties for a depth model.      ***
 ************************************************************/
Bool pcardrdml(ErsModel *model, int *ifile)
{ 
  ErsMaterials     *mats;
  ErsMaterial      *mat;
  ErsTransforms    *tdata;
  ModLimits        *mlimits;
  ErsTransform     *tx,*ty,*tz;
  int   MAXPTS = 1000;
  int   Phdr,Shdr,Thdr;
  int   npts,ndof,ndim,nhor;
  int   i,rewind,one=1,istat,ncards;
  int  hids[199];
  char *hnames[199],*colors[199], keyword[16];
  char  name[24],color[24];
  char  cx[33],cy[33],cz[33],str[33];
  int   iform=0;   /*formatted*/
  int   istatus=1; /*old      */
  int   iaccess=0; /*sequential*/
  float xvals[10];
  int   mid, old_mid;
   
  int   *stype=NULL,*segid=NULL;
  float *xpts=NULL,*zpts=NULL,*ypts=NULL,*pvals=NULL;

 /* Check for necessary information */
  if(*ifile <= 0 ) return False;
  if(model == NULL)  return False;

  nhor = 0;
  strcpy(keyword,"*VHORIZON");
  pcardrdhz( ifile, keyword, &nhor,
             &Phdr, &Shdr, &Thdr,
             hids, hnames, colors);
  if(nhor > 199) nhor = 199;

  tdata= model_gettdata(model);
  mats = model_getmdata(model);
  if(materials_count(mats) != 0)
   { destroy_materials(mats);
     model->MatData = new_materials();
     mats = model_getmdata(model);
   }
  stype = (int *) calloc(1,MAXPTS*sizeof(int));
  segid = (int *) calloc(1,MAXPTS*sizeof(int));
  xpts = (float *) calloc(1,MAXPTS*sizeof(float));
  zpts = (float *) calloc(1,MAXPTS*sizeof(float));
  ypts = (float *) calloc(1,MAXPTS*sizeof(float));
  pvals = (float *) calloc(1,3*MAXPTS*sizeof(float));
/*
 * Tie material units to the model limits units */
  mlimits = model_getmlim(model);
  mlimits_get_trans(mlimits, &tx,&ty,&tz);
  mats_set_transx(mats,tx);
  mats_set_transz(mats,tz);
  mats_set_transy(mats,ty);
  npts= 0;
  ndof = 5;
  /* Position to the VELOCITY section-force a rewind of the file */
  strcpy(cx,"XANNOTATION");
  strcpy(cy,"YANNOTATION");
  strcpy(cz,"KILOMETER");
  strcpy(str,"*VELOCITY");
  rewind=1;
  rmodfcrd_w_(ifile,&rewind,str,cx,cz,&ndof,&istat);
  if(istat != 0) goto error;

  ndim = 2;
  if(ndof >5) ndim=3;
  materials_setdim(mats,ndim);
  xpts[npts] = xvals[0]    = UNDEFINED_VAL; /* x-value     */
  zpts[npts] = xvals[1]    = UNDEFINED_VAL; /* z-value     */
  old_mid    = xvals[2]    = UNDEFINED_VAL; /* material id */
  stype[npts] = xvals[3]    = UNDEFINED_VAL; /* surface id  */
  pvals[npts] = xvals[4]    = UNDEFINED_VAL; /* 1st parm val*/
  ypts[npts]  = xvals[5]    = UNDEFINED_VAL; /* y-value     */
  segid[npts] = xvals[6]    = UNDEFINED_VAL; /* segment-id  */
  istat     = 0;
  mid       = 1;

  rmodrval_w_(ifile, &ndof, &one, &ncards, xvals, &istat);
  if(istat != 0 || ncards == 0) goto error;
  mat = new_material();
  material_setdim(mat,ndim);
  xpts[npts] = xvals[0];
  zpts[npts] = xvals[1];
  old_mid    = (ndof >2) ? xvals[2] + 0.0001: 1;
  stype[npts]= (ndof >3) ? xvals[3] + 0.0001: 1;
  pvals[npts]= (ndof >4) ? xvals[4] : 1500.0;
  ypts[npts] = (ndof >5) ? xvals[5] : 0.0;
  segid[npts]= (ndof >6) ? xvals[6] + 0.0001: 1;
/*  for(j=0;j<ndof-4;j++) { pvals[npts*(ndof-4) + j] = xvals[j+4]; } */
  sprintf(name,"mid%-3d",old_mid);
  strcpy(color,"green");
  for(i=0;i<nhor;i++)
   {if(hids[i] == old_mid)
     {if(hnames[i]!= NULL) strcpy(name, hnames[i]);
      strcpy(color,colors[i]);
      break;}
   }
  npts++;
    
  while(istat == 0)
   { rmodrval_w_(ifile, &ndof, &one, &ncards, xvals, &istat);
     if(ncards == 0 || istat !=0) goto bugout;
     mid    = (ndof >2) ? xvals[2] + 0.0001: 1;
  /*   stype[npts]= (ndof >3) ? xvals[3]+0.0001 : 1; */
     if( mid != old_mid)
      {/* save material data */
        if(ndim==2)
         material_set(mat,one,old_mid,npts,stype,xpts,zpts,pvals);
        else
         material_set3(mat,one,old_mid,npts,stype,segid,
         xpts,ypts,zpts,pvals);
        material_setname(mat,name);
        material_setcolor(mat,color);
        materials_add(mats,mat);

        mat  = new_material();
        material_setdim(mat,ndim);
        npts = 0;
        old_mid = mid;
        sprintf(name,"mid%-3d",mid);
        strcpy(color,"green");
        for(i=0;i<nhor;i++)
         {if(hids[i] == mid)
           {if(hnames[i]!= NULL) strcpy(name, hnames[i]);
            strcpy(color,colors[i]);
            break;
           }
         }
      }
     xpts[npts] = xvals[0];
     zpts[npts] = xvals[1];
     stype[npts]= (ndof >3) ? xvals[3] + 0.0001: 1;
     pvals[npts]= (ndof >4) ? xvals[4] : 1500.0;
     ypts[npts] = (ndof >5) ? xvals[5] : 0.0;
     segid[npts]= (ndof >6) ? xvals[6]+0.001 : 1;
  /* for(j=0;j<ndof-4;j++) { pvals[npts*(ndof-4) + j] = xvals[j+4]; }*/

     if(npts < MAXPTS-1) npts++;
     else
      {MAXPTS += MAXPTS;
       stype = (int *) realloc(stype,MAXPTS*sizeof(int));
       segid = (int *) realloc(segid,MAXPTS*sizeof(int));
       xpts = (float *) realloc(xpts,MAXPTS*sizeof(float));
       zpts = (float *) realloc(zpts,MAXPTS*sizeof(float));
       ypts = (float *) realloc(ypts,MAXPTS*sizeof(float));
       pvals = (float *) realloc(pvals,3*MAXPTS*sizeof(float));
       printf("pcardrdml: buffer was expanded to avoid buffer overflow\n");
       npts++;
      }
        
    }

bugout:
 if(ndim==2)
  material_set(mat,one,old_mid,npts,stype,xpts,zpts,pvals);
 else
  material_set3(mat,one,old_mid,npts,stype,segid,
  xpts,ypts,zpts,pvals);
 material_setname(mat,name);
 material_setcolor(mat,color);
 materials_add(mats,mat);
 for(i=0;i<nhor;i++) {free(hnames[i]),free(colors[i]);}
 if(stype) free( stype);
 if(segid) free( segid);
 if(xpts) free( xpts);
 if(zpts) free( zpts);
 if(ypts) free( ypts);
 if(pvals) free( pvals);
 return True;
error:
 for(i=0;i<nhor;i++) {free(hnames[i]),free(colors[i]);}
 if(stype) free( stype);
 if(segid) free( segid);
 if(xpts) free( xpts);
 if(zpts) free( zpts);
 if(ypts) free( ypts);
 if(pvals) free( pvals);
 printf("pcardrdml: error occured\n");
 return False;
}

Bool pcardwrml(ErsModel *model, int *ifile)
{ 
  ErsMaterials     *mats;
  ErsMaterial      *mat;
  ErsTransform     *tx,*ty,*tz;
  int   npts,ndof,ndim,iptr;
  int   i,j,k,l,one=1,istat;
  char  cx[33],cy[33],cz[33],str[33],card[132],*name;

  int   mid, *stype,*segid;
  float *xpts,*ypts,*zpts,*pvals;

  /* Check for necessary information */
  if(*ifile <= 0 )   return False;
  if(model == NULL)  return False;

  mats = model_getmdata(model);
  ndim = materials_getdim(mats);
  strcpy(cx,"XANNOTATION");
  strcpy(cy,"YANNOTATION");
  strcpy(cz,"KILOMETER");
  if( (tx=mats_get_transx(mats)) != NULL)
   { name = transform_getname(tx);
     if(name != NULL) strcpy(cx,name);
   }
  if( (ty=mats_get_transy(mats)) != NULL)
   { name = transform_getname(ty);
     if(name != NULL) strcpy(cy,name);
   }
  if( (tz=mats_get_transz(mats)) != NULL)
   { name = transform_getname(tz);
     if(name != NULL) strcpy(cz,name);
   }
  strcpy(str,"*VELOCITY");
  ndof = 5;
  mat  = mats_get_nth(mats, 0);
  if(mat != NULL)
   { ndof = ndim + material_getdof(mat) + 2;
     if(ndim==3) ndof += 1;
   }
  if(ndof<5) ndof = 5;
  rmodwchd_w_(ifile,str,cx,cy,cz,&ndof);
  if(materials_count(mats) <= 0) return True;

  for(i=0;i<mats->nmat;i++)
    { mat  = mats_get_nth(mats,i);
    /*  material_get(mat,&ndof,&mid,&npts,&stype,&xpts,&zpts,&pvals);*/
      material_get3(mat,&ndof,&mid,&npts,&stype,&segid,
       &xpts,&ypts,&zpts,&pvals);
      for(j=0;j<npts;j++)
        {
          iptr = 0;
          sprintf(card+iptr,"%10.3f ", xpts[j]); iptr += 11;
          sprintf(card+iptr,"%10.3f ", zpts[j]); iptr += 11;
          sprintf(card+iptr,"%3d ", mid); iptr += 4;
          sprintf(card+iptr,"%3d ", stype[j]); iptr += 4;
          for(k=0;k<ndof;k++)
            {l = j*ndof + k;
             sprintf(card+iptr,"%10.3f ",pvals[l]); iptr += 11;}
          if(ndim>2)
            { sprintf(card+iptr,"%10.3f ", ypts[j]); iptr += 11;
              sprintf(card+iptr,"%3d", segid[j]); iptr += 3;
            }
          card[iptr]='\0';
          rmodwrcard_(ifile, card, &istat);
        }
    }

 return True;
}

/*************************************************************
 * Write  the cell parameters for a depth model.           ***
 ************************************************************/
Bool pcardwrcl(ErsModel *model, int *ifile)
{
  ErsCells         *Cdata;
  ErsCell          *cell;
  ErsTransform     *tx,*tz,*ty;
  int   ndof, ncell, ndim;
  int   cell_id;
  int   i,istat,iptr;
  char  cx[33],cy[33],cz[33],str[33],card[132],*name;
  float xi,zi,yi;


 /* Check for necessary information */
  if(*ifile <= 0 ) return False;
  if(model == NULL)  return False;
  Cdata = model_getcdata(model);

  ndof  = 3;
  strcpy(cx,"XANNOTATION");
  strcpy(cy,"YANNOTATION");
  strcpy(cz,"KILOMETER");
  if( (tx=cells_get_transx(Cdata)) != NULL)
   { name = transform_getname(tx);
     if(name != NULL) strcpy(cx,name);
   }
  if( (tz=cells_get_transz(Cdata)) != NULL)
   { name = transform_getname(tz);
     if(name != NULL) strcpy(cz,name);
   }
  if( (ty=cells_get_transy(Cdata)) != NULL)
   { name = transform_getname(ty);
     if(name != NULL) strcpy(cz,name);
   }

  cell = cells_get_nth(Cdata,0);
  ndim = cells_getdim(Cdata);
  strcpy(str,"*CELL");
  ndof = 5;
  if(ndim == 2) ndof = 3;
  rmodwchd_w_(ifile,str,cx,cy,cz,&ndof);
  ncell =cells_count(Cdata);
  if(ncell <= 0) return True;

  for(i=0;i<ncell;i++)
   { cell = cells_get_nth(Cdata,i);
     if(cell == NULL) break;
     iptr = 0;
     if(ndim==2)
      cell_get(cell,&xi,&zi,&cell_id);
     else
      cell_get3(cell,&xi,&zi,&yi,&cell_id);
     sprintf(card+iptr,"%12.4f ", xi); iptr += 13;
     sprintf(card+iptr,"%12.4f ", zi); iptr += 13;
     sprintf(card+iptr,"%3d ", cell_id); iptr += 4;
     if(ndim==3) sprintf(card+iptr,"%12.4f 0", yi); iptr += 14;
     card[iptr]='\0';
     rmodwrcard_(ifile, card, &istat);
   }

 return True;
}

/*************************************************************
 * Read in the cell parameters for a depth model.          ***
 ************************************************************/
Bool pcardrdcl(ErsModel *model, int *ifile)
{ 
  ErsCells         *Cdata;
  ErsCell          *cell;
  ErsTransforms    *tdata;
  ErsTransform     *tx,*ty,*tz;
  ModLimits        *mlimits;
  int   ncell,nbndpt,ndof;
  int   rewind,one,istat,ncards;
  int   ndim;
  int   mid;
  char  cx[33],cz[33],str[33];
  float xvals[10];

 /* Check for necessary information */
  if(*ifile <= 0 ) return False;
  if(model == NULL)  return False;

  Cdata = model_getcdata(model);
  tdata = model_gettdata(model);
  ncell = cells_count(Cdata);
  if(ncell >0)
   { destroy_cells(Cdata);
     model->CellData = new_cells();
     Cdata = model_getcdata(model);
   }

  mlimits = model_getmlim(model);
  mlimits_get_trans(mlimits, &tx,&ty,&tz);
  cells_set_transx(Cdata,tx);
  cells_set_transz(Cdata,tz);
  nbndpt= 0;
  ndof  = 3;
  ndim  = 2;
  /* Position to the CELL section-force a rewind of the file */
  strcpy(cx,"XANNOTATION");
  strcpy(cz,"KILOMETER");
  strcpy(str," *CELL");
  rewind=1;
  rmodfcrd_w_(ifile,&rewind,str,cx,cz,&ndof,&istat);
  if(istat != 0 || ndof < 3) goto error;
     
  if(ndof>=4) ndim = 3;
  cells_setdim(Cdata,ndim);
  xvals[0]    = UNDEFINED_VAL; /* x-value     */
  xvals[1]    = UNDEFINED_VAL; /* z-value     */
  xvals[2]    = UNDEFINED_VAL; /* material id */
  xvals[3]    = UNDEFINED_VAL; /* material id */
  one         = 1;
  istat       = 0;
  mid       = 1;

  rmodrval_w_(ifile, &ndof, &one, &ncards, xvals, &istat);
  while(istat == 0)
    {if(ncards == 0 ) goto bugout;
     mid = xvals[2] + 0.001;
     cell = new_cell();
     if(ndim==3)
      {cell_set3(cell,&xvals[0],&xvals[1],&xvals[3],&mid);
       cell_setdim(cell,3);
      }
     else
      {cell_set(cell,&xvals[0],&xvals[1],&mid);
       cell_setdim(cell,2);
      }
     cells_add(Cdata,cell);
     ncell = cells_count(Cdata);
     rmodrval_w_(ifile, &ndof, &one, &ncards, xvals, &istat);
    }

bugout:
 return True;
error:
 printf("pcardrdcl: error occured\n");
 return False;
}

/*******************************************
 * Set up some default transformations.  *** 
 ******************************************/
void pcardsettran(ErsTransforms *tdata,
                int numv, float *hd, int nhdrs,
                int Phdr, int Shdr, int Thdr)
{int   i,ip1,khdr,ncoord;
 long  hdrval[16];
/*
= {XGRIDHDR_,YGRIDHDR_,XBASEHDR_,YBASEHDR_,XANNOHDR_,
YANNOHDR_,TIMEHDR_,METER_,KMETER_,FEET_,KFEET_,DPTHHDR_};
*/
 float x1,x2;
 char  tname[16][17];
 char  units[16];

 if(tdata == NULL ) return;
/* First set some default transforms */
 ncoord = 0;
 tdata->ntrans = 0;
/*
 ErsTransName(XGRIDHDR_,name);
 ErsTransSet(tdata,name, 0.0,1.0,XGRIDHDR_,"METER"); ncoord++;
 ErsTransName(YGRIDHDR_,name);
 ErsTransSet(tdata,name, 0.0,1.0,YGRIDHDR_,"METER"); ncoord++;
 ErsTransName(XBASEHDR_,name);
 ErsTransSet(tdata,name, 0.0,1.0,XBASEHDR_,"METER"); ncoord++;
 ErsTransName(YBASEHDR_,name);
 ErsTransSet(tdata,name, 0.0,1.0,YBASEHDR_,"METER"); ncoord++;
 ErsTransName(XANNOHDR_,name);
 ErsTransSet(tdata,name, 0.0,1.0,XANNOHDR_,"METER"); ncoord++;
 ErsTransName(YANNOHDR_,name);
 ErsTransSet(tdata,name, 0.0,1.0,YANNOHDR_,"METER"); ncoord++;
 ErsTransName(TIMEHDR_,name);
 ErsTransSet(tdata,name, 0.0,1.0,TIMEHDR_,"SECOND"); ncoord++;
 ErsTransSet(tdata,"SECOND",0.0,1.0,TIMEHDR_,"SECOND"); ncoord++;
 ErsTransName(METER_,name);
 ErsTransSet(tdata,name, 0.0,1000.0,METER_,"METER"); ncoord++;
 ErsTransName(KMETER_,name);
 ErsTransSet(tdata,name, 0.0,1.0,KMETER_,"KILOMETER"); ncoord++;
 ErsTransName(FEET_,name);
 ErsTransSet(tdata,name, 0.0,3280.84,FEET_,"FEET"); ncoord++;
 ErsTransName(KFEET_,name);
 ErsTransSet(tdata,name, 0.0,3.28084,KFEET_,"KILOFEET"); ncoord++;
 ErsTransName(DPTHHDR_,name);
 ErsTransSet(tdata,name, 0.0,1000.0,DPTHHDR_,"METER"); ncoord++;

*/
 if(hd == NULL || nhdrs <1) return;
/***********************************************
 * header words exist so we determine which  ***
 * transformations can be redefined by header***
 **********************************************/
/* Headers 1,7,17,37, & nn may get used           */
 hdrval[ncoord] = SEQHDR_;
 ErsTransName(hdrval[ncoord],tname[ncoord]); ncoord++;
 strcpy(tname[ncoord],"XTRACE");
 if(nhdrs >= XGRIDHDR_ )
  {hdrval[ncoord] = XGRIDHDR_;
   ErsTransName(hdrval[ncoord],tname[ncoord]); (ncoord)++; }
 if(nhdrs >= YGRIDHDR_ )
  {hdrval[ncoord] = YGRIDHDR_;
   ErsTransName(hdrval[ncoord],tname[ncoord]); (ncoord)++; }
 if(nhdrs >= XBASEHDR_)
  {hdrval[ncoord] = XBASEHDR_;
   ErsTransName(hdrval[ncoord],tname[ncoord]); (ncoord)++; }
 if(nhdrs >= YBASEHDR_)
  {hdrval[ncoord] = YBASEHDR_;
   ErsTransName(hdrval[ncoord],tname[ncoord]); (ncoord)++; }
 if(nhdrs >= XANNOHDR_)
  {hdrval[ncoord] = XANNOHDR_;
   ErsTransName(hdrval[ncoord],tname[ncoord]); (ncoord)++; }
 if(nhdrs >= YANNOHDR_)
  {hdrval[ncoord] = YANNOHDR_;
   ErsTransName(hdrval[ncoord],tname[ncoord]); (ncoord)++; }
 khdr = Phdr;
 if(khdr != UNDEFINED_KEY && khdr <= nhdrs)
  {if(khdr != YGRIDHDR_ && khdr != YBASEHDR_ && khdr !=YANNOHDR_ )
    {if(khdr != SEQHDR_ && khdr != XGRIDHDR_ && 
       khdr != XBASEHDR_ && khdr !=XANNOHDR_ )
      {hdrval[ncoord] = khdr;
       ErsTransName(hdrval[ncoord],tname[ncoord]);
       ++ncoord;
      }
    }
  }

/* Fill in the the Transformation structures when there is no match */
 for(i=0;i<ncoord;i++)
  { ip1 = 0;
    x1 = hd[ip1+hdrval[i]-1];
    ip1 = nhdrs*(numv-1);
    x2 = hd[ip1+hdrval[i]-1];
    units[0] = '\0';
    if( ErsTransGet(tdata,tname[i],&x1,&x2,&hdrval[i],units) == False )
      {ErsTransSet(tdata,tname[i],x1,x2,hdrval[i],units);}
  }
 units[0] = '\0';

 return;
}


Bool gwswrpik(ErsModel *model, char *outfile)
{
 PR_ *pikrec;
 ErsHorizon       *thorizon;
 ErsHorizon       *hlist[199];
 ErsHorizon       *slist[199];
 ErsPoint         *point;
 int  Phdr;
 int  wdtyp=3,nrecl=0;
 int i,j,k,hnumber,istat;

 int  one=1;
 char card[132];

 char *hname;
 float xvals[2];
 int   nhor,nseg,N,lun,iptr;
 int   iform=0;   /*formatted*/
 int   istatus=0; /*new      */
 int   iaccess=0; /*sequential*/

 if(model == NULL) return False;
 pikrec = (PR_ *) model_getpdata(model);
 if(pikrec== NULL) return False;
 if( pikrec->horizon_count < 1) return True;
 Phdr = (int) PR_GetPhdr(pikrec);

 /* open the file      */
 wdtyp = model_getwtype(model);
 if(wdtyp< 1) wdtyp = WIEEE;
 rmodopen_w_(&lun, outfile,&iform,&istatus,&iaccess,
 &nrecl,&wdtyp,&istat);
 if(istat != 0) return False;

 /* Get list of unique horizon names  (hlist) */
 /* Get segment count for each horizon(slist) */
 ErsHorizonGetHList(pikrec, &nhor , hlist);
 strcpy(card," PICKS");
 for(i=0;i<nhor;i++)
   {j=0;
    while(j<131) { card[j] = ' '; j++; }
    card[j]='\0';
    strncpy(card," PICKS",6);
    strncpy(card+8,"UNTITLED LINE",13);
    hname = hlist[i]->horizon_name;
    strncpy(card+36,hname,strlen(hname));
    if(strcmp(hlist[i]->horizon_name,"FAULTS")==0)
     strncpy(card+66,"FAULTS",6);
    else
     strncpy(card+66,"HORI",4);
    strncpy(card+70,"TIME",4);
    card[81]='\0';
    rmodwrcard_(&lun, card, &istat);

    ErsHorizonMemberList(pikrec, hlist[i], &nseg, &N, slist);
    for(k=0;k<nseg;k++)
      {j=0;
       while(j<131) { card[j] = ' '; j++; }
       card[j]='\0';
       strncpy(card," PICKN  ",8);
       thorizon = slist[k];
       hnumber  = thorizon->hid;
       point    = slist[k]->first_point;
       iptr = 8;
       while (point != NULL)
        {for (j=0; j<=point->npicks; j++)
          {
           if (j == point->npicks)
            {xvals[0] = point->tn;
             xvals[1] = point->time;
             if(Phdr != UNDEFINED_KEY) xvals[0] = point->pkey;
            }
           else
            {xvals[0] = point->pick_list[j].traceno;
             xvals[1] = point->pick_list[j].time;
             if(Phdr != UNDEFINED_KEY) xvals[0] = point->pick_list[j].pkey;
            }
           sprintf(card+iptr,"%7.2f", xvals[0]); iptr += 7;
           sprintf(card+iptr,"%7.2f", xvals[1]); iptr += 7;
           if(iptr>=120)
            { card[iptr]='\0';
              rmodwrcard_(&lun, card, &istat);
              iptr = 8;
              strncpy(card," PICKZ  ",8);
            }
          }
         point = point->next;
        }  
       if(iptr>8)
         { card[iptr]='\0';
           rmodwrcard_(&lun, card, &istat);
           iptr = 8;
           strncpy(card," PICKZ  ",8);
         }
      }
   }

 /* close the file       */
 rmodclos_w_(&lun);
 return True;
}

Bool gwsrdpik(ErsModel *model, char *infile)
{UserInfo         *us;
 PR_ *pikrec;
 ErsHorizon       *thorizon;


 ErsPoint         *point;
 int  wdtyp=3,nrecl=0;
 int istat;
 int  hnumber, old_hnumber;

 int  one=1;
 char str1[16],card[144],keyword[32];
 char lname[32],hname[32],htype[16],ptype[16];
 char *targline, *targtype;
 char *cl;
 char *cp1, *cp2;
/* Variables for cracking pick cards */
 int   l, nflds;
 float xvals[2];
 long  tssn,rewind=1;
 int   nhor,lun;
 int   iform=0;   /*formatted*/
 int   istatus=1; /*old      */
 int   iaccess=0; /*sequential*/

 char which[8];
 int nl,ncrds;
 static Bool found = False;

 if(model == NULL) return False;
 pikrec = (PR_ *) model_getpdata(model);
 if(pikrec== NULL) return False;

 /* open the file      */
 wdtyp = model_getwtype(model);
 if(wdtyp< 1) wdtyp = WIEEE;
 rmodopen_w_(&lun, infile,&iform,&istatus,&iaccess,
 &nrecl,&wdtyp,&istat);
 if(istat != 0) return False;
 hnumber     = 1;
 old_hnumber = 1;
 nl    = 0;
 nhor  = 0;
 ncrds = 0;
 found = False;
 strcpy(which,"FAUL");

/* rmodrdcard_(&lun, card, &istat); */
 strcpy(keyword,"PICKS");
 rmodfstr_w_(&lun,&rewind,keyword,card,&istat);
 if(istat != 0)
   { rmodclos_w_(&lun);
     return False;}
 cp1 = strstr(card,"PICKS");
L100:
 
 while(cp1 == NULL)
   { rmodrdcard_(&lun, card, &istat);
     if(istat != 0)
       { rmodclos_w_(&lun);
         if(ncrds < 1) return False;
         return True;
       }
     cp1 = strstr(card,"PICKS");
   }

 strncpy(lname,card+6,30);  lname[30]='\0';
 strncpy(hname,card+36,30); hname[30]='\0';
 cp2 = &hname[29];
 while(cp2[0] == ' ') {cp2[0]='\0'; cp2 -= 1; }
 strncpy(htype,card+66,4);  htype[4]='\0';
 strncpy(ptype,card+70,4);  ptype[4]='\0';
 targline = lname;
 targtype = ptype;
 if(strcmp(lname,targline)!=0 || strcmp(ptype,targtype)!=0)
   { found = False;
     goto L100;
   }
 else
   { found = True;
     us= model_getudata(model);
      if(us != NULL)
       { cl= us->linename;
         if(cl!= NULL) free(cl);
         cl= (char *) malloc(strlen(lname)+1);
         strcpy(cl,lname);
         us->linename =cl;
       }
     old_hnumber = hnumber;
/*
     thorizon    = ErsHorizonCreate(pikrec,
                       hname, "red", 2, False);
     hnumber     = thorizon->hid;
*/
     ncrds++;
   }
   

L125:
 rmodrdcard_(&lun, card, &istat);
 if(istat != 0)
   { rmodclos_w_(&lun);
     if(ncrds < 1) return False;
     return True;
   }

 l = strlen(card+7);
 nflds = l/7;
 if( (cp1 = strstr(card,"PICKN")) != NULL)
   { printf("pickn card\n");
     thorizon    = ErsHorizonCreate(pikrec,
                       hname, "red", 2, False);
     hnumber     = thorizon->hid;
     ncrds++;
     for(l=0;l<nflds;l += 2)
       { 
         strncpy(str1,cp1+7+l*7,7); str1[7]='\0';
         sscanf(str1,"%7f",&xvals[0]);
         strncpy(str1,cp1+14+l*7,7); str1[7]='\0';
         sscanf(str1,"%7f",&xvals[1]);
         tssn  = UNDEFINED_TN;
         point = ErsPointCreate(xvals[1], tssn, NULL,
                 xvals[0], UNDEFINED_VAL, UNDEFINED_VAL );
         ErsPointAdd(pikrec, thorizon, point, ErsSILENT);
       }
     goto L125;
   }
 if( (cp1 = strstr(card,"PICKZ")) != NULL)
   { printf("pickz card\n");
     ncrds++;
     for(l=0;l<nflds;l += 2)
       {
         strncpy(str1,cp1+7+l*7,7); str1[7]='\0';
         sscanf(str1,"%7f",&xvals[0]);
         strncpy(str1,cp1+14+l*7,7); str1[7]='\0';
         sscanf(str1,"%7f",&xvals[1]);
         tssn  = UNDEFINED_TN;
         point = ErsPointCreate(xvals[1], tssn, NULL,
                 xvals[0], UNDEFINED_VAL, UNDEFINED_VAL );
         ErsPointAdd(pikrec, thorizon, point, ErsSILENT);
       }
     goto L125;
   }
 if( (cp1 = strstr(card,"PICKS")) != NULL)
   { printf("PICKS card\n");
     goto L100;
   }
 else
   { cp1 = NULL;
     goto L100;
   }

}

