/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* PSMANAGER: $Revision: 1.6 $ ; $Date: 2011/11/17 00:10:53 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" PSMANAGER - printer MANAGER for HP 4MV and HP 5Si Mx Laserjet ",
"                PostScript printing				",
" 								",
"   psmanager < stdin  [optional parameters] > stdout 		",
" 								",
" Required Parameters:						",
"  none 							",
" Optional Parameters:						",
" papersize=0	paper size  (US Letter default)			",
" 		=1       US Legal				",
" 		=2	 A4					",
" 		=3     	 11x17					",
" 								",
" orient=0	paper orientation (Portrait default)		",
"  		=1   	Landscape				",
" 								",
" tray=3        printing tray (Bottom tray default)		",
"  		=1	tray 1 (multipurpose slot)		",
"  		=2	tray 2 					",
" 								",
" manual=0	no manual feed 					",
"  		=1     (Manual Feed)				",
" 								",
" media=0	regular paper					",
"  		=1     Transparency				",
"  		=2     Letterhead				",
"  		=3     Card Stock				",
"  		=4     Bond					",
"  		=5     Labels					",
"  		=6     Prepunched				",
"  		=7     Recyled					",
"  		=8     Preprinted				",
"  		=9     Color (printing on colored paper)	",
" 								",
" Notes: 							",
" The option manual=1 implies tray=1. The media options apply	",
" only to the HP LaserJet 5Si MX model printer.			",
"  								",
" Examples: 							",
"   overheads:							",
"    psmanager <  postscript_file manual=1 media=1 | lpr	",
"   labels:							",
"    psmanager <  postscript_file manual=1 media=5 | lpr	",
" 								",
NULL};

/*
 * Notes:  This code was reverse engineered using output from
 *         the NeXTStep  printer manager.
 * 
 * Author:  John Stockwell, June 1995, October 1997
 * 
 * Reference:   
 *		PostScript Printer Description File Format Specification,
 *		version 4.2, Adobe Systems Incorporated
 */
/**************** end self doc ********************************/

/* Statically defined strings */
static char *letter = {"<</DeferredMediaSelection true /PageRegion [612 792] /ImagingBBox null>> setpagedevice"};
static char *legal = {"<</DeferredMediaSelection true /PageRegion [612 1008] /ImagingBBox null>> setpagedevice"};
static char *tabloid = {"<</DeferredMediaSelection true /PageRegion [792 1224] /ImagingBBox null>> setpagedevice"};
static char *a4 = {"<</DeferredMediaSelection true /PageRegion [595 842] /ImagingBBox null>> setpagedevice"};
static char *cass = {"<</DeferredMediaSelection true /MediaPosition 0>> setpagedevice"};
static char *lcass = {"<</DeferredMediaSelection true /MediaPosition 1>> setpagedevice"};
static char *mp = {"<</DeferredMediaSelection true /MediaPosition 3>> setpagedevice"};
static char *none = {"<</DeferredMediaSelection true /MediaType null>> setpagedevice"}; 
static char *preprinted = {"<</DeferredMediaSelection true /MediaType (Preprinted)>> setpagedevice"}; 
static char *letterhead = {"<</DeferredMediaSelection true /MediaType (Letterhead)>> setpagedevice"}; 
static char *transparency = {"<</DeferredMediaSelection true /MediaType (Transparency)>> setpagedevice"}; 
static char *prepunched = {"<</DeferredMediaSelection true /MediaType (Prepunched)>> setpagedevice"}; 
static char *labels = {"<</DeferredMediaSelection true /MediaType (Labels)>> setpagedevice"}; 
static char *bond = {"<</DeferredMediaSelection true /MediaType (Bond)>> setpagedevice"}; 
static char *recycled = {"<</DeferredMediaSelection true /MediaType (Recycled)>> setpagedevice"}; 
static char *color = {"<</DeferredMediaSelection true /MediaType (Color)>> setpagedevice"}; 
static char *cardstock = {"<</DeferredMediaSelection true /MediaType (Card Stock)>> setpagedevice"}; 

/* Statically defined integer arrays */
static int letterdim[] = {612,792};
static int legaldim[] = {612,1008};
static int tabloiddim[] = {792,1224};
static int a4dim[] = {595,842};

/* Prototypes of subroutines used internally */
void pstitle(int pages);
void documentPaperSizes(char *size);
void orientation(char *direction);
void endProlog(int pages);
void space(void);
void grestore(void);
void gsave(void);
void showpage(void);
void setUserDict(int count);
void clearUserDict(void);
void setUpPage(char *pagestr, char *papersizestr, char *mediastr,
		char *mediatypestr, int orient, int manual,
		char *slotstr, char *traystr, int *pagedim, int llx,
		int lly, int urx,int ury, float pllx,float plly,
		float purx, float pury, int pages);
void pageTrailer(int *pagedim, int orient, float pllx,
			float plly, float purx, float pury);


int main (int argc, char **argv)
{
	int papersize;		/* print papersize		*/
	int media;		/* paper media			*/
	int orient;		/* portrait or landscape	*/
	int tray;		/* paper tray			*/
	int manual;		/* manual feed			*/

	cwp_String papersizestr=NULL;	/* papersize name	*/
	cwp_String mediatypestr=NULL;	/* media type name	*/
	cwp_String mediastr=NULL;	/* media name		*/
	cwp_String orientstr=NULL;	/* orientation name	*/
	cwp_String traystr=NULL;	/* tray name		*/
	cwp_String slotstr=NULL;	/* input slot name	*/
	char line[BUFSIZ];	/* one line in input		*/

	int llx=0;		/* BoundingBox lower left  x 	*/
	int lly=0;		/*    ...      ...   ...   y	*/
	int urx=0;		/*    ...      upper right x	*/
	int ury=0;		/*    ...      ...   ...   y	*/
	int pages=1;		/*   pages in output		*/
	int count=0;		/* page counter			*/
	int oldcount=0;		/* temporary page counter	*/

	float fllx=0.;		/* BoundingBox lower left  x 	*/
	float flly=0.;		/*    ...      ...   ...   y	*/
	float furx=0.;		/*    ...      upper right x	*/
	float fury=0.;		/*    ...      ...   ...   y	*/

	float pllx;		/* PageBoundingBox parameters	*/
	float plly;		
	float purx;
	float pury;

	int pagedim[2];		/* dimensions of page		*/

	char *pagestr=NULL;	/* page style instructions	*/

	FILE *infp=stdin;	/* input file pointer		*/
	FILE *outfp=stdout;	/* output file pointer		*/
	FILE *tmpfp;		/* tempfile pointer 		*/

	/* Initialize getpar */
	initargs(argc,argv);
	requestdoc(1);

	/* Get parameters */
	if (!getparint("papersize",&papersize))		papersize = 0 ;
	if (!getparint("orient",&orient))	orient = 0;
	if (!getparint("tray",&tray))		tray = 3;
	if (!getparint("manual",&manual))	manual = 0;
	if (!getparint("media",&media))		media = 0;
        checkpars();


	/* Manual feed */
	if (manual) {
		tray=1;
		traystr=mp;
	}

	/* Set up the tray */
	if (tray==2) {
		slotstr="Cassette";
		traystr=cass;
	} else if (tray==1) {
		slotstr="MP";
		traystr=mp;
	} else if (tray==3) {
		slotstr="LargeCapacity";
		traystr=lcass;
	} else {
		err("Unsupported tray, %d", tray);
	}


	/* Set up identifying strings for orientation */
	if (orient==0) {
		orientstr="Portrait";
	} else if (orient==1) {
		orientstr="Landscape";
	} else  {
		err("Unsupported orientation, %d", orient);
	}

	/* Set up identifying strings for papersize types */
	if (papersize==0) {
		papersizestr="Letter";
		pagestr=letter;
		if (orient==0){
			pagedim[0] = letterdim[0];
			pagedim[1] = letterdim[1];
		} else {
			pagedim[0] = letterdim[1];
			pagedim[1] = letterdim[0];
		}
	} else if (papersize==1) {
		papersizestr="Legal";
		pagestr=legal;
		if (orient==0){
			pagedim[0] = legaldim[0];
			pagedim[1] = legaldim[1];
		} else {
			pagedim[0] = legaldim[1];
			pagedim[1] = legaldim[0];
		}
	} else if (papersize==2) {
		papersizestr="A4";
		pagestr=a4;
		if (orient==0){
			pagedim[0] = a4dim[0];
			pagedim[1] = a4dim[1];
		} else {
			pagedim[0] = a4dim[1];
			pagedim[1] = a4dim[0];
		}
	} else if (papersize==3) {
		papersizestr="Tabloid";
		pagestr=tabloid;;
		if (orient==0){
			pagedim[0] = tabloiddim[0];
			pagedim[1] = tabloiddim[1];
		} else {
			pagedim[0] = tabloiddim[1];
			pagedim[1] = tabloiddim[0];
		}
	} else  {
		err("Unsupported papersize selection, %d", papersize);
	}
/* Statically defined integer arrays */

	/* Get paper media */
	if (media==0) {
		mediatypestr="None";
		mediastr=none;
	} else if (media==1) {
		mediatypestr="Transparency";
		mediastr=transparency;
	} else if (media==2) {
		mediatypestr="Letterhead";
		mediastr=letterhead;
	} else if (media==3) {
		mediatypestr="Card Stock";
		mediastr=cardstock;
	} else if (media==4) {
		mediatypestr="Bond";
		mediastr=bond;
	} else if (media==5) {
		mediatypestr="Labels";
		mediastr=labels;
	} else if (media==6) {
		mediatypestr="Prepunched";
		mediastr=prepunched;
	} else if (media==7) {
		mediatypestr="Recycled";
		mediastr=recycled;
	} else if (media==8) {
		mediatypestr="Preprinted";
		mediastr=preprinted;
	} else if (media==9) {
		mediatypestr="Color";
		mediastr=color;
	} else {
		err("Unsupported media selection, %d", media);
	}

	/* Open tempfile */
	tmpfp = etmpfile();
	rewind(tmpfp);

	/* Read input line by line and capture BoundingBox info  */
	while (fgets(line,BUFSIZ,infp)!=NULL){ 

		/* If the line begins with a % */
		if (line[0] == '%'){

			/* if BoundingBox */
			if (strstr(line,"%%BoundingBox:")!=NULL) {
	
				/* if  BoundingBox: (atend) skip to next line */
				if (strstr(line,"atend")!=NULL)
					continue;
	
				/* capture BoundingBox values */
				if (sscanf(line,"%*s %d %d %d %d",
					&llx,&lly,&urx,&ury)==4) {
	
					/* integers read */

				} else if (sscanf(line,"%*s %f %f %f %f",
						&fllx,&flly,&furx,&fury)==4) {
					/* read floats and convert */
					llx = NINT(fllx);
					lly = NINT(flly);
					urx = NINT(furx);
					ury = NINT(fury);
				} else {
					err("Error reading BoundingBox!");
				}

			}
			/* if Pages: */
			if (strstr(line,"%%Pages:")!=NULL) {

				/* capture number of pages values */
				if (sscanf(line,"%*s %d", &pages)==1) {};
			}
		}
	
		/* Write line to tmpfile */
		fputs(line,tmpfp);
	}
	/* Rewind tempfile */
	rewind(tmpfp);

	/* Compute PageBoundingBox dimensions */
	pllx = (pagedim[0] - urx + llx)/2.0;
	plly = (pagedim[1] - ury + lly)/2.0;
	purx = urx - llx + pllx;
	pury = ury - lly + plly;

	/* begin creating PostScript wrapper */
	pstitle(pages);

	/* DocumentPapersizes */
	documentPaperSizes(papersizestr);

	/* Orientation */
	orientation(orientstr);

	/* End Prolog */
	endProlog(pages);

	/* Setup page */
	setUpPage(pagestr,papersizestr,mediastr,mediatypestr,
		orient,manual,slotstr,traystr,pagedim,
		llx,lly,urx,ury,pllx, plly,purx,pury,pages);

	/* Write contents of tmpfile to outfp */
	while (fgets(line,BUFSIZ,tmpfp)!=NULL){ 

		oldcount = count;
		/* setup pages in multipage document */
		if (pages!=1){
			if (strstr(line,"%%Page:")!=NULL) {
				++count;
				clearUserDict();
				fputs(line,outfp);
				setUserDict(count);
			}
		}
		if (count==oldcount) fputs(line,outfp);
	}

	/* Page Trailer */
	if (pages==1) pageTrailer(pagedim,orient,pllx,plly,purx,pury);

	return EXIT_SUCCESS;
}

void pstitle(int pages) 
/**********************************************************************
pstitle - begin title portion of PostScript wrapper
**********************************************************************
Input:
int pages	number of pages in document
**********************************************************************
Author: John Stockwell, CWP, June 1996
**********************************************************************/
{
	fprintf(stdout, "%%!PS-Adobe-2.0\n");
	fprintf(stdout, "%%%%Title:\n");
	fprintf(stdout, "%%%%Creator: psmanager\n");
	fprintf(stdout, "%%%%CreationDate\n");
	fprintf(stdout, "%%%%For: psmanager user\n");
	fprintf(stdout, "%%%%DocumentFonts: (atend)\n");

	if (pages==1)
		fprintf(stdout, "%%%%Pages: (atend) 1\n");
	else
		fprintf(stdout, "%%%%Pages: %d 1\n",pages);

	fprintf(stdout, "%%%%BoundingBox: (atend)\n");

}

void documentPaperSizes(char *size)
/**********************************************************************
documentPaperSizes 
**********************************************************************
Author: John Stockwell, CWP, June 1996
**********************************************************************/
{
	fprintf(stdout, "%%%%DocumentPaperSizes: %s\n",size);
}

void orientation(char *direction)
/**********************************************************************
orientation 
**********************************************************************
Author: John Stockwell, CWP, June 1996
**********************************************************************/
{
	fprintf(stdout, "%%%%Orientation: %s\n",direction);
}

void endProlog(int pages)
/**********************************************************************
endProlog 
**********************************************************************
Author: John Stockwell, CWP, June 1996
**********************************************************************/
{
	if (pages==1) {
		fprintf(stdout,"/__CWPbdef {\n");
		fprintf(stdout,"    1 index where {\n");
		fprintf(stdout,"        pop pop pop\n");
		fprintf(stdout,"    } {\n");
		fprintf(stdout,"        bind def\n");
		fprintf(stdout,"    } ifelse\n");
		fprintf(stdout,"} bind def /__CWPRectPath {\n");
		fprintf(stdout,"    4 2 roll moveto 1 index 0 rlineto 0 exch rlineto neg 0 rlineto closepath\n");
		fprintf(stdout,"} __CWPbdef /__CWPProcessRectArgs {\n");
		fprintf(stdout,"    1 index type /arraytype eq {\n");
		fprintf(stdout,"        exch 0 4 2 index length 1 sub {\n");
		fprintf(stdout,"            dup 3 add 1 exch {\n");
		fprintf(stdout,"                1 index exch get exch\n");
		fprintf(stdout,"            } for 5 1 roll 5 index exec\n");
		fprintf(stdout,"        } for pop pop\n");
		fprintf(stdout,"    } {\n");
		fprintf(stdout,"        exec\n");
		fprintf(stdout,"    } ifelse\n");
		fprintf(stdout,"} __CWPbdef /rectfill {\n");
		fprintf(stdout,"    gsave newpath {\n");
		fprintf(stdout,"        __CWPRectPath fill\n");
		fprintf(stdout,"    } __CWPProcessRectArgs grestore\n");
		fprintf(stdout,"} __CWPbdef /rectclip {\n");
		fprintf(stdout,"    newpath {\n");
		fprintf(stdout,"        __CWPRectPath\n");
		fprintf(stdout,"    } __CWPProcessRectArgs clip newpath\n");
		fprintf(stdout,"} __CWPbdef\n");
		fprintf(stdout,"%%%%EndComments\n");
		fprintf(stdout,"\n");
		fprintf(stdout,"gsave\n");
		fprintf(stdout,"-20 -28 translate\n");
		fprintf(stdout," /__CWPbasematrix matrix currentmatrix def\n");
		fprintf(stdout,"grestore\n");
		fprintf(stdout,"%%%%EndProlog\n");
	} else {
		fprintf(stdout,"%%%%EndComments\n");
	}
}



void space(void)
/**********************************************************************
space 
**********************************************************************
Author: John Stockwell, CWP, June 1996
**********************************************************************/
{
	fprintf(stdout,"\n");
}

void grestore(void)
/**********************************************************************
grestore 
**********************************************************************
Author: John Stockwell, CWP, June 1996
**********************************************************************/
{
	fprintf(stdout,"grestore\n");
}

void showpage(void)
/**********************************************************************
showpage 
**********************************************************************
Author: John Stockwell, CWP, June 1996
**********************************************************************/
{
	fprintf(stdout,"showpage\n");
}
void gsave(void)
/**********************************************************************
gsave 
**********************************************************************
Author: John Stockwell, CWP, June 1996
**********************************************************************/
{
	fprintf(stdout,"gsave\n");
}

void setUpPage(char *pagestr, char *papersizestr,
		char *mediastr, char *mediatypestr, int orient, 
		int manual, char *slotstr,char *traystr, 
		int *pagedim,int llx, int lly,int urx,int ury, 
		float pllx,float plly,
		float purx,float pury, int pages)
/**********************************************************************
setUpPage 
**********************************************************************
Input:
char *pagestr		PostScript string for given page size
char *papersizestr		PostScript string for identifying print papersize
int orient		=0 Portrait  =1 Landscape
int manual		=0 not manual  =1 manual feed
char *slotstr		PostScript string for given input slot
char *traystr		PostScript string identifying input tray
int *pagedim		Page dimensions for given page size
int llx			BoundingBox lower left x
int lly			BoundingBox lower left y
int urx			BoundingBox upper right x
int ury			BoundingBox upper right y
float pllx		PageBoundingBox lower left x
float plly		PageBoundingBox lower left y
float purx		PageBoundingBox upper right x
float pury		PageBoundingBox upper right y
int pages		number of Pages

Output:
Prints Postscript Page setup information to standard out
**********************************************************************
Author: John Stockwell, CWP, June 1996
**********************************************************************/
{
	int ipllx,iplly,ipurx,ipury;

	/* Begin Setup */
	fprintf(stdout,"%%%%BeginSetup\n");
		
	/* Manual feed */
	if (manual) {
		fprintf(stdout,"%%%%BeginFeature: *ManualFeed True\n");
		space();
		space();
		fprintf(stdout,"<</ManualFeed true>> setpagedevice\n");
		fprintf(stdout,"%%%%EndFeature\n");
	}

	/* Input slot and tray */
	fprintf(stdout,"%%%%BeginFeature: *InputSlot %s\n",slotstr);

	space();
	fprintf(stdout,"%s\n",traystr);
	fprintf(stdout,"%%%%EndFeature\n");

	/* Paper media */
	fprintf(stdout,"%%%%BeginFeature: *MediaType %s\n",mediatypestr);
	space();
	fprintf(stdout,"%s\n",mediastr);
	fprintf(stdout,"%%%%EndFeature\n");

	/* Set up the page size/ page region */
	fprintf(stdout,"%%%%BeginFeature: *PageRegion %s\n",papersizestr);

	space();
	fprintf(stdout,"%s\n",pagestr);
	fprintf(stdout,"%%%%EndFeature\n");

	fprintf(stdout,"%%%%Feature: *Resolution 600dpi\n");
	fprintf(stdout,"%%%%EndFeature\n");

	fprintf(stdout,"%%%%EndSetup\n");
	space();
	space();

	/* Shift PageBoundingBox */
	ipllx = (int) pllx; ipurx = pagedim[0] - ipllx;
	iplly = (int) plly; ipury = pagedim[1] - iplly;

	if (pages==1) {

		fprintf(stdout,"%%%%Page: 1 1\n");
		fprintf(stdout,"%%%%PageBoundingBox: %d %d %d %d\n",
			ipllx,iplly,ipurx,ipury);

		fprintf(stdout,"%%%%PageFonts: (atend)\n");
		fprintf(stdout,"%%%%BeginPageSetup\n");
	}

	/* Input slot and tray */
	fprintf(stdout,"%%%%BeginFeature: *InputSlot %s\n",slotstr);
	
	space();
	fprintf(stdout,"%s\n",traystr);
	fprintf(stdout,"%%%%EndFeature\n");

	/* manual feed */
	if (manual) {
		fprintf(stdout,"%%%%BeginFeature: *ManualFeed True\n");
		space();
		space();
		fprintf(stdout,"<</ManualFeed true>> setpagedevice");
		fprintf(stdout,"%%%%EndFeature\n");
	}

	/* page size/region */
	fprintf(stdout,"%%%%BeginFeature: *PageRegion %s\n",papersizestr);
	space();
	space();

	if (pages==1) {
		fprintf(stdout,"%s\n",pagestr);
		fprintf(stdout,"%%%%EndFeature\n");
		fprintf(stdout,"/__CWPsheetsavetoken save def\n");

		/* orientation */
		if (orient) fprintf(stdout,"-90 rotate\n");
		if (orient) fprintf(stdout,"%d 0 translate\n",-pagedim[0]);

		fprintf(stdout,"%3.1f %3.1f translate\n",pllx,plly);
		gsave();
		fprintf(stdout,"-20 -28 translate\n");
		fprintf(stdout," /__CWPbasematrix matrix currentmatrix def\n");
		grestore();
		gsave();
		fprintf(stdout,"0 0 %d %d rectclip\n", (urx-llx),(ury-lly));
		fprintf(stdout,"%d %d translate\n",-llx,-lly);
		fprintf(stdout,"0 0 translate\n");
		fprintf(stdout,"%%%%EndPageSetup\n");
		gsave();
		fprintf(stdout,"%d %d %d %d rectclip\n",
				llx,lly,(urx-llx),(ury-lly));
		fprintf(stdout,"gsave 1 setgray %d %d %d %d rectfill grestore\n",
			llx,lly,(urx-llx),(ury-lly));

		fprintf(stdout,"userdict /_CWPPreviewSaveObject0 systemdict /save get exec put\n");
		fprintf(stdout,"/showpage {\n");
		space();    
		fprintf(stdout,"} def\n");

		fprintf(stdout,"%%%%BeginFile:\n");
	}
}
	

void pageTrailer(int *pagedim, int orient, float pllx,
			float plly, float purx, float pury)
/**********************************************************************
pageTrailer
**********************************************************************
Author: John Stockwell, CWP, June 1996
**********************************************************************/
{
	int ipllx,iplly,ipurx,ipury;

	/* Shift PageBoundingBox */
	ipllx = (int) pllx; ipurx = pagedim[0] - ipllx;
	iplly = (int) plly; ipury = pagedim[1] - iplly;

	fprintf(stdout,"%%%%EndFile\n");
	fprintf(stdout,"clear userdict /_CWPPreviewSaveObject0 known {\n");
	fprintf(stdout,"    _CWPPreviewSaveObject0 systemdict /restore get exec\n");
	fprintf(stdout,"} if\n");

	grestore();
	grestore();
	showpage();
	fprintf(stdout,"__CWPsheetsavetoken restore\n");
	fprintf(stdout,"%%%%PageTrailer\n");
	fprintf(stdout,"%%%%Trailer\n");
	fprintf(stdout,"%%%%Pages: 1 1\n");

	if (orient)
		fprintf(stdout,"%%%%BoundingBox:%d %d %d %d\n",ipllx,iplly,ipury,ipurx);
	else
		fprintf(stdout,"%%%%BoundingBox:%d %d %d %d\n",ipllx,iplly,ipurx,ipury);
}
void clearUserDict(void)
{
	
	fprintf(stdout,"clear userdict /_CWPPreviewSaveObject0 known {\n");
    	fprintf(stdout,"   _CWPPreviewSaveObject0 systemdict /restore get exec\n");
	fprintf(stdout,"} if\n");
}

void setUserDict(int count)
{	
	fprintf(stdout,"userdict /_CWPPreviewSaveObject0 systemdict /save get exec put\n");
	fprintf(stdout,"%%Page: %d %d\n", count,count);
}
