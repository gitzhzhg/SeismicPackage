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
//========================= COPYRIGHT NOTICE ================================
//====  CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.      ========
//====   PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK       ========
//======================== COPYRIGHT NOTICE =================================


//===========================================================================
//========== Header shift class. Applies shifts from a user       ===========
//========== selected trot file to the velocity file and writes   ===========
//========== the new shifted velocity file.                       ===========
//========== Michael L. Sherrill 11/97                            ===========
//===========================================================================

// $Id: va_header_shift.cc,v 1.3 2004/08/05 19:16:32 cornkc Exp $
// $Name: 05Aug04 $





#include <stdio.h>
#include <Xm/Label.h>
#include <stdlib.h>
#include <stdio.h>
#include "tfio.h"
#include "tfdefs.h"
#include "vaplots/va_header_shift.hh"
#include "vaplots/va_cmp_plot.hh"
#include "vaplots/va_plot.hh"
#include "sp/seis_plot.hh"
#include "plot_image.hh"
#include "sl/shell_watch.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_file_base.hh"
#include "vf/vf_informer.hh"
#include "vf/vf_read_save.hh"
#include "vf/vf_diskfile.hh"
#include "sl/sl_error_pop.hh"
#include "cprim.h"
#include "sp/seis_plot.hh"
#include "plot_image.hh"


#define MIS_MATCH "Could not match the following cmp file locations.\nZero shifts applied to these locations:\n"
#define LESS_THAN_ZERO "Some times were shifted to less than zero\nand have been reset to zero."


static String  defres[]= {
  "*shift_headerL.labelString:   Shift Header",
  "*match_headerL.labelString:   Match Header",
  "*toleranceL.labelString:      Tolerance",
  "*float_to_final.labelString:  Floating to final with velocity conversion",
  "*remove_shift.labelString:    Remove post shift no velocity conversion", 
    NULL };




//===========================================================================
//======== The following class provides the trot file reading ===============
//===========================================================================

VaReadShiftHeaders::VaReadShiftHeaders(char *description) 
      : FileBase(description,"trc*"),_first_time(True)
{
}

VaReadShiftHeaders::~VaReadShiftHeaders()
{
}


FileBase::Result VaReadShiftHeaders::virtualRead(const char *, char *)
{
  printf("\nvirtualRead in headers called\n");
  return FileBase::SUCCESS;
}

FileBase::Validity VaReadShiftHeaders::virtualValidate(const char *filename,
                                                       char * /*info*/)
{
TF_Global G;
int istat;

  if (ioIndex() == 1) return (FileBase::VALID_YES);
  Validity retval= FileBase::VALID_YES;
  if( !lastInputFilenameValidated() ) 
    {
    get_global_data_((char *)filename, &G, &istat);
    if( istat == 0) 
      {
      retval= FileBase::VALID_YES;
      _first_time = False;
      }
    else
      {
      retval= FileBase::VALID_NO;
      }
    } 
      
  return retval;

}





FileBase::Prepare VaReadShiftHeaders::virtualPrepareRead (const char* /*name*/,
                                                          char* /*errmsg*/)
{
  return GODSPEED;
}

/*
void VaReadShiftHeaders::postNewFilename()
{
}
*/


//============================================================================
//======= The following is the main class for providing the shifts ===========
//============================================================================

//============================================================================
//====================== Constructor =========================================
//============================================================================
VaHeaderShift::VaHeaderShift( Widget     p,
                              char       *name,
                              HelpCtx    hctx,
                              VfManager  *vf_manager,
                              VaCmpPlot  *cmp_plot) 
                            : SLFPopSep(p,name,FP_DOALL,hctx,True,False)
{
static SLText text1[]  = 
 {
  {"shift_header","range:1 9999999, default:40",NULL, SLType_int,SHIFT_HEADER}, 
  {"match_header","range:1 9999999, default:37",NULL, SLType_int, MATCH_HEADER},
  {"tolerance", "range:0.0001 *, default:2.000",NULL, SLType_float,TOLERANCE}
 };
 text1[0].target= &_shift_header;
 text1[1].target= &_match_header;
 text1[2].target= &_tolerance;

static SLRadio radios[]  = 
 {
  { "float_to_final", FLOAT_TO_FINAL },
  { "remove_shift",   SHIFT_ONLY }
 };


  setDefaultResources( p, name, defres);

  _shifted_dataset = NULL;

  _shifted_manager = new VfManager(1,1,"VA");
  _trc_filebase = new VaReadShiftHeaders("Header File");
  _trc_file_choice = new SLFileChoice (this, "trc_choice", SLpFile::_INPUT,
                                        _trc_filebase,
                                        "File Containing Shifts...",
                                        NULL, TRUE);
  _vel_filebase = new VfFileBase("VelocityFile", "vel", _shifted_manager,
                                 VfFileBase::USE_FOR_OUTPUT, False);
  _vel_file_choice = new SLFileChoice(this, "vel_choice", SLpFile::_OUTPUT,
                                      _vel_filebase,
                                      "Shifted Velocity File Output...",
                                      NULL, TRUE);
  _vel_file_choice->setComplexNotify(this);
                           

  _vf_manager = vf_manager;
  _cmp_plot   = cmp_plot;
  _first_time = True;
  _shift_vals = NULL;
  _trace_xlocs = NULL;
  _trace_ylocs = NULL;
  _num_locations = 0;
  _new_times = (float *) malloc( sizeof(float) );
  _new_vels =  (float *) malloc( sizeof(float) );
  _temp_times = (float *) malloc( sizeof(float) );
  _temp_vels = (float *) malloc( sizeof(float) );
  _vfd_xlocs = (float *) malloc( sizeof(float) );
  _vfd_ylocs = (float *) malloc( sizeof(float) );
  _missing_locations = (float *) malloc( sizeof(float) );
  _missing_location_string = (char *) malloc( sizeof(char) );

 
  _text_box =  new SLTextBox( this, "text_box", hctx, text1,
                              XtNumber(text1), True);
  _text_box->setAltLosingAction( (SLTextfunc)losingFocusAction, this );
  

  _radio = new SLRadioBox (this, "radios", hctx, radios, XtNumber(radios),
                           NULL, True);
  _radio->SetRadio(FLOAT_TO_FINAL);

}


//============================================================================
//====================== Destructor          =================================
//============================================================================
VaHeaderShift::~VaHeaderShift()
{
  if(_shift_vals)              free (_shift_vals);
  if(_trace_xlocs)             free (_trace_xlocs);
  if(_trace_ylocs)             free (_trace_ylocs);
  if(_vfd_xlocs)               free (_vfd_xlocs);
  if(_vfd_ylocs)               free (_vfd_ylocs);
  if(_new_times)               free (_new_times);
  if(_new_vels)                free (_new_vels);
  if(_temp_times)              free (_temp_times);
  if(_temp_vels)               free (_temp_vels);
  if(_missing_locations)       free (_missing_locations);
  if(_missing_location_string) free (_missing_location_string);
}


//============================================================================
//====================== Make method   =======================================
//============================================================================
Widget VaHeaderShift::make(Widget p)
{

   if ( made() ) return topWidget();
   _trace_filename = _cmp_plot->SP()->filename();


   SLFPopSep::make(p);

   setTitle("Header Shifts");

   XtVaSetValues( _trc_file_choice->W(),
                                   XmNtopAttachment,   XmATTACH_FORM,
                                   XmNtopOffset,       10,
                                   XmNleftAttachment,  XmATTACH_FORM, 
                                   XmNleftOffset,      10, 
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNrightOffset,     10, NULL );
   
   XtVaSetValues( _vel_file_choice->W(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _trc_file_choice->W(),
                                   XmNtopOffset,       5,
                                   XmNleftAttachment,  XmATTACH_FORM, 
                                   XmNleftOffset,      10, 
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNrightOffset,     10, NULL );

   XtVaSetValues( _text_box->W(),  XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _vel_file_choice->W(),
                                   XmNtopOffset,       10,
                                   XmNleftAttachment,  XmATTACH_FORM, 
                                   XmNleftOffset,      10, 
                                   NULL);
   Widget tmp=  XtVaCreateManagedWidget( "", 
                                   xmLabelWidgetClass, topWidget(),
                                   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _text_box->W(),
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNbottomAttachment,XmATTACH_WIDGET,
                                   XmNbottomWidget,    bottomSeparator(),
                                   XmNtopOffset,      5,
                                   XmNleftOffset,     5,
                                   XmNbottomOffset,   25,
                                   NULL);


   XtVaSetValues( _radio->W(),     XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _vel_file_choice->W(),
                                   XmNtopOffset,       35,
                                   XmNleftAttachment,  XmATTACH_WIDGET, 
                                   XmNleftWidget,      _text_box->W(),
                                   XmNleftOffset,      50, 
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNrightOffset,     10, NULL );

   Widget datum_label = XtVaCreateManagedWidget( "Datum shift",
                                   xmLabelWidgetClass,topWidget(),
                                   XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                                   XmNleftWidget,     _radio->W(),
                                   XmNleftOffset,     100, 
                                   XmNbottomAttachment,XmATTACH_WIDGET,
                                   XmNbottomWidget,   _radio->W(),
                                   XmNbottomOffset,   5, NULL );

   _radio->SetRadio( FLOAT_TO_FINAL);

   return topWidget();

}


Boolean VaHeaderShift::notifyComplex(SLDelay *obj, int ident)
{
VfDataset *vfd = _vf_manager->activeDataset(); 


  //this method is only used to delay deleting of the new shifted
  //functions until it is safe to do so. If a vel file is being
  //over written the user will get a question pop to confirm so
  //we do not want to get rid of the shifted functions until after
  //confirmation.

  if(_shifted_dataset == NULL) return True;

  if(obj == _vel_file_choice) 
    {
    if(ident == FP_APPLY)//file written successfully
      {
      _shifted_dataset->deleteAllVelocityFunctions();
      vfd->informer()->afterChanges();
      }  
    if(ident == FP_NONE)//file not written but we still want to delete functions
      {
      _shifted_dataset->deleteAllVelocityFunctions();
      vfd->informer()->afterChanges();
      }
    }

   

  return True;
}


//============================================================================
//====================== Handle ok, apply, cancel buttons ====================
//============================================================================
void VaHeaderShift::DoAction()
{
int stat;
ShellWatch watch = topWidget(); 
VfDataset *vfd = _vf_manager->activeDataset(); 

  _shifted_dataset = _shifted_manager->activeDataset();

  _shifted_dataset->setNhx(vfd->getNhx());
  _shifted_dataset->setNhy(vfd->getNhy());


  switch(whichButton())
    {
    case FP_OK:
    case FP_APPLY:
      vfd->informer()->beforeChanges();
      _shifted_dataset->replaceVelocityFunctions(vfd);
      vfd->informer()->afterChanges();
      stat = getHeaders();
      if(!stat)
         { 
         SLErrorPop *errpop = 
             new SLErrorPop(topWidget(), "Error",
                            "Could not crack file headers.");
         break;
         }
      stat = computeNewVelocities();
      if(!stat)
         { 
         SLErrorPop *errpop = 
             new SLErrorPop(topWidget(), "Error",
                            "Not enough memory");
         break;
         }
    break;

    case FP_CANCEL://free memory by reducing array sizes to one element
      if(_shift_vals != NULL)
        {
        _shift_vals = (float *)realloc(_shift_vals, sizeof(float));
        }
      if(_trace_xlocs != NULL)
        {
        _trace_xlocs = (float *)realloc(_trace_xlocs, sizeof(float));
        }
      if(_trace_ylocs != NULL)
        {
        _trace_ylocs = (float *)realloc(_trace_ylocs, sizeof(float));
        }
      if(_vfd_xlocs != NULL)
        {
        _vfd_xlocs = (float *)realloc(_vfd_xlocs, sizeof(float));
        }
      if(_vfd_ylocs != NULL)
        {
        _vfd_ylocs = (float *)realloc(_vfd_xlocs, sizeof(float));
        }
    break;
    }

}

//============================================================================
//====================== Method to undo input, not currently used ============
//============================================================================
void VaHeaderShift::UndoInput()
{

}


//============================================================================
//====================== Manage method =======================================
//============================================================================
void VaHeaderShift::manage()
{
VfDataset *vfd = _vf_manager->activeDataset(); 

 if(!vfd->numVelocityFunctions() || vfd->numPicks(0) < 2)
   {
   SLErrorPop *errpop = new SLErrorPop(topWidget(), "Error", 
                         "Velocities must be picked or loaded from a file");
   return;
   }

 SLBase::manage();
 if(_first_time)
   {
   _shift_header = 40;
   _match_header = (int)vfd->getNhx();
   _tolerance = 2.0;
   _text_box->SetValue(SHIFT_HEADER, (long)_shift_header);
   _text_box->SetValue(MATCH_HEADER, (long)_match_header);
   _text_box->SetValue(TOLERANCE,    (float)_tolerance);
   _first_time = False;
   _trace_filename = _cmp_plot->SP()->filename();
   _trc_filebase->setInputFilename((const char*)_trace_filename);
   _trc_file_choice->updateFieldsFromFileBase();
   _vel_filebase->setOutputFilename((const char*)"shifted.vel");
   _vel_file_choice->updateFieldsFromFileBase();
   } 

  _trc_file_choice->updateFields();
  _vel_file_choice->updateFields();
}


//============================================================================
//====================== Reload defaults     =================================
//============================================================================
void VaHeaderShift::reloadDefaults(Boolean /*do_method*/)
{
  DoAction();
}


//============================================================================
//====================== Reload system defaults ==============================
//============================================================================
void VaHeaderShift::reloadSystemDefaults(Boolean /*do_method*/)
{
  DoAction();
}


//============================================================================
//====================== Handle value changes here later if needed ===========
//============================================================================
void VaHeaderShift::losingFocusAction(void * /*data*/, long ident)
{

  switch(ident)
    {
      case SHIFT_HEADER:
      break;
     
      case TOLERANCE:
      break;
    }
}


//============================================================================
//================ Open the file and obtain the shift values ============
//============================================================================
int VaHeaderShift::getHeaders()
{
float *hd;
float *traces;
int normalize = 0;
unsigned char *bytes;
char msg[81];
int istat, cl_stat = 0;
long  i, j, k;
int count;
float current_group, group1 = 0;
float *tasf;
int tr_read, maxread;
int tr_start;
long ntot;
int too_big;
int nhdrs, jreadonly = 0;
Boolean last_try = False;  
long headersize;
long traces_in_group;
TF_Global G;
IO_request Cl;
float shift, average_shift;
long live_traces;
int open_file = 1;
int lav_header_index = 24;


 get_global_data_(_trace_filename, &G, &istat);
 if( istat > 0) return False;

 maxread = min(READ10, G.ntrfil);
 tr_read = maxread;

/*allocate arrays to read in data*/
 headersize = max( G.nhdwd,
                   (_cmp_plot->SP()->numHeaders()*(2*sizeof(float))) );
 hd  = (float *) malloc( (int)(maxread * headersize) );
 if(hd == NULL) return False;
 traces = (float *)malloc(maxread * sizeof(float));
 if(traces == NULL)
   {
   free (hd);
   return False;
   }

 tasf = (float *) malloc( (maxread + 1) * (sizeof(float)));
 if(tasf == NULL)
   {
   free (hd);
   free(traces); 
   return False;
   }

 //Can get rid of this when old byte type disappears
 bytes = (unsigned char *) malloc(maxread * (4*sizeof(float)));
 if(bytes == NULL) 
   {
   free (hd);
   free(traces);
   free(tasf);
   return False;
   }
 
 Cl.ntot = maxread;
 Cl.ndo = maxread;
 Cl.nsamp = 1;
 Cl.samp1 = 1;
 Cl.sdec = 1;
 Cl.iskp = 0;
 Cl.nskp = 0;
 Cl.trnsps = 0;
 Cl.cnvrt = 0;
 Cl.axis = 0;
 Cl.index = 0;
 ntot = maxread;
 nhdrs = _cmp_plot->SP()->numHeaders();

 if(G.lun < 0) G.lun = -G.lun;

/*read in 10 traces at a time*/
 count = 0;
 while(count == 0)
    {
    //tr_start = Cl.iskp + 1;

    if(strcmp(G.ftyp,"CBYTE") == 0 )//old byte type
      {
      istat = read_byt_(_trace_filename, &istat, &Cl, &normalize, &nhdrs,
                        (char *)hd, bytes, tasf);
      if(istat != 0)
        {
        tf_glbl_getn_( &istat,  _trace_filename, &G); 
        tf_close_(&G.lun, &istat, msg);
        free(hd);      hd    = NULL;
        free(traces); traces = NULL;
        free(tasf);    tasf  = NULL;
        free(bytes);   bytes = NULL;
        return False;
        }
      }
    else
      {
      istat = read_data_(_trace_filename, &istat, &Cl,
                      (char *)hd, (char *)traces, tasf, &G.lun, open_file);
      if(istat != 0)
        {
        tf_trcio_close_file_(G.lun, &istat);
        free(hd);      hd    = NULL;
        free(traces); traces = NULL;
        free(tasf);    tasf  = NULL;
        free(bytes);   bytes = NULL;
        return False;
        }
      }

    open_file = 0;

    if(!Cl.iskp)group1 = hd[GRPHDR];//first header id
    /*search the 10 new traces*/
    for(i=0;i<tr_read;i++)
       {
       current_group = hd[i * nhdrs + GRPHDR];
       if(current_group != group1) /*stop have default*/
          {
          count = (int)i + Cl.iskp;
          i = Cl.iskp + tr_read;
          }
       }

   /*may have only one panel if we havent found defaults on last read*/
    if(!count && last_try)  count = G.ntrfil;

   /*increment skip to read next 10 traces*/
   if(!count)
      {
      Cl.iskp += tr_read;
      if(Cl.iskp >= G.ntrfil) /*end of file need to redo this later*/
         {
         too_big = Cl.iskp - G.ntrfil;
         Cl.iskp = maxread - too_big;
         if(Cl.iskp >= G.ntrfil)Cl.iskp = G.ntrfil - 1;
         tr_read = too_big;
         last_try = True;
         }
      }

   } /*end while*/

  if(strcmp(G.ftyp,"CBYTE") == 0 )//old byte type
     tf_glbl_getn_( &istat,  _trace_filename, &G); 
  tf_close_(&G.lun, &istat, msg);

//Ok now have trace count info now loop thru and get information at each cdp
  traces_in_group = count;
  _num_locations = G.ntrfil / count;

  if(_trace_xlocs == NULL)
    _trace_xlocs = (float *) malloc( (int)_num_locations * sizeof(float));
  else
    _trace_xlocs = (float *) realloc(_trace_xlocs,  
                                         (int)_num_locations * sizeof(float));
  if(_trace_xlocs == NULL)
    {
    free(hd);      hd    = NULL;
    free(traces); traces = NULL;
    free(tasf);    tasf  = NULL;
    free(bytes);  bytes  = NULL;
    return False;
    }

  if(_trace_ylocs == NULL)
    _trace_ylocs = (float *) malloc( (int)_num_locations * sizeof(float));
  else
    _trace_ylocs = (float *) realloc(_trace_ylocs,  
                                         (int)_num_locations * sizeof(float));
  if(_trace_ylocs == NULL)
    {
    free(hd);     hd    = NULL;
    free(traces); traces = NULL;
    free(tasf);   tasf  = NULL;
    free(bytes);  bytes  = NULL;
    return False;
    }

  if(_shift_vals == NULL)
    _shift_vals      = (float *) malloc( (int)_num_locations * sizeof(float)); 
  else
    _shift_vals = (float *) realloc(_shift_vals,  
                                         (int)_num_locations * sizeof(float));
  if(_shift_vals == NULL)
    {
    free(hd);     hd    = NULL;
    free(traces); traces = NULL;
    free(tasf);   tasf  = NULL;
    free(bytes);  bytes = NULL;
    return False;
    }

  hd = (float *) realloc(hd, (int)(count * headersize));
  if(hd == NULL)
    {
    free(traces); traces = NULL;
    free(tasf);   tasf  = NULL;
    free(bytes);  bytes  = NULL;
    return False;
    }

  traces = (float *)realloc(traces, count * sizeof(float));
  if(traces == NULL)
    {
    free(hd);     hd    = NULL;
    free(tasf);   tasf  = NULL;
    free(bytes);  bytes  = NULL;
    return False;
    }

  tasf = (float *) realloc(tasf, (int) (count+1) * sizeof(float));
  if(tasf == NULL)
    {
    free(hd);     hd    = NULL;
    free(traces); traces = NULL;
    free(bytes);  bytes  = NULL;
    return False;
    }

  //Get rid of this after byte goes away
  bytes = (unsigned char *) realloc(bytes, count * (4*sizeof(float)));
  if(bytes == NULL)
    {
    free(hd);     hd    = NULL;
    free(traces); traces = NULL;
    free(tasf);   tasf  = NULL;
    return False;
    }
  

//Loop thru all data and get shifts
  Cl.ntot = count;
  Cl.ndo  = count;
  Cl.nskp = 0;
  Cl.iskp = 0;
  open_file = 1;



// This is a cluge because we foolishly only ask for one _match_header. We
//   need to ask for a X-match header and a Y-match header. The cluge is
//   that if the X-header given at file import is used, then it will be
//   assumed that the also the given Y-header at file import is also wanted
//   instead of merely incrementing one to take the next header as it is now.
//   In the case that the originally given X-header is not used, then as it
//   is now, we will merely increment the user specified X-match header.
  VfDataset *vfd = _shifted_manager->activeDataset(); 
  int x_header_index, y_header_index;
  if (_match_header == (int)vfd->getNhx()) {
    x_header_index = _match_header - 1;
    y_header_index = (int)vfd->getNhy() - 1;
  }
  else {
    x_header_index = _match_header - 1;
    y_header_index = _match_header    ;
  }
  for(j = 0; j < _num_locations; j++)
    {
    if(strcmp(G.ftyp,"CBYTE") == 0 )//old byte type
      {
      istat = read_byt_(_trace_filename, &istat, &Cl, &normalize, &nhdrs,
                        (char *)hd, bytes, tasf);
       if(istat != 0)
        {
        tf_glbl_getn_( &istat,  _trace_filename, &G); 
        tf_close_(&G.lun, &istat, msg);
        free(hd);      hd    = NULL;
        free(traces); traces = NULL;
        free(tasf);    tasf  = NULL;
        free(bytes);   bytes = NULL;
        return False;
        }
      }
    else
      {
      istat = read_data_(_trace_filename, &istat, &Cl,
                     (char *)hd, (char *)traces, tasf, &G.lun, open_file);

      if(istat != 0)
        {
        tf_trcio_close_file_(G.lun, &istat);
        free(hd);      hd    = NULL;
        free(traces); traces = NULL;
        free(tasf);    tasf  = NULL;
        free(bytes);   bytes = NULL;
        return False;
        }
      }
    open_file = 0;


    shift = 0.0;
    average_shift = 0.0;
    live_traces = 0;
    for(k = 0; k < count; k++)//get average shift of live traces
      {                      
      if(hd[k * nhdrs + lav_header_index] != 0.0) //Not a dead trace
        {
        ++live_traces;
        shift += hd[k * nhdrs + _shift_header - 1];
        }
      }
    if(live_traces)  average_shift = shift / (float)live_traces;
    _trace_xlocs[j] = hd[x_header_index];
    _trace_ylocs[j] = hd[y_header_index];
    _shift_vals[j]  = average_shift / 1000.0;
    if(_radio->WhichSelected() == SHIFT_ONLY)
       _shift_vals[j] = -(_shift_vals[j]);
    Cl.iskp += count;
    }

  if(strcmp(G.ftyp,"CBYTE") == 0 )//old byte type
     tf_glbl_getn_( &istat,  _trace_filename, &G);
  tf_close_(&G.lun, &cl_stat, msg);

  free(hd);     hd    = NULL;
  free(traces); traces = NULL;
  free(tasf);   tasf  = NULL; 

  return True;


}






//============================================================================
//============ Compute the new time velocity pairs ===========================
//============================================================================
int VaHeaderShift::computeNewVelocities()
{
float last_rms = 0.0;
float last_time = 0.0;
long found_match;
float time_array[MAXPICKS];
float velocity_array[MAXPICKS];
long i, j;
float last_vel;
float last_shifted_time;
float this_shifted_time;
float velocity;
int npicks = 0, previous_npicks;
int missed_location = 0;
char temp_string[20];
int str_size;
int total_picks = 0;
int negative_times = False;
int pop_up_negative_warning = False;
float tempv;
VfDataset *vfd = _shifted_manager->activeDataset(); 
char msg[222];

  //make local copy of vels and locations
  if(_vfd_xlocs == NULL)
    _vfd_xlocs = (float *) malloc( (int)vfd->numVelocityFunctions() 
                                    * sizeof(float));
  else
    _vfd_xlocs = (float *)realloc(_vfd_xlocs, (int)(vfd->numVelocityFunctions() 
                                                  * sizeof(float)));
  if(_vfd_xlocs == NULL) return False;


  if(_vfd_xlocs == NULL)
    _vfd_xlocs = (float *) malloc( (int)vfd->numVelocityFunctions() 
                                    * sizeof(float));
  else
    _vfd_ylocs = (float *)realloc(_vfd_ylocs, (int)(vfd->numVelocityFunctions() 
                                                  * sizeof(float)));
  if(_vfd_ylocs == NULL) return False;


  //fill arrays of velocity function locations
  if(_new_times == NULL)
    _new_times = (float *) malloc( sizeof(float));
  if(_new_vels == NULL)
    _new_vels = (float *) malloc( sizeof(float));

  for(i = 0; i < vfd->numVelocityFunctions(); i++)
    {
    vfd->getAbscissaArray(i, time_array, VTNM);
    vfd->getOrdinateArray(i, velocity_array, VTNM);
    previous_npicks = total_picks;
    total_picks += (int)vfd->numPicks(i);
    _new_times = (float *) realloc(_new_times, total_picks * sizeof(float));
    if(_new_times == NULL) return False;
    _new_vels  = (float *) realloc(_new_vels,  total_picks * sizeof(float));
    if(_new_vels == NULL)  return False;
    //warning the following saves the values without any matching order
    for(j = 0; j < vfd->numPicks(i); j++)
      { 
      _new_times[j + previous_npicks] = time_array[j];
      _new_vels[ j + previous_npicks] = velocity_array[j];
      }
    _vfd_xlocs[i] = vfd->getXloc(i);
    _vfd_ylocs[i] = vfd->getYloc(i);
    }


  //now find matches and do the work
  for(i = 0; i < _num_locations; i++)
    {
    found_match = _cmp_plot->findArrayMatch(_trace_xlocs[i], _tolerance,
                                            _trace_ylocs[i], _tolerance,
                                            _vfd_xlocs,      _vfd_ylocs,
                                            vfd->numVelocityFunctions(), 
                                            1, True);
    if(found_match >= 0)
      {
      vfd->getAbscissaArray(found_match, time_array, VTNM);
      vfd->getOrdinateArray(found_match, velocity_array, VTNM);
      npicks = (int)vfd->numPicks(found_match);
      for(j = 0; j < npicks; j++)
        {
        if(_radio->WhichSelected() != SHIFT_ONLY)//Apply to interval velocities
          {
          if(j)
            {
            last_vel  = velocity_array[j-1];
            last_time = time_array[j-1];
            }
          else
            {
            //make sure 1st picks are stored in matching order
            _new_vels [j] = velocity_array[j];
            _new_times[j] = time_array[j];
            last_vel  = velocity_array[j];
            last_time = time_array[j];
            }
          if(time_array[j] != 0.0)
            {
            velocity=(velocity_array[j] * velocity_array[j] * time_array[j] - 
                last_vel * last_vel * last_time) / (time_array[j] - last_time);
            if(velocity < 0.0) velocity = -velocity;
            velocity = sqrt(velocity);
            _new_times[j] =  time_array[j] + _shift_vals[i];
            this_shifted_time = _new_times[j];
            if(this_shifted_time == 0.0) this_shifted_time = .0000001;
            if(_new_times[j] < 0.0)
              {
              negative_times = True;
              _new_times[j] = 0.0;
              }
            if(last_time != 0) 
              {
              last_shifted_time = last_time + _shift_vals[i];
              }
            else 
              {
              last_shifted_time = 0;
              }
            tempv = (last_rms * last_rms * 
                    last_shifted_time + velocity * velocity * 
                   (this_shifted_time - last_shifted_time))/this_shifted_time;
            if(tempv < 0.0)tempv = -tempv;
            _new_vels[j] =  sqrt(tempv);
	    last_rms = _new_vels[j];
            }
          }
        else//bulk shift only, dont recalculate velocities.
          {
          _new_times[j] =  time_array[j] + _shift_vals[i];
          if(_new_times[j] < 0.0)
            {
            negative_times = True;
            _new_times[j] = 0.0;
            }
          _new_vels [j] =  velocity_array[j];
          }
        }//end for j
      if(negative_times)
        {
        checkNewPicks(found_match, &_new_times[0],&_new_vels[0]);
        negative_times = False;
        pop_up_negative_warning = True;
        }
      //store the new data
      vfd->resetNumPicks(found_match, npicks, &_new_times[0],&_new_vels[0],
                         VTNM);
      }
    else //store mis-matched locations
      {
      ++missed_location;
      if(_missing_locations == NULL)
         _missing_locations = (float *) malloc( sizeof(float));
      _missing_locations = (float *) realloc(_missing_locations, 
                                           missed_location * sizeof(float));
      if(_missing_locations == NULL) return False;
      _missing_locations[missed_location - 1] = _trace_xlocs[i];
      }
    }//end of for i thru _num_locations
  

  if(pop_up_negative_warning)
    {
    SLErrorPop *errpop = new SLErrorPop(topWidget(), "Warning",
                                        LESS_THAN_ZERO);
    }

  if(missed_location)
    {
    str_size = missed_location * 12 * sizeof(char) + 
               strlen(MIS_MATCH) * sizeof(char);
    if(_missing_locations == NULL)
         _missing_locations = (float *) malloc( sizeof(char));
    _missing_location_string = (char *) realloc(_missing_location_string,  
                                         str_size);
    if(_missing_location_string == NULL) return False;
    strcpy(_missing_location_string,MIS_MATCH);
     
    for(i = 0; i < missed_location; i++)
      {
      sprintf(temp_string,"%8.2f\n",_missing_locations[i]);
      strcat(_missing_location_string,temp_string);
      }
    SLErrorPop *errpop = new SLErrorPop(topWidget(), "Warning", 
                                        _missing_location_string);
    }


  //Write the new data
   _vel_filebase->setType(VTNM);
   //_vel_filebase->readsave()->diskfile()->updatePjarFromKernal(
   //                _shifted_dataset->kernal(),NULL, 0);
   //_vel_file_choice->takeAction(this,FP_APPLY);
   int stat = _shifted_dataset->saveVelocityFile(
                  _vel_file_choice->getFileBase()->outputFilename(), msg,
                  _vel_filebase->readsave());

  return True;

}


//============================================================================
//============ Check the new times and interpolate values. If there     ======
//============ are duplicate zero times I do not delete the duplicates  ======
//============ because I want to keep the same number of picks in the   ======
//============ new file as there is in memory since I have to restore   ======
//============ the time velocity pairs in memory                        ======
//============ So this is what will happen to multiple picks at zero    ======
//============ time:
//============       shifted picks coming in    picks after this method ======
//============       -----------------------    ----------------------- ======
//============        0.0   1500                0.0   3500              ======
//============        0.0   2500                 .033 3833              ======
//============        0.0   3500                 .066 4166              ======
//============         .1   4500                 .1   4500              ======
//============         .2   5500                 .2   5500              ======
//============================================================================
void VaHeaderShift::checkNewPicks(long ifun, float *newtimes, float *newvels)
{
int j, k, l;
float time_array[MAXPICKS];
float velocity_array[MAXPICKS];
long current, next;
long npicks = 0;
long number_duplicates;
float first_non_zero_time;
float first_non_zero_vel;
float first_vel,time_increment;
float vel_increment; 
VfDataset *vfd = _shifted_manager->activeDataset(); 

  vfd->getAbscissaArray(ifun, time_array, VTNM);
  vfd->getOrdinateArray(ifun, velocity_array, VTNM);
  npicks = (int)vfd->numPicks(ifun);
  number_duplicates = 0;

  for(j = 0; j < npicks - 1; j++)
    {
    current = j;
    next    = current + 1;
    if(newtimes[current] == 0.0 && newtimes[next] == 0.0)
       number_duplicates += 1;
    }

  if(number_duplicates > 0)
    {
    number_duplicates += 1;
    first_non_zero_time = newtimes[number_duplicates];
    first_non_zero_vel  = newvels [number_duplicates];
    first_vel = newvels[number_duplicates - 1];
    time_increment = first_non_zero_time / number_duplicates;
    vel_increment = (first_non_zero_vel - first_vel) / number_duplicates;
    for(k = 0; k < number_duplicates; k++)
      {
      newtimes[k] += k * time_increment;
      newvels [k] = first_vel + k * vel_increment;
      }
    number_duplicates = 0;
    }

  for(l = 0; l < npicks; l++)
    {
    time_array[l]     = newtimes[l];
    velocity_array[l] = newvels[l];
    }

}

