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
//********************************************
//Author Michael L. Sherrill 02/97
//Class to apply nmc and write a trot file
//using va's velocity file.
//********************************************

// $Id: va_nmc_output.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include <stdio.h>
#include <stdlib.h>
#include <Xm/Label.h>
#include "tfio.h"
#include "tfdefs.h"
#include "trslib.h"
#include "sl/sl_error_pop.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_tog_box.hh"
#include "cprim.h"
#include "inquire.h"
#include "vaplots/va_plot.hh"
#include "vaplots/va_nmc_output.hh"
#include "vaplots/va_cmp_plot.hh"
#include "vaplots/va_iso_plot.hh"
#include "vf/vf_moveout.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_informer.hh"
#include "sl/sl_file_pair_plus.hh"
#include "sp/seis_plot.hh"

 
#define MIS_MATCH "Could not match the following velocity locations.\nNo nmc was applied to these locations :\n"
#define FORWARD_NMC 1

static String  defres[]= {
    NULL, };



enum {TOLERANCE, ICMP};

//============================================================================
//====================== Constructor =========================================
//============================================================================
VaNmcOutput::VaNmcOutput(   Widget  p,
                            char    *name,
                            HelpCtx hctx,
                            VfManager  *vf_manager,
                            VaCmpPlot  *cmp_plot,
                            VaIsoPlot  *iso_plot ) 
                            : SLFPopSep(p,name,FP_DOALL,hctx,True,False)
{
static SLText text1[]  = 
 {
  {"tolerance", "range:0.0001 *, default:1.000",NULL, SLType_float,TOLERANCE}
 };
 text1[0].target= &_tolerance;


static SLTog irregular_cmp[] = {
  {"Irregular", NULL, ICMP },
};


  setDefaultResources( p, name, defres);

  _vf_manager              = vf_manager;
  _cmp_plot                = cmp_plot;
  _iso_plot                = iso_plot;
  _first_time              = True;    
  _tolerance               = 1.0;
  _missing_locations       = (float *) malloc( sizeof(float) );
  _missing_location_string = (char *)  malloc( sizeof(char) );
  _times                   = (float *) malloc( sizeof(float) );
  _velocities              = (float *) malloc( sizeof(float) );
  _vfd_xlocs               = (float *) malloc( sizeof(float) );
  _vfd_ylocs               = (float *) malloc( sizeof(float) );
  _headers                 = (float *) 
                                 calloc(1,PlotImage::MAXHDRS*(sizeof(double)));
  _floats                  = (float *) calloc(1, (2*sizeof(float)));
  _tasf                    = (float *) calloc(1,(2*sizeof(float)));
  _bytes                   = (unsigned char *) 
                                 calloc(1, (2*sizeof(unsigned char)));
  _vf_moveout              = new VfMoveout();

 
  _file_pair = new SLFilePairPlus( this, "nmc_file_pair", hctx, True,
                                   "Trot file", "Trot file",
                                   "trc8", "trc", True, True, "", "_nmc");
  _file_pair->registerValidateTrap( validateTrap, this);

  _text_box =  new SLTextBox( this, "text_box", hctx, text1,
                              XtNumber(text1), False, 2, True, False);

  _irregular_cmp = new SLTogBox(this, "Irregular", hctx,
                                irregular_cmp, XtNumber(irregular_cmp));  

}


//============================================================================
//====================== Destructor          =================================
//============================================================================
VaNmcOutput::~VaNmcOutput()
{
  if(_missing_locations)       free (_missing_locations);
  if(_missing_location_string) free (_missing_location_string);
  if(_floats)                  free (_floats);
  if(_bytes)                   free (_bytes);
  if(_tasf)                    free (_tasf);
  if(_headers)                 free (_headers);
  if(_times)                   free (_times);
  if(_velocities)              free (_velocities);
  if(_vfd_xlocs)               free (_vfd_xlocs);
  if(_vfd_ylocs)               free (_vfd_ylocs);
}


Widget VaNmcOutput::make(Widget p)
{

   if ( made() ) return topWidget();
   _input_filename = _cmp_plot->SP()->filename();
   _file_pair->maybeUpdate(_input_filename);
   SLFPopSep::make(p);

   setTitle("Output NMC file");

   XtVaSetValues( _file_pair->W(), XmNtopAttachment,   XmATTACH_FORM,
                                   XmNtopOffset,       10,
                                   XmNleftAttachment,  XmATTACH_FORM, 
                                   XmNleftOffset,      10, 
                                   XmNrightAttachment, XmATTACH_NONE, NULL);

    XtVaSetValues( _irregular_cmp->W(), XmNtopAttachment,   XmATTACH_FORM,
                                   XmNtopOffset,       15,
                                   XmNleftAttachment,  XmATTACH_WIDGET,
                                   XmNleftWidget,      _file_pair->W(), 
                                   XmNleftOffset,      5, 
                                   XmNrightAttachment, XmATTACH_FORM,
                                   XmNrightOffset,     10, NULL );


    XtVaSetValues( _text_box->W(), XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _file_pair->W(),
                                   XmNtopOffset,       50,
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


   Widget text_label = XtVaCreateManagedWidget( 
                                   "Tolerance",
                                   xmLabelWidgetClass,  topWidget(),
                                   XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                                   XmNleftWidget,       _text_box->W(),
                                   XmNleftOffset,       10, 
                                   XmNbottomAttachment, XmATTACH_WIDGET,
                                   XmNbottomWidget,     _text_box->W(),
                                   XmNbottomOffset,     5, NULL );

   return topWidget();

}



//============================================================================
//====================== Callback to handle file name changes ================
//============================================================================
void VaNmcOutput::validateTrap(   void *user_data,
                                  const char * /*filename1*/, 
                                  const char * /*filename2*/,
                                  long *valid1, 
                                  long *valid2,
                                  char * /*info1*/, 
                                  char * /*info2*/,
                                  long * /*same_datasets*/)
{
VaNmcOutput *obj = (VaNmcOutput *)user_data;
int istat;
GLBL local_g;

  obj->_input_filename  = (char *)obj->_file_pair->inputFilename();
  obj->_output_filename = (char *)obj->_file_pair->outputFilename();

  if(strlen(obj->_input_filename) > 1)
    {
    get_global_data_(obj->_input_filename, &local_g, &istat);
    if( istat > 0) 
      {
      *valid1 = INQUIRE_VALID_NO;
      }
    else
      {
      *valid1 = INQUIRE_VALID_YES;
      if(!strcmp(obj->_input_filename, obj->_cmp_plot->SP()->filename()))
        obj->_irregular_cmp->SetTog(ICMP,obj->_cmp_plot->SP()->usingSelector());
      else
        obj->_irregular_cmp->SetTog(ICMP,False);
      }
    }
  else
    {
    *valid1 = INQUIRE_VALID_MAYBE;
    }


  if(strlen(obj->_output_filename) > 1)
    {
    get_global_data_(obj->_output_filename, &local_g, &istat);
    if( istat == 0) 
      {
      *valid2 = INQUIRE_VALID_MAYBE;
      }
    else
      {
      *valid2 = INQUIRE_VALID_YES;
      }
    }
  else
    {
    *valid2 = INQUIRE_VALID_MAYBE;
    }  

}







//============================================================================
//====================== Handle ok, apply, cancel buttons ====================
//============================================================================
void VaNmcOutput::DoAction()
{
int stat;
VfDataset *vfd = _vf_manager->activeDataset(); 

 vfd->informer()->beforeChanges();

 switch(whichButton())
    {
    case FP_OK:
    case FP_APPLY:
      stat = applyNMC();
      if(!stat)
         { 
         SLErrorPop *errpop = 
             new SLErrorPop(topWidget(), "Error", _errmsg);
         }
    _file_pair->closeFile();
    break;

    case FP_CANCEL://free memory by reducing array sizes to one element
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
vfd->informer()->afterChanges();
}


//============================================================================
//====================== Method to undo input, not currently used ============
//============================================================================
void VaNmcOutput::UndoInput()
{

}


//============================================================================
//====================== Manage method =======================================
//============================================================================
void VaNmcOutput::manage()
{
VfDataset *vfd = _vf_manager->activeDataset(); 

 if(!vfd->numVelocityFunctions() || vfd->numPicks(0) < 2)
   {
   SLErrorPop *errpop = new SLErrorPop(topWidget(), "Error", 
                "Velocities must first be picked or loaded from a file");
   return;
   }

 SLBase::manage();

 if(_first_time)
   {
   _tolerance = 1.0;
   _first_time = False;
   //_input_filename = _cmp_plot->SP()->filename();
   _text_box->SetValue(TOLERANCE, (float)_tolerance);
   }

 _input_filename = _cmp_plot->SP()->filename();
 _file_pair->maybeUpdate(_input_filename); 
 
}




//============================================================================
//====================== Reload defaults     =================================
//============================================================================
void VaNmcOutput::reloadDefaults(Boolean /*do_method*/)
{
  DoAction();
}


//============================================================================
//====================== Reload system defaults ==============================
//============================================================================
void VaNmcOutput::reloadSystemDefaults(Boolean /*do_method*/)
{
  DoAction();
}

//============================================================================
//== Apply the nmc and write out the trot file. This was converted from ======
//============================================================================
int VaNmcOutput::applyNMC()
{
int nhead,chk = 1;
int i,j,k,n;
int starting_trace;
float tmin, tmax;
float times2[MAXPICKS],velocities2[MAXPICKS];
long func_index;
long npicks;
int missed_location = 0;
int str_size, ok = True;
char temp_string[20];
long found_match;
Boolean have_a_match = False;
Boolean this_cmp_matches = False;
long num_groups; float firstgroup;
char msg[81];
int stat;
int do_write = 4;//Create new file with read and write priviledges
VfDataset *vfd = _vf_manager->activeDataset(); 
float dop = 999.0;
int lun, open_file = 1;
int user_aborted = 0;
SeisPlot *sp = _cmp_plot->SP();
long irregular_trace = 0;
int nhdrs;

  _times     = (float *) realloc(_times, MAXPICKS * 
                              (int)vfd->numVelocityFunctions()*sizeof(float));
  if(_times == NULL)
    {
    strcpy(_errmsg,"Not enough memory in VaNmcOutput applyNMC");
    cleanUp(NULL);
    return 0;
    }

  _velocities = (float *) realloc(_velocities, MAXPICKS * 
                           (int)vfd->numVelocityFunctions() * sizeof(float));
  if(_velocities == NULL)
    {
    strcpy(_errmsg,"Not enough memory in VaNmcOutput applyNMC");
    cleanUp(NULL);
    return 0;
    }

  
  // Read the globals from the glbl file, glbl is the tf globals structure
  _glbls = getglbl(&nhead,_input_filename, &ok);
  if(!ok)
    {
    strcpy(_errmsg,"Error in opening the globals file");
    cleanUp(NULL);
    return 0;
    }

   nhdrs = _glbls.nhdwd;

  //Data is regular
  if(!_irregular_cmp->IsSelected(ICMP))
    {
    //Find out how many traces per group we have
    _num_per_group = 0;
    for(i = 0; i < _glbls.ntrfil; i++)
      {
      _glbls_curtr = i + 1;
      if( (ok = getHeaders(_glbls_curtr, open_file)) == False )
        {
        strcpy(_errmsg,"Error in reading the input trot file");
        cleanUp(NULL);
        return 0;
        }
      open_file = 0;
      if(i == 0) firstgroup = _headers[2];
      if(firstgroup != _headers[2])//Found number of traces per group
        {
        i = _glbls.ntrfil;
        _num_per_group = _glbls_curtr - 1;
        }    
      }
    if(_num_per_group == 0)_num_per_group = _glbls_curtr;//Only one group
    num_groups = _glbls.ntrfil / _num_per_group;

    //Attempt to open for create the new file.
    tmax = (_Gout.ndptr - 1) * _Gout.srval + _Gout.tstrt;
    if((stat = tf_trcio_create_file_(_output_filename, "w+", 0, nhdrs, _Gout.ndptr,
                                      8, 64, _Gout.srval, _Gout.tstrt, tmax,
                                      _Gout.trmaxg, &_Gout.lun)) != 0 ) 
      {
      strcpy(_errmsg,"Error in opening the output file");
      cleanUp(&_G);
      return 0;
      }
    }
  else//Data is not regular
    {
    if(!strcmp(_input_filename, sp->filename()))//Use cmp plot info
      {
      _num_per_group = 0;
      num_groups     = _cmp_plot->numberLocationsInFile();
      if( (ok = getHeaders(1, open_file)) == False )
        {
        strcpy(_errmsg,"Error in reading the input trot file");
        return 0;
        }
      open_file = 0;

      //Attempt to open for create the new file.
      tmax = (_Gout.ndptr - 1) * _Gout.srval + _Gout.tstrt;
      if((stat = tf_trcio_create_file_(_output_filename, "w+", 0,nhdrs,_Gout.ndptr,
                                      8, 64, _Gout.srval, _Gout.tstrt, tmax,
                                      _Gout.trmaxg, &_Gout.lun)) != 0 ) 
        {
        strcpy(_errmsg,"Error in opening the output file");
        cleanUp(&_G);
        return 0;
        }
      }
    else//Not the same as cmp plot so we need to read irregular data
      {
      if(_xlocations == NULL)
        _xlocations = (float *) calloc(1, (int)(MAX_LOCS * sizeof(float)));
      else
        _xlocations = (float *) realloc(
                               _xlocations,(int)(MAX_LOCS *sizeof(float)));
      if(_xlocations == NULL)
        {
        strcpy(_errmsg,"Error in allocating memory for input trot file");
        cleanUp(NULL);
        return 0;
        }      
      if(_ylocations == NULL)
        _ylocations = (float *) calloc(1, (int)(MAX_LOCS * sizeof(float)));
      else
        _ylocations = (float *) realloc(
                               _ylocations,(int)(MAX_LOCS *sizeof(float)));
      if(_ylocations == NULL)
        {
        free(_xlocations); _xlocations = NULL;
        strcpy(_errmsg,"Error in allocating memory for input trot file");
        cleanUp(NULL);
        return 0;
        }

      sp = new SeisPlot(topWidget());
      stat= sp->findTracesByPattern(_input_filename, 
                                    vfd->getNhx(), vfd->getNhy(),
                                    _glbls.ntrfil, &num_groups, 
                                    &user_aborted, _xlocations, 
                                    _ylocations);
      if(!stat) 
        {
        strcpy(_errmsg,"Error reading input trot file");
        cleanUp(NULL);
        return 0;
        }

      if( (ok = getHeaders(1, 1)) == False )
        {
        strcpy(_errmsg,"Error in reading the input trot file");
        cleanUp(NULL);
        return 0;
        }

      //Attempt to open for create the new file.
      tmax = (_Gout.ndptr - 1) * _Gout.srval + _Gout.tstrt;
      if((stat = tf_trcio_create_file_(_output_filename, "w+", 0,nhdrs,_Gout.ndptr,
                                      8, 64, _Gout.srval, _Gout.tstrt, tmax,
                                      _Gout.trmaxg, &_Gout.lun)) != 0 ) 
        {
        strcpy(_errmsg,"Error in opening the output file");
        cleanUp(&_G);
        return 0;
        }

      _num_per_group = 0;

      }
    }



//Loop thru the groups and match to the velocity file
  for(i = 0; i < num_groups; i++)
    {
    this_cmp_matches = False;
    if(_irregular_cmp->IsSelected(ICMP))
      {
      irregular_trace += _num_per_group;
      _num_per_group = sp->getSelectorNumTraces(i);
      _glbls_curtr = irregular_trace + 1;//Set trace to read
      }
    else
      {
      _glbls_curtr = i * _num_per_group + 1;//Set trace to read
      }
    if( (ok = getHeaders(_glbls_curtr, open_file)) == False )
      {
      strcpy(_errmsg,"Error in reading the input trot file");
      cleanUp(&_G);
      cleanUp(&_Gout);
      return 0;
      }
    stat = getVelocityLocations();
    if(!stat)
      {
      strcpy(_errmsg,"Not enough memory in VaNmc Output getVelocityLocations");
      cleanUp(&_G);
      cleanUp(&_Gout);
      return 0;
      }

    for(j = 0; j < vfd->numVelocityFunctions(); j++)
      {
      found_match = _cmp_plot->findArrayMatch(_vfd_xlocs[j], _tolerance,
                                              _vfd_ylocs[j], _tolerance,
                                              &_headers[vfd->getNhx()-1],
                                              &_headers[vfd->getNhy()-1],
                                              1,1,True); 
      if( found_match >= 0)
        {
        func_index = vfd->findMatchingVelfun(_vfd_xlocs[j],_vfd_ylocs[j]);
        vfd->getAbscissaArray(func_index, _times, VTNM);
        vfd->getOrdinateArray(func_index, _velocities, VTNM);
        npicks = (int)vfd->numPicks(func_index);
        have_a_match = True;
        this_cmp_matches = True;
        for(k = 0; k < npicks;++k) 
          {
          times2[k]       = _times[k];
          velocities2[k]  = _velocities[k];
          }
        starting_trace = _glbls_curtr;
        _Cl.ntot = _Cl.ndo = _num_per_group;
        if( (ok = reallocateArrays(_num_per_group)) == False )
          {
          cleanUp(&_G);
          cleanUp(&_Gout);
          return 0;
          }
        if( (ok = getData(starting_trace, open_file)) == False )
          {
          strcpy(_errmsg,"Error in reading the input trot file");
          cleanUp(&_G);
          cleanUp(&_Gout);
          return 0;
          }
        stat = _vf_moveout->doMoveout(vfd, func_index, FORWARD_NMC, dop,
                        &_floats[0], &_headers[0], 
                        _glbls.ndptr, nhead,  _num_per_group,
                        _glbls.tstrt,_glbls.srval);
        if(stat)//nmc failed
          {
          strcpy(_errmsg,"Error in applying the nmc\n");
          cleanUp(&_G);
          cleanUp(&_Gout);
          return 0;
          }

        for(int n = 0; n < _num_per_group; n++)
          {
          if((stat = tf_trcio_write_trace_(_Gout.lun, &_headers[n * nhdrs],
                                           &_floats[n * _Gout.ndptr],
                                           nhdrs, _Gout.ndptr )) != 0)
            {
            strcpy(_errmsg,"Error in writing the output trot file");
            cleanUp(&_G);
            cleanUp(&_Gout); 
            return 0;
            }
          }

        j = (int)vfd->numVelocityFunctions();//stop loop and get next cmp
        }
      }//End for numfuncs



    if(this_cmp_matches == False)//Register missing locations 
      {  
      ++missed_location;
      _missing_locations = (float *) realloc(_missing_locations, 
                                             missed_location * sizeof(float));
      if(_missing_locations == NULL)
        {
        strcpy(_errmsg,"Couldnt allocate enough memory"); 
        cleanUp(&_G);
        cleanUp(&_Gout);
        return 0;
        }
      _missing_locations[missed_location - 1] = _headers[vfd->getNhx()-1];
      //Write out this gather without nmc
      starting_trace = _glbls_curtr;
      _Cl.ntot = _Cl.ndo = _num_per_group;
      if( (ok = reallocateArrays(_num_per_group)) == False )
        {
        cleanUp(&_G);
        cleanUp(&_Gout);
        return 0;
        }
      if( (ok = getData(starting_trace, open_file)) == False )
        {
        strcpy(_errmsg,"Error in reading the input trot file");
        cleanUp(&_G);
        cleanUp(&_Gout);
        return 0;
        }

      for(int n = 0; n < _num_per_group; n++)
          {
          if((stat = tf_trcio_write_trace_(_Gout.lun, &_headers[n * nhdrs],
                                           &_floats[n * _Gout.ndptr],
                                           nhdrs, _Gout.ndptr )) != 0)
            {
            strcpy(_errmsg,"Error in writing the output trot file");
            cleanUp(&_G);
            cleanUp(&_Gout);
            return 0;
            }
          }

      }
    }//End for num_groups (done with matching, writing etc.


   //The next is to be implemented when and if we want to write history, etx
   chk=writeGlbl(_glbls,_iso_plot->getFilename());
   if(!chk)
     {
     strcpy(_errmsg,"Error in writing the new global file");
     return 0;
     }


//Done with searching and writing data. See if we have matches
  if(!have_a_match)
    {
    strcpy(_errmsg,"Could not find a match of velocities to trot file");
    return(False);
    }

//Pop up message listing the missed locations
  if(missed_location)
    {
    str_size = missed_location * 12 * sizeof(char) + 
               strlen(MIS_MATCH) * sizeof(char);
    _missing_location_string=(char *)realloc(_missing_location_string,str_size);
    if(_missing_location_string == NULL)
      {
      strcpy(_errmsg,"Couldnt allocate enough memory"); 
      return 0;
      }
    strcpy(_missing_location_string,MIS_MATCH);
     
    for(i = 0; i < missed_location; i++)
      {
      sprintf(temp_string,"%8.2f\n",_missing_locations[i]);
      strcat(_missing_location_string,temp_string);
      }
    SLErrorPop *errpop = new SLErrorPop(topWidget(), "Warning", 
                                        _missing_location_string);
    }


  cleanUp(&_G);
  cleanUp(&_Gout);
  cleanUp(NULL);

  return True;
}


//============================================================================
//========== Get header info from a global file. =============================
//============================================================================
GLBL VaNmcOutput::getglbl(int *nhead,char *name, int *ok)
{
char *tmpname;
int chk, stat;
GLBL glbl;


  stat = get_global_data_(name, &glbl, &chk);

  if(stat != 0)
    {
    *ok = False;
    return glbl;
    }

  *ok = True;

  //*nhead = glbl.nhdwd;       
  *nhead = PlotImage::MAXHDRS; //For our current purposes

  if(_floats) 
    {
    free(_floats);
    _floats = NULL;
    }

  if( (_floats=(float *)
      malloc(((glbl.ndptr+1) * 2)*sizeof(float))) == NULL)
    {
    strcpy(_errmsg,"Not enough memory to read the global file");
    ok = False;
    return glbl;
    }

  if(strcmp(glbl.ftyp,"CBYTE") == 0 )//old byte type
    {
    if(_bytes) 
      {
      free(_bytes);
      _bytes = NULL;
      }
    if( (_bytes=(unsigned char *)
              malloc(((glbl.ndptr+1) * 2)* (2*sizeof(float)))) == NULL)
      {
      strcpy(_errmsg,"Not enough memory to read the global file");
      ok = False;
      return glbl;
      }
    }


  if(_tasf) free(_tasf);
  if( (_tasf=(float *) malloc(2 * sizeof(float))) == NULL)
    {
    strcpy(_errmsg,"Not enough memory to read the global file");
    ok = False;
    return glbl;
    }  

  return glbl;
}


//=========================================================================== 
//========================= Get headers from input file =====================
//===========================================================================
int VaNmcOutput::getHeaders(int trace, int open_file)
{
int istat;
int jreadonly = 0;
char msg[81];
int nhdrs;
int normalize = 0;
int num_traces = 1;
int ok, cstat;

  

  if(open_file)
    {
    get_global_data_(_input_filename, &_G, &istat);
    if( istat > 0) return False;
    if(_G.lun < -1) _G.lun = -_G.lun;
    }

  _Cl.ntot = 1;
  _Cl.ndo = 1;
  _Cl.axis = 0;
  _Cl.index = 0;
  _Cl.samp1 = _Cl.sdec  = 1;
  _Cl.nsamp = 1;//Interested in headers only
  _Cl.iskp  = trace - 1;
  _Cl.nskp  = _Cl.trnsps = _Cl.cnvrt = 0;

  if( (ok = reallocateArrays(num_traces)) == False ) return False;


  if(strcmp(_G.ftyp,"CBYTE") == 0 )//old byte type
    {
    istat = read_byt_(_input_filename, &istat, &_Cl, &normalize, &nhdrs,
                     (char *)_headers, _bytes, _tasf);
    if(istat != 0)
      {
      tf_glbl_getn_( &istat,  _input_filename, &_G); 
      tf_close_(&_G.lun, &istat, msg);
      return False;
      }
    }
  else
    {

    istat = read_data_(_input_filename, &istat, &_Cl,
                     (char *)_headers, (char *)_floats, _tasf, 
                      &_G.lun, open_file);
    if(istat != 0)
      {
      tf_trcio_close_file_(_G.lun, &istat);
      return False;
      }
    }



 //Transfer some of the input global information to the output globals
    _Gout.ntrfil = _G.ntrfil;
    _Gout.nbycll = _G.nbycll;
    _Gout.ntrcll = _G.ntrcll;
    _Gout.grecsiz= _G.grecsiz;
    _Gout.ntb    = _G.ntb;	
    _Gout.numhc  = _G.numhc;	
    _Gout.nbydp  = _G.nbydp;	
    _Gout.nbyhd  = _G.nbyhd;	
    _Gout.hdtyp  = _G.hdtyp;	
    _Gout.wdtyp  = _G.wdtyp;	
    _Gout.nhdwd  = _G.nhdwd;	
    _Gout.ndptr  = _G.ndptr;	
    _Gout.srval  = _G.srval;	
    _Gout.tstrt  = _G.tstrt;	
    _Gout.xorg   = _G.xorg;	
    _Gout.yorg   = _G.yorg;	
    _Gout.dx0[0] = _G.dx0[0];
    _Gout.dx0[1] = _G.dx0[1];
    _Gout.dx0[2] = _G.dx0[2];
    _Gout.dx0[3] = _G.dx0[3];
    _Gout.dn0[0] = _G.dn0[0];
    _Gout.dn0[1] = _G.dn0[1];
    _Gout.dn0[2] = _G.dn0[2];
    _Gout.dn0[3] = _G.dn0[3];
    _Gout.trmaxg = _G.trmaxg;
    _Gout.dunits = _G.dunits;
    strcpy(_Gout.ftyp,_G.ftyp);
    strcpy(_Gout.srun,_G.srun);
    _Gout.h = _G.h;  
   
  return True;
}


//=========================================================================== 
//================ Get velocity objects x and y locations   =================
//===========================================================================
Boolean VaNmcOutput::getVelocityLocations()
{
VfDataset *vfd = _vf_manager->activeDataset(); 
long i;


  if(_vfd_xlocs == NULL)
     _vfd_xlocs = (float *) malloc((int)(vfd->numVelocityFunctions() 
                                                  * sizeof(float)));
  else
    _vfd_xlocs = (float *)realloc(_vfd_xlocs, (int)(vfd->numVelocityFunctions() 
                                                  * sizeof(float)));
  if(_vfd_xlocs == NULL) return False;

  if(_vfd_ylocs == NULL)
     _vfd_ylocs = (float *) malloc((int)(vfd->numVelocityFunctions() 
                                                  * sizeof(float)));
  else
    _vfd_ylocs = (float *)realloc(_vfd_ylocs, (int)(vfd->numVelocityFunctions() 
                                                  * sizeof(float)));
  if(_vfd_ylocs == NULL) return False;


  for(i = 0; i < vfd->numVelocityFunctions(); i++)
    {
    _vfd_xlocs[i] = vfd->getXloc(i);
    _vfd_ylocs[i] = vfd->getYloc(i);
    }

  return True;

}
//=========================================================================== 
//================ Get header and trot data from input file =================
//===========================================================================
int VaNmcOutput::getData(int trace, int open_file)
{
int ireadonly = 0;
char msg[81];
int dont_delete = 0;
int istat;
int normalize = 0;
int nhdrs;
int num_vals, cstat;



  if(open_file)
    {
    get_global_data_(_input_filename, &_G, &istat);
    if( istat > 0) return False;
    if(_G.lun < 0) _G.lun = -_G.lun;
    }


  _Cl.iskp  = trace - 1;
  _Cl.nsamp = _G.ndptr;

  num_vals = _Cl.ntot * _Cl.nsamp;
  if(strcmp(_G.ftyp,"CBYTE") == 0 )//old byte type
    {
    istat = read_byt_(_input_filename, &istat, &_Cl, &normalize, &nhdrs,
                     (char *)_headers, _bytes, _tasf);
    if(istat == 0)
      {
      byte_to_float_(_bytes, &num_vals, &num_vals, &_G.trmaxg, _floats); 
      }
    else
      {
      tf_glbl_getn_( &istat,  _input_filename, &_G); 
      tf_close_(&_G.lun, &istat, msg);
      return False;
      }
    }
  else
    {
    istat = read_data_(_input_filename, &istat, &_Cl,
                       (char *)_headers, (char *)_floats, _tasf, &_G.lun,
                       open_file);
    if(istat != 0)
      {
      tf_trcio_close_file_(_G.lun, &istat);
      return False;
      }
    }


  return True;
}


//============================================================================
//========== Write the new globals               =============================
//============================================================================
int VaNmcOutput::writeGlbl(GLBL glbl, char *velfname)
{
int stat = 1;

//printf("Add history info here\n");
  return stat;
}

//============================================================================
//========== Re-allocate arrays                 ==============================
//============================================================================
int VaNmcOutput::reallocateArrays(int num_traces)
{


  _floats = (float *) realloc(_floats, (num_traces * ((_glbls.ndptr+1) * 2))
                              * sizeof(float)); 
  if(_floats == NULL)
      {
      strcpy(_errmsg,"Not enough memory for data arrays");
      return False;
      }




  if(strcmp(_G.ftyp,"CBYTE") == 0 )//old byte type
    {
    _bytes = (unsigned char *) 
               realloc(_bytes, (num_traces * ((_glbls.ndptr+1) * 2))
                              * (2*sizeof(float)));
     if(_bytes == NULL)
        {
        free(_floats); _floats = NULL;
        strcpy(_errmsg,"Not enough memory for data arrays");
        return False;
        }
    }

  int headersize = max( (_G.nhdwd * sizeof(double)) ,
                        (PlotImage::MAXHDRS*(sizeof(double))) ) * num_traces; 
  _headers =  (float *) realloc(_headers, headersize);
  if(_headers == NULL)
    {
    free(_floats); _floats = NULL;
    if(_bytes) {free(_bytes); _bytes = NULL;}
    strcpy(_errmsg,"Not enough memory for data arrays");
    return False;
    }

  _tasf =  (float *) realloc(_tasf, num_traces * 2 * sizeof(float));
  if(_tasf == NULL)
    {
    free(_headers); _headers = NULL;
    if(_bytes) {free(_bytes); _bytes = NULL;}
    strcpy(_errmsg,"Not enough memory for data arrays");
    return False;
    }
  return True;
}

//============================================================================
//========== Finish up and free resources        =============================
//============================================================================
void VaNmcOutput::cleanUp(GLBL *glbl)
{
int stat;
char msg[81];

  if(_missing_locations)
    {
    free (_missing_locations); 
    _missing_locations = NULL;
    }
  if(_missing_location_string)
    {
    free (_missing_location_string); 
    _missing_location_string = NULL;
    }
  if(_floats)
    {
    free (_floats);
    _floats = NULL;
    }
  if(_bytes)
    {
    free (_bytes);
    _bytes = NULL;
    }
  if(_headers)
    {
    free (_headers);
    _headers = NULL;
    }
  if(_tasf)
    {
    free (_tasf);
    _tasf = NULL;
    }
  if(_times)
    {
    _times      = (float *) realloc(_times, sizeof(float));
    }
  if(_velocities)
    {
    _velocities = (float *) realloc(_velocities,sizeof(float));
    }

  if(glbl != NULL)
    {
    if(strcmp(glbl->ftyp,"CBYTE") == 0 )//old byte type
      tf_glbl_getn_( &stat,  glbl->path, glbl); 
    tf_trcio_close_file_(glbl->lun, &stat);
    }


 
}



