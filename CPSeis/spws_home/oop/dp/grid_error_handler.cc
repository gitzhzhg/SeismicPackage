// grid_error_handler.cc:  A class to handle grid errors
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
#include "dp/grid_error_handler.hh"

static char _error_string[72];

char *gridErrorMessage (GridErrorCodes error_status)
{
  switch (error_status) {


    case FG_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case FG_MEMORY_ALLOCATION_ERROR:
      strcpy (_error_string, "Could not allocate grid memory");
      break;
    case FG_SIZE_TOO_SMALL:
      strcpy (_error_string, "Grid size too small");
      break;
    case FG_SET_RANGE_ERROR:
      strcpy (_error_string, "Grid range data is set invalidly");
      break;
    case FG_INVALID_GRID:
      strcpy (_error_string, "Grid is either NULL or undefined");
      break;
    case FG_INVALID_SUB_REGION:
      strcpy (_error_string, "Grid subregion is wrong");
      break;
    case FG_INVALID_VALUE:
      strcpy (_error_string, "Input value is not defined");
      break;
    case FG_ZERO_DENOMINATOR:
      strcpy (_error_string, "Grid had a divide by zero condition");
      break;
    case FG_USER_ABORTED:
      strcpy (_error_string, "Grid operation was aborted by user");
      break;

    
    case GF_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case GF_MEMORY_ALLOCATION_ERROR:
      strcpy (_error_string, "Could not allocate gridder memory");
      break;
    case GF_BAD_REACH_VALUES:
      strcpy (_error_string, "Gridder reach values are wrong");
      break;
    case GF_BAD_MAX_HITS_VALUE:
      strcpy (_error_string, "Gridder maximum hits value is wrong");
      break;
    case GF_BAD_INPUTS:
      strcpy (_error_string, "Gridder input(s) is(are) NULL");
      break;


    case AGF_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case AGF_MEMORY_ALLOCATION_ERROR:
      strcpy (_error_string, "Could not allocate auto-gridder memory");
      break;
    case AGF_BAD_INPUTS:
      strcpy (_error_string, "Auto-gridder input(s) is(are) NULL");
      break;
    case AGF_ZERO_DENOMINATOR:
      strcpy (_error_string, "Auto-gridder found divide by zero condition");
      break;


    case CP_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case CP_MEMORY_ALLOCATION_ERROR:
      strcpy (_error_string, "Could not allocate control point memory");
      break;
    case CP_BAD_INPUTS:
      strcpy (_error_string, "Control point input(s) is(are) NULL");
      break;


    case UCG_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case UCG_MEMORY_ALLOCATION_ERROR:
      strcpy (_error_string, "Could not allocate byte grid memory");
      break;
    case UCG_SIZE_TOO_SMALL:
      strcpy (_error_string, "Byte grid size too small");
      break;
    case UCG_SET_RANGE_ERROR:
      strcpy (_error_string, "Byte grid range data is set invalidly");
      break;
    case UCG_INVALID_GRID:
      strcpy (_error_string, "Byte grid is either NULL or undefined");
      break;
    case UCG_INVALID_SUB_REGION:
      strcpy (_error_string, "Byte grid subregion is wrong");
      break;
    case UCG_UNDEFINED_ARITHMETIC:
      strcpy (_error_string, "Byte grid arithmetic operation undefined");
      break;
    case UCG_USER_ABORTED:
      strcpy (_error_string, "Byte grid operation was aborted by user");
      break;


    case UCA_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case UCA_BAD_INPUTS:
      strcpy (_error_string, "Byte-array accessor input(s) is(are) NULL");
      break;
    case UCA_INITIALIZATION_ERROR:
      strcpy (_error_string, "Error initializing byte-array accessor");
      break;


    case DPB_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case DPB_MEMORY_ALLOCATION_ERROR:
      strcpy (_error_string, "Could not allocate display processor memory");
      break;
    case DPB_BAD_INPUTS:
      strcpy (_error_string, "Display processor input(s) is(are) NULL");
      break;
    case DPB_INITIALIZATION_ERROR:
      strcpy (_error_string, "Error initializing display processor");
      break;
    case DPB_UNDEFINED_ARITHMETIC:
      strcpy (_error_string,
        "Display processor arithmetic operation undefined");
      break;
    case DPB_COMPRESSION_ERROR:
      strcpy (_error_string,
        "Error compressing display processor information");
      break;
    case DPB_USER_ABORTED:
      strcpy (_error_string, "Display processor task was aborted by user");
      break;


    case RGB_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case RGB_MEMORY_ALLOCATION_ERROR:
      strcpy (_error_string,
        "Could not allocate RGBZ-generator memory");
      break;
    case RGB_BAD_INPUTS:
      strcpy (_error_string,
        "RGBZ-generator input(s) is(are) NULL");
      break;


    case FCGP_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case FCGP_MEMORY_ALLOCATION_ERROR:
      strcpy (_error_string,
        "Could not allocate FgQc Compute Grid popup memory");
      break;


    case FCG_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case FCG_MEMORY_ALLOCATION_ERROR:
      strcpy (_error_string,
        "Could not allocate FgQc Compute Grid memory");
      break;
    case FCG_PLOTTING_ERROR:
      strcpy (_error_string,
        "FgQc Compute Grid plotting error");
      break;


    case CGP_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;


    case CG_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case CG_MEMORY_ALLOCATION_ERROR:
      strcpy (_error_string,
        "Could not allocate Compute Grid memory");
      break;
    case CG_AMPLITUDE_SCALE_ERROR:
      strcpy (_error_string,
        "Compute Grid amplitude scaling error");
      break;


    case FA_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case FA_BAD_INPUTS:
      strcpy (_error_string,
        "Float Grid Accessor input(s) is(are) invalid");
      break;


    case CL_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case CL_MEMORY_ALLOCATION_ERROR:
      strcpy (_error_string,
        "Could not allocate Color Look-Up-Table memory");
      break;
    case CL_BAD_INPUTS:
      strcpy (_error_string,
        "Color Look-Up-Table input(s) is(are) invalid");
      break;


    case BFA_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case BFA_MEMORY_ALLOCATION_ERROR:
      strcpy (_error_string,
        "Could not allocate Base Float Array memory");
      break;
    case BFA_BAD_INPUTS:
      strcpy (_error_string,
        "Base Float Array input(s) is(are) invalid");
      break;
    case BFA_SET_RANGE_ERROR:
      strcpy (_error_string, "Base Float Array range data is set invalidly");
      break;
    case BFA_INVALID_ARRAY:
      strcpy (_error_string, "Base Float Array is either NULL or undefined");
      break;


    case OL_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case OL_BAD_INPUTS:
      strcpy (_error_string,
        "Output Look-Up-Table input(s) is(are) invalid");
      break;


    case IL_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case IL_MEMORY_ALLOCATION_ERROR:
      strcpy (_error_string,
        "Could not allocate Base Input Look-Up-Table memory");
      break;
    case IL_BAD_INPUTS:
      strcpy (_error_string,
        "Input Look-Up-Table input(s) is(are) invalid");
      break;
    case IL_INITIALIZATION_ERROR:
      strcpy (_error_string, "Error initializing Input Look-Up-Table");
      break;


    case D_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case D_MEMORY_ALLOCATION_ERROR:
      strcpy (_error_string,
        "Could not allocate Decoder memory");
      break;
    case D_BAD_INPUTS:
      strcpy (_error_string,
        "Decoder input(s) is(are) invalid");
      break;


    case MA_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case MA_MEMORY_ALLOCATION_ERROR:
      strcpy (_error_string,
        "Could not allocate Multi-Attributes memory");
      break;
    case MA_BAD_INPUTS:
      strcpy (_error_string,
        "Multi-Attributes input(s) is(are) invalid");
      break;
    case MA_COMPRESSION_ERROR:
      strcpy (_error_string, "Error compressing Multi-Attributes");
      break;
    case MA_USER_ABORTED:
      strcpy (_error_string, "Multi-Attributes operation was aborted by user");
      break;


    case FU_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case FU_INVALID_SUB_REGION:
      strcpy (_error_string,
        "Specified subregion is invalid converting Float to Unsigned Char");
      break;


    case AR_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case AR_BAD_INPUTS:
      strcpy (_error_string,
        "Array remapper input is invalid");
      break;


    case RGBZP_SUCCESSFUL:
      strcpy (_error_string, " ");
      break;
    case RGBZP_MEMORY_ALLOCATION_ERROR:
      strcpy (_error_string,
        "Could not allocate color popup memory");
      break;
    case RGBZP_BAD_INPUTS:
      strcpy (_error_string,
        "Color popup input is invalid");
      break;


    default:
      strcpy (_error_string, " ");  // default is successful
      break;
  }
  return _error_string;
}

GridErrorHandler::GridErrorHandler (SLDelay *slparent, char *name,
  GridErrorCodes error_status):
  SLErrorPop (slparent, name, (const char *)gridErrorMessage (error_status))
{
}

GridErrorHandler::GridErrorHandler (Widget wparent, char *name,
  GridErrorCodes error_status):
  SLErrorPop (wparent, name, (const char *)gridErrorMessage (error_status))
{
}

GridErrorHandler::~GridErrorHandler ()
{
}
