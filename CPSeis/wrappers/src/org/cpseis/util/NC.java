package org.cpseis.util;

/**
<pre>
             Java equivalent to the named constants primitive.
--------------------------------------------------------------------------------
                                 NC.java
--------------------------------------------------------------------------------
This java Class is equivalent to the named_constants.h and the
named_constants.f90 files.  See one of these files for details.

The trace flow constants are these:  NO_MORE_TRACES  FATAL_ERROR  NEED_TRACES

Nil values are these:                INIL  LNIL  FNIL  DNIL  BNIL(false)  CNIL("")

Header word numbers:                 HDR_SEQUENCE(1) thru HDR_BOTTOM_MUTE(64)
--------------------------------------------------------------------------------
                         RECENT REVISION HISTORY              

     Date        Author       Description
     ----        ------       -----------
  1. 2006-04-11  Stoeckley    Initial version.
--------------------------------------------------------------------------------
</pre>
*/

//---------------------------- start of class ----------------------------//
//---------------------------- start of class ----------------------------//
//---------------------------- start of class ----------------------------//

public class NC
{

//-------------------------- trace flow constants ------------------------//
//-------------------------- trace flow constants ------------------------//
//-------------------------- trace flow constants ------------------------//

public static final int NO_MORE_TRACES =  0;
public static final int FATAL_ERROR    = -1;
public static final int NEED_TRACES    = -2;

//------------------------------ nil values -----------------------------//
//------------------------------ nil values -----------------------------//
//------------------------------ nil values -----------------------------//

// Note that LNIL is a long in NC.java, whereas LNIL is a logical
// in named_constants.h and named_constants.f90.

public static final int      INIL  = -888728;
public static final long     LNIL  = -888728;
public static final float    FNIL  = -1.0e-30f;
public static final double   DNIL  = -1.0e-30;
public static final String   CNIL  = "";
public static final boolean  BNIL  = false;


//------------------- trace header word numbers -------------------------//
//------------------- trace header word numbers -------------------------//
//------------------- trace header word numbers -------------------------//

/**
The nominal number of CPS trace header words.
The actual number is at least this large and may be larger.
*/
public static final int HDR_NOMINAL_SIZE       = 64;
 
                // these are also fortran-style indices //

public static final int HDR_SEQUENCE           =  1;
public static final int HDR_TOP_MUTE           =  2;
public static final int HDR_CURRENT_GROUP      =  3;
public static final int HDR_CURRENT_CHANNEL    =  4;
public static final int HDR_FOLD               =  5;
public static final int HDR_OFFSET             =  6;
public static final int HDR_MIDPOINT_XGRID     =  7;
public static final int HDR_MIDPOINT_YGRID     =  8;
public static final int HDR_ORIGINAL_GROUP     =  9;
public static final int HDR_ORIGINAL_CHANNEL   = 10;
public static final int HDR_SOURCE_XLOC        = 11;
public static final int HDR_SOURCE_YLOC        = 12;
public static final int HDR_SOURCE_ELEV        = 13;
public static final int HDR_RECEIVER_XLOC      = 14;
public static final int HDR_RECEIVER_YLOC      = 15;
public static final int HDR_RECEIVER_ELEV      = 16;
public static final int HDR_MIDPOINT_XLOC      = 17;
public static final int HDR_MIDPOINT_YLOC      = 18;
public static final int HDR_MIDPOINT_ELEV      = 19;
public static final int HDR_SOURCE_DEPTH       = 20;
public static final int HDR_RECEIVER_DEPTH     = 21;
public static final int HDR_SOURCE_COMPONENT   = 22;
public static final int HDR_RECEIVER_COMPONENT = 23;
public static final int HDR_PANEL              = 24;
public static final int HDR_LAV                = 25;
public static final int HDR_SOURCE_LINE        = 26;
public static final int HDR_RECEIVER_LINE      = 27;
public static final int HDR_RECEIVER_SHOTPOINT = 28;
public static final int HDR_SOURCE_SHOTPOINT   = 29;
public static final int HDR_SCRATCH_30         = 30;
public static final int HDR_SCRATCH_31         = 31;
public static final int HDR_SCRATCH_32         = 32;
public static final int HDR_SOURCE_XGRID       = 33;
public static final int HDR_SOURCE_YGRID       = 34;
public static final int HDR_RECEIVER_XGRID     = 35;
public static final int HDR_RECEIVER_YGRID     = 36;
public static final int HDR_MIDPOINT_SHOTPOINT = 37;
public static final int HDR_MIDPOINT_LINE      = 38;
public static final int HDR_PRE                = 39;
public static final int HDR_POST               = 40;
public static final int HDR_CUM_DATUM_STATIC   = 41;
public static final int HDR_CUM_REFR_STATIC    = 42;
public static final int HDR_CUM_RESID_STATIC   = 43;
public static final int HDR_SOURCE_UPTIME      = 44;
public static final int HDR_RECEIVER_UPTIME    = 45;
public static final int HDR_SOURCE_GP          = 46;
public static final int HDR_RECEIVER_GP        = 47;
public static final int HDR_USER_48            = 48;
public static final int HDR_USER_49            = 49;
public static final int HDR_USER_50            = 50;
public static final int HDR_USER_51            = 51;
public static final int HDR_USER_52            = 52;
public static final int HDR_USER_53            = 53;
public static final int HDR_USER_54            = 54;
public static final int HDR_USER_55            = 55;
public static final int HDR_RPRE               = 56;
public static final int HDR_RPOST              = 57;
public static final int HDR_SCRATCH_58         = 58;
public static final int HDR_SCRATCH_59         = 59;
public static final int HDR_SCRATCH_60         = 60;
public static final int HDR_SCRATCH_61         = 61;
public static final int HDR_SCRATCH_62         = 62;
public static final int HDR_GVS_MODIFIER       = 63;
public static final int HDR_BOTTOM_MUTE        = 64;

//------------------- trace header word indices -------------------------//
//------------------- trace header word indices -------------------------//
//------------------- trace header word indices -------------------------//

             // these are C-style and java-style indices //

public static final int CPS_SEQUENCE           =  0;
public static final int CPS_TOP_MUTE           =  1;
public static final int CPS_CURRENT_GROUP      =  2;
public static final int CPS_CURRENT_CHANNEL    =  3;
public static final int CPS_FOLD               =  4;
public static final int CPS_OFFSET             =  5;
public static final int CPS_MIDPOINT_XGRID     =  6;
public static final int CPS_MIDPOINT_YGRID     =  7;
public static final int CPS_ORIGINAL_GROUP     =  8;
public static final int CPS_ORIGINAL_CHANNEL   =  9;
public static final int CPS_SOURCE_XLOC        = 10;
public static final int CPS_SOURCE_YLOC        = 11;
public static final int CPS_SOURCE_ELEV        = 12;
public static final int CPS_RECEIVER_XLOC      = 13;
public static final int CPS_RECEIVER_YLOC      = 14;
public static final int CPS_RECEIVER_ELEV      = 15;
public static final int CPS_MIDPOINT_XLOC      = 16;
public static final int CPS_MIDPOINT_YLOC      = 17;
public static final int CPS_MIDPOINT_ELEV      = 18;
public static final int CPS_SOURCE_DEPTH       = 19;
public static final int CPS_RECEIVER_DEPTH     = 20;
public static final int CPS_SOURCE_COMPONENT   = 21;
public static final int CPS_RECEIVER_COMPONENT = 22;
public static final int CPS_PANEL              = 23;
public static final int CPS_LAV                = 24;
public static final int CPS_SOURCE_LINE        = 25;
public static final int CPS_RECEIVER_LINE      = 26;
public static final int CPS_RECEIVER_SHOTPOINT = 27;
public static final int CPS_SOURCE_SHOTPOINT   = 28;
public static final int CPS_SCRATCH_30         = 29;
public static final int CPS_SCRATCH_31         = 30;
public static final int CPS_SCRATCH_32         = 31;
public static final int CPS_SOURCE_XGRID       = 32;
public static final int CPS_SOURCE_YGRID       = 33;
public static final int CPS_RECEIVER_XGRID     = 34;
public static final int CPS_RECEIVER_YGRID     = 35;
public static final int CPS_MIDPOINT_SHOTPOINT = 36;
public static final int CPS_MIDPOINT_LINE      = 37;
public static final int CPS_PRE                = 38;
public static final int CPS_POST               = 39;
public static final int CPS_CUM_DATUM_STATIC   = 40;
public static final int CPS_CUM_REFR_STATIC    = 41;
public static final int CPS_CUM_RESID_STATIC   = 42;
public static final int CPS_SOURCE_UPTIME      = 43;
public static final int CPS_RECEIVER_UPTIME    = 44;
public static final int CPS_SOURCE_GP          = 45;
public static final int CPS_RECEIVER_GP        = 46;
public static final int CPS_USER_48            = 47;
public static final int CPS_USER_49            = 48;
public static final int CPS_USER_50            = 49;
public static final int CPS_USER_51            = 50;
public static final int CPS_USER_52            = 51;
public static final int CPS_USER_53            = 52;
public static final int CPS_USER_54            = 53;
public static final int CPS_USER_55            = 54;
public static final int CPS_RPRE               = 55;
public static final int CPS_RPOST              = 56;
public static final int CPS_SCRATCH_58         = 57;
public static final int CPS_SCRATCH_59         = 58;
public static final int CPS_SCRATCH_60         = 59;
public static final int CPS_SCRATCH_61         = 60;
public static final int CPS_SCRATCH_62         = 61;
public static final int CPS_GVS_MODIFIER       = 62;
public static final int CPS_BOTTOM_MUTE        = 63;

//----------------------- mathematical constants -------------------------//
//----------------------- mathematical constants -------------------------//
//----------------------- mathematical constants -------------------------//

public static final double PI                 =  3.1415926535898;
public static final double RADIANS_PER_DEGREE =  0.01745329251994333300;
public static final double DEGREES_PER_RADIAN = 57.29577951308219500000;

/* equivalent:
public static final double PI                 = 3.1415926535898;
public static final double RADIANS_PER_DEGREE = PI / 180.0;
public static final double DEGREES_PER_RADIAN = 180.0 / PI;
*/

//------------------------- physical constants ---------------------------//
//------------------------- physical constants ---------------------------//
//------------------------- physical constants ---------------------------//


//----------------------- operating system flags -------------------------//
//----------------------- operating system flags -------------------------//
//----------------------- operating system flags -------------------------//


//---------------------------- debug flags -------------------------------//
//---------------------------- debug flags -------------------------------//
//---------------------------- debug flags -------------------------------//


//------------------------ get header word description ------------------//
//------------------------ get header word description ------------------//
//------------------------ get header word description ------------------//

/**
Returns a description of the CPS header word with the specified index.
The index must be specified with 0 as the first header word.
Asserts if the index is less than 0.

@param  indx    index of the desired CPS header word (>= 0).
@return         description of the CPS header word.
*/
public static String getHeaderWordDescription(int indx)
{
  assert(indx >= 0);
  if(indx < DESCRIPTIONS.length) return DESCRIPTIONS[indx];
  return (" " + (indx+1) + "   user defined");
}

//-------------------------- header word descriptions ----------------------//
//-------------------------- header word descriptions ----------------------//
//-------------------------- header word descriptions ----------------------//

public static final String[] DESCRIPTIONS =
   {
   "  1   trace sequence number",
   "  2   head mute index",
   "  3   current gather number",
   "  4   current channel number",
   "  5   fold of stack",
   "  6   offset",
   "  7   CMP X grid coordinate",
   "  8   CMP Y grid coordinate",
   "  9   original shot profile number",
   " 10   original channel number",
   " 11   source surveyed X coordinate",
   " 12   source surveyed Y coordinate",
   " 13   source elevation",
   " 14   receiver surveyed X coordinate",
   " 15   receiver surveyed Y coordinate",
   " 16   receiver elevation",
   " 17   CMP surveyed X coordinate",
   " 18   CMP surveyed Y coordinate",
   " 19   CMP elevation",
   " 20   source hole depth",
   " 21   receiver hole depth",
   " 22   source component numbers",
   " 23   receiver component numbers",
   " 24   panel number",
   " 25   largest absolute value on trace",
   " 26   source line number",
   " 27   receiver line number",
   " 28   receiver shotpoint",
   " 29   source shotpoint",
   " 30   scratch",
   " 31   scratch",
   " 32   scratch",
   " 33   source X grid coordinate",
   " 34   source Y grid coordinate",
   " 35   receiver X grid coordinate",
   " 36   receiver Y grid coordinate",
   " 37   CMP shotpoint",
   " 38   CMP line number",
   " 39   pre-NMO datum shift  (ms)",
   " 40   post-NMO datum shift  (ms)",
   " 41   cumulative datum static  (ms)",
   " 42   cumulative refraction static  (ms)",
   " 43   cumulative residual static  (ms)",
   " 44   source uphole time  (ms)",
   " 45   receiver uphole time  (ms)",
   " 46   source sequential ground position",
   " 47   receiver sequential ground position",
   " 48   user defined",
   " 49   user defined",
   " 50   user defined",
   " 51   user defined",
   " 52   user defined",
   " 53   user defined",
   " 54   user defined",
   " 55   user defined",
   " 56   pre-NMO refraction shift  (ms)",
   " 57   post-NMO refraction shift  (ms)",
   " 58   scratch",
   " 59   scratch",
   " 60   scratch",
   " 61   scratch",
   " 62   scratch",
   " 63   GVS modifier",
   " 64   tail mute index"
   };

//----------------------------- end of class -------------------------------//
//----------------------------- end of class -------------------------------//
//----------------------------- end of class -------------------------------//

}

//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
