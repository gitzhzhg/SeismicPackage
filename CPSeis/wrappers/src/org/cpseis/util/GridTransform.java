package org.cpseis.util;

/**
<pre>
            Java wrapper around the CPS grid transform primitive.
              See the CPS grid transform primitive for details.
--------------------------------------------------------------------------------
                             GridTransform.java
--------------------------------------------------------------------------------
                         SUMMARY OF PUBLIC METHODS
 
 static double[] fetchIdentityGrid()
 static double[] fetchGrid (double xorigin, double yorigin, double angle,
                            int handedness, double xwidth, double ywidth)

        constructor   GridTransform    ()
        constructor   GridTransform    (double... grid) xorigin,yorigin,dx11,dx21,dx12,dx22
        constructor   GridTransform    (double xorigin, double yorigin,
                                        double angle,
                                        double xwidth, double ywidth, int hand)

        void          destroy          ()
        void          finalize         ()
        void          copy             (GridTansform other)

        double        getXorigin       ()
        double        getYorigin       ()
        double        getRotationAngle ()
        double        getXgridWidth    ()
        double        getYgridWidth    ()
        int           getHandedness    ()
        boolean       isRightHanded    ()
        boolean       isLeftHanded     ()

        double[]      getGrid          ()   xorigin,yorigin,dx11,dx21,dx12,dx22

        double        getCosineAngle   ()
        double        getSineAngle     ()
        double        getDx11          ()
        double        getDx21          ()
        double        getDx12          ()
        double        getDx22          ()
        double        getDn11          ()
        double        getDn21          ()
        double        getDn12          ()
        double        getDn22          ()
        double        getDeterminant   ()

        double        getXlocCoord     (double xgrid, double ygrid)
        double        getYlocCoord     (double xgrid, double ygrid)
        double        getXgridCoord    (double xloc , double yloc )
        double        getYgridCoord    (double xloc , double yloc )

        void          initialize       ()
        void          setXorigin       (double xorigin)
        void          setYorigin       (double yorigin)
        void          setRotationAngle (double angle)
        void          setXgridWidth    (double xwidth)
        void          setYgridWidth    (double ywidth)
        void          setHandedness    (int hand)
        void          setRightHanded   ()
        void          setLeftHanded    ()

        void setTransform            (double xorigin, double yorigin,
                                      double angle,
                                      double xwidth, double ywidth, int hand)

        void setRightHandedTransform (double xorigin, double yorigin,
                                      double angle,
                                      double xwidth, double ywidth)

        void setLeftHandedTransform  (double xorigin, double yorigin,
                                      double angle,
                                      double xwidth, double ywidth)

        void          setDx11          (double dx11)
        void          setDx21          (double dx21)
        void          setDx12          (double dx12)
        void          setDx22          (double dx22)
        void          setDn11          (double dn11)
        void          setDn21          (double dn21)
        void          setDn12          (double dn12)
        void          setDn22          (double dn22)

        void setGrid (double... grid)     xorigin,yorigin,dx11,dx21,dx12,dx22

        void setForwardRotationMatrix
                     (double dx11, double dx21, double dx12, double dx22)

        void setReverseRotationMatrix
                     (double dn11, double dn21, double dn12, double dn22)

        void defineOrigin         (double xgrid, double ygrid,
                                   double xloc , double yloc)

        void defineRotationAngle  (double xloc1, double yloc1,
                                   double xloc2, double yloc2)

        void defineOriginAndAngle (double xgrid, double ygrid,
                                   double xloc1, double yloc1,
                                   double xloc2, double yloc2)

        void refineBinCenter      (double xloc, double yloc)
        void refineRotationAngle  (double xloc, double yloc)
        void incrementGridCoords  (double xstep, double ystep)

        void defineTransform      (int npoints,
                   final double   xloc[], final double   yloc[],   // input
                   final double  xgrid[], final double  ygrid[],   // input
                         double xresid[],       double yresid[])   // output
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


public class GridTransform
{

//-------------------------------- data ----------------------------------//
//-------------------------------- data ----------------------------------//
//-------------------------------- data ----------------------------------//

  private long    _pointer = 0;
 
  public long getPointer() { return _pointer; }  // for JNI code only.

//--------------------------- static methods ------------------------------//
//--------------------------- static methods ------------------------------//
//--------------------------- static methods ------------------------------//

public static double[] fetchIdentityGrid()
{
  GridTransform transform = new GridTransform();
  double[] grid = transform.getGrid();
  transform.destroy();
  return grid;
}

public static double[] fetchGrid (double xorigin, double yorigin, double angle,
                                  double xwidth, double ywidth, int hand)
{
  GridTransform transform = new GridTransform
                    (xorigin, yorigin, angle, xwidth, ywidth, hand);
  double[] grid = transform.getGrid();
  transform.destroy();
  return grid;
}

//-------------------------- native prototypes ----------------------------//
//-------------------------- native prototypes ----------------------------//
//-------------------------- native prototypes ----------------------------//

  static { System.loadLibrary("org_cpseis_util_library"); }

  private native long nativeCreate   ();
  private native void nativeDestroy  (long pointer);

  // get values:

  private native double  nativeGetXorigin       (long pointer);
  private native double  nativeGetYorigin       (long pointer);
  private native double  nativeGetRotationAngle (long pointer);
  private native double  nativeGetXgridWidth    (long pointer);
  private native double  nativeGetYgridWidth    (long pointer);
  private native int     nativeGetHandedness    (long pointer);   // +1 or -1
  private native boolean nativeIsRightHanded    (long pointer);
  private native boolean nativeIsLeftHanded     (long pointer);

  private native double  nativeGetCosineAngle   (long pointer);
  private native double  nativeGetSineAngle     (long pointer);
  private native double  nativeGetDx11          (long pointer);
  private native double  nativeGetDx21          (long pointer);
  private native double  nativeGetDx12          (long pointer);
  private native double  nativeGetDx22          (long pointer);
  private native double  nativeGetDn11          (long pointer);
  private native double  nativeGetDn21          (long pointer);
  private native double  nativeGetDn12          (long pointer);
  private native double  nativeGetDn22          (long pointer);
  private native double  nativeGetDeterminant   (long pointer);

  // get transformed coordinates:
  // if either input argument is NC.DNIL, returns NC.DNIL:

  private native double nativeGetXlocCoord 
                             (long pointer, double xgrid, double ygrid);
  private native double nativeGetYlocCoord 
                             (long pointer, double xgrid, double ygrid);
  private native double nativeGetXgridCoord 
                            (long pointer, double xloc , double yloc );
  private native double nativeGetYgridCoord 
                            (long pointer, double xloc , double yloc );

  // set values:

  private native void nativeInitialize       (long pointer);
  private native void nativeSetXorigin       (long pointer, double xorigin);
  private native void nativeSetYorigin       (long pointer, double yorigin);
  private native void nativeSetRotationAngle (long pointer, double angle);
  private native void nativeSetXgridWidth    (long pointer, double xwidth);
  private native void nativeSetYgridWidth    (long pointer, double ywidth);
  private native void nativeSetHandedness    (long pointer, int hand);
                                                                 // +1 or -1
  private native void nativeSetRightHanded   (long pointer);
  private native void nativeSetLeftHanded    (long pointer);

  private native void nativeSetTransform            
                       (long pointer, double xorigin, double yorigin,
                        double angle, double xwidth, double ywidth, int hand);
  private native void nativeSetRightHandedTransform 
                       (long pointer, double xorigin, double yorigin,
                        double angle, double xwidth, double ywidth);
  private native void nativeSetLeftHandedTransform  
                       (long pointer, double xorigin, double yorigin,
                        double angle, double xwidth, double ywidth);

  private native void nativeSetDx11 (long pointer, double dx11);
  private native void nativeSetDx21 (long pointer, double dx21);
  private native void nativeSetDx12 (long pointer, double dx12);
  private native void nativeSetDx22 (long pointer, double dx22);
  private native void nativeSetDn11 (long pointer, double dn11);
  private native void nativeSetDn21 (long pointer, double dn21);
  private native void nativeSetDn12 (long pointer, double dn12);
  private native void nativeSetDn22 (long pointer, double dn22);

  private native void nativeSetForwardRotationMatrix
           (long pointer, double dx11, double dx21, double dx12, double dx22);
  private native void nativeSetReverseRotationMatrix
           (long pointer, double dn11, double dn21, double dn12, double dn22);

  // useful functions to define coordinate system:
  // xresid and yresid can be null:

  private native void nativeDefineOrigin         (long pointer,
                                                 double xgrid, double ygrid,
                                                 double xloc , double yloc );
  private native void nativeDefineRotationAngle  (long pointer,
                                                 double xloc1, double yloc1,
                                                 double xloc2, double yloc2);
  private native void nativeDefineOriginAndAngle (long pointer,
                                                 double xgrid, double ygrid,
                                                 double xloc1, double yloc1,
                                                 double xloc2, double yloc2);
  private native void nativeRefineBinCenter      (long pointer,
                                                 double xloc , double yloc );
  private native void nativeRefineRotationAngle  (long pointer,
                                                 double xloc , double yloc );
  private native void nativeIncrementGridCoords  (long pointer,
                                                 double xstep, double ystep);
  private native void nativeDefineTransform      (long pointer,
                                                 int npoints,
                      final double   xloc[], final double   yloc[],   // input
                      final double  xgrid[], final double  ygrid[],   // input
                            double xresid[], double yresid[]);  // output

//--------------------------- constructors --------------------------------//
//--------------------------- constructors --------------------------------//
//--------------------------- constructors --------------------------------//

public GridTransform()
{
  _pointer = nativeCreate();
/*
System.out.println("GridTransform.java: this = " + this);
System.out.println("GridTransform.java: _pointer = " + _pointer);
*/
}
 
public GridTransform (double xorigin, double yorigin, double angle,
                      double xwidth, double ywidth, int hand)
{
  _pointer = nativeCreate();
  setTransform (xorigin, yorigin, angle, xwidth, ywidth, hand);
}

public GridTransform (double... grid)
{
  _pointer = nativeCreate();
  setGrid (grid);
}

//----------------------- misc methods --------------------------------//
//----------------------- misc methods --------------------------------//
//----------------------- misc methods --------------------------------//

public void destroy()
{
  if(_pointer == 0) return;
  nativeDestroy(_pointer);
  _pointer = 0;
}

public void finalize()
{
  if(_pointer == 0) return;
  nativeDestroy(_pointer);
  _pointer = 0;
}

public void copy(GridTransform other)
{
  double xorigin = other.getXorigin       ();
  double yorigin = other.getYorigin       ();
  double angle   = other.getRotationAngle ();
  double xwidth  = other.getXgridWidth    ();
  double ywidth  = other.getYgridWidth    ();
  int    hand    = other.getHandedness    ();
  setTransform(xorigin, yorigin, angle, xwidth, ywidth, hand);
}

//------------------------- get values -------------------------------//
//------------------------- get values -------------------------------//
//------------------------- get values -------------------------------//

public double  getXorigin       () { return nativeGetXorigin       (_pointer); }
public double  getYorigin       () { return nativeGetYorigin       (_pointer); }
public double  getRotationAngle () { return nativeGetRotationAngle (_pointer); }
public double  getXgridWidth    () { return nativeGetXgridWidth    (_pointer); }
public double  getYgridWidth    () { return nativeGetYgridWidth    (_pointer); }
public int     getHandedness    () { return nativeGetHandedness    (_pointer); }
public boolean isRightHanded    () { return nativeIsRightHanded    (_pointer); }
public boolean isLeftHanded     () { return nativeIsLeftHanded     (_pointer); }

public double[] getGrid()
{
  double[] grid = new double[6];
  grid[0] = getXorigin();
  grid[1] = getYorigin();
  grid[2] = getDx11();
  grid[3] = getDx21();
  grid[4] = getDx12();
  grid[5] = getDx22();
  return grid;
}

public double  getCosineAngle   () { return nativeGetCosineAngle (_pointer); }
public double  getSineAngle     () { return nativeGetSineAngle   (_pointer); }
public double  getDx11          () { return nativeGetDx11        (_pointer); }
public double  getDx21          () { return nativeGetDx21        (_pointer); }
public double  getDx12          () { return nativeGetDx12        (_pointer); }
public double  getDx22          () { return nativeGetDx22        (_pointer); }
public double  getDn11          () { return nativeGetDn11        (_pointer); }
public double  getDn21          () { return nativeGetDn21        (_pointer); }
public double  getDn12          () { return nativeGetDn12        (_pointer); }
public double  getDn22          () { return nativeGetDn22        (_pointer); }
public double  getDeterminant   () { return nativeGetDeterminant (_pointer); }

//-------------------- coordinate transforms -------------------------//
//-------------------- coordinate transforms -------------------------//
//-------------------- coordinate transforms -------------------------//

  // get transformed coordinates:
  // if either input argument is NC.DNIL, returns NC.DNIL:

/**
Get X survey coordinate from grid coordinates.
@param xgrid X grid coordinate (CMP bin units).
@param ygrid Y grid coordinate (CMP bin units).
@return X (easting) survey coordinate (feet or meters).
*/
public double getXlocCoord  (double xgrid, double ygrid)
{
  return nativeGetXlocCoord(_pointer, xgrid, ygrid);
}

/**
Get Y survey coordinate from grid coordinates.
@param xgrid X grid coordinate (CMP bin units).
@param ygrid Y grid coordinate (CMP bin units).
@return Y (northing) survey coordinate (feet or meters).
*/
public double getYlocCoord  (double xgrid, double ygrid)
{
  return nativeGetYlocCoord(_pointer, xgrid, ygrid);
}


/**
Get X grid coordinate from survey coordinates.
@param xloc X (easting) survey coordinate (feet or meters).
@param yloc Y (northing) survey coordinate (feet or meters).
@return X grid coordinate (CMP bin units).
*/
public double getXgridCoord (double xloc , double yloc )
{
  return nativeGetXgridCoord(_pointer, xloc, yloc);
}


/**
Get Y grid coordinate from survey coordinates.
@param xloc X (easting) survey coordinate (feet or meters).
@param yloc Y (northing) survey coordinate (feet or meters).
@return Y grid coordinate (CMP bin units). 
*/
public double getYgridCoord (double xloc , double yloc )
{
  return nativeGetYgridCoord(_pointer, xloc, yloc);
}

//--------------------------- set values -----------------------------//
//--------------------------- set values -----------------------------//
//--------------------------- set values -----------------------------//

public void initialize       ()               
{
  nativeInitialize       (_pointer);
}

public void setXorigin       (double xorigin) 
{
  nativeSetXorigin       (_pointer, xorigin);
}

public void setYorigin       (double yorigin) 
{
  nativeSetYorigin       (_pointer, yorigin);
}

public void setRotationAngle (double angle)   
{
  nativeSetRotationAngle (_pointer, angle);
}

public void setXgridWidth    (double xwidth)  
{
  nativeSetXgridWidth    (_pointer, xwidth);
}

public void setYgridWidth    (double ywidth)  
{
  nativeSetYgridWidth    (_pointer, ywidth);
}

public void setHandedness    (int hand)       
{
  nativeSetHandedness    (_pointer, hand);
}

public void setRightHanded   ()               
{
  nativeSetRightHanded   (_pointer);
}

public void setLeftHanded    ()               
{
  nativeSetLeftHanded    (_pointer);
}

public void setTransform            (double xorigin, double yorigin,
                                     double angle,
                                     double xwidth, double ywidth, int hand)
{
  nativeSetTransform(_pointer, xorigin, yorigin, angle, xwidth, ywidth, hand);
}

public void setRightHandedTransform (double xorigin, double yorigin,
                                     double angle,
                                     double xwidth, double ywidth)
{
  nativeSetRightHandedTransform
                   (_pointer, xorigin, yorigin, angle, xwidth, ywidth);
}

public void setLeftHandedTransform  (double xorigin, double yorigin,
                                     double angle,
                                     double xwidth, double ywidth)
{
  nativeSetLeftHandedTransform
                   (_pointer, xorigin, yorigin, angle, xwidth, ywidth);
}


public void setDx11 (double dx11) { nativeSetDx11(_pointer, dx11); }
public void setDx21 (double dx21) { nativeSetDx21(_pointer, dx21); }
public void setDx12 (double dx12) { nativeSetDx12(_pointer, dx12); }
public void setDx22 (double dx22) { nativeSetDx22(_pointer, dx22); }
public void setDn11 (double dn11) { nativeSetDn11(_pointer, dn11); }
public void setDn21 (double dn21) { nativeSetDn21(_pointer, dn21); }
public void setDn12 (double dn12) { nativeSetDn12(_pointer, dn12); }
public void setDn22 (double dn22) { nativeSetDn22(_pointer, dn22); }

public void setGrid (double... grid)
{
  if(grid == null || grid.length != 6) grid = fetchIdentityGrid();

  setXorigin               (grid[0]);
  setYorigin               (grid[1]);
  setForwardRotationMatrix (grid[2], grid[3], grid[4], grid[5]);
}

public void setForwardRotationMatrix
                       (double dx11, double dx21, double dx12, double dx22)
{
  nativeSetForwardRotationMatrix(_pointer, dx11, dx21, dx12, dx22);
}

public void setReverseRotationMatrix
                       (double dn11, double dn21, double dn12, double dn22)
{
  nativeSetReverseRotationMatrix(_pointer, dn11, dn21, dn12, dn22);
}

//---------------------- define coordinate transform -----------------//
//---------------------- define coordinate transform -----------------//
//---------------------- define coordinate transform -----------------//

  // useful functions to define coordinate system:
  // xresid and yresid can be null:

public void defineOrigin         (double xgrid, double ygrid,
                                  double xloc , double yloc) 
{
  nativeDefineOrigin         (_pointer, xgrid, ygrid, xloc , yloc);
}

public void defineRotationAngle  (double xloc1, double yloc1,
                                  double xloc2, double yloc2) 
{
  nativeDefineRotationAngle  (_pointer, xloc1, yloc1, xloc2, yloc2);
}

public void defineOriginAndAngle (double xgrid, double ygrid,
                                  double xloc1, double yloc1,
                                  double xloc2, double yloc2) 
{
  nativeDefineOriginAndAngle
                       (_pointer, xgrid, ygrid, xloc1, yloc1, xloc2, yloc2);
}

public void refineBinCenter      (double xloc, double yloc) 
{
  nativeRefineBinCenter      (_pointer, xloc, yloc);
}

public void refineRotationAngle  (double xloc, double yloc) 
{
  nativeRefineRotationAngle  (_pointer, xloc, yloc);
}

public void incrementGridCoords  (double xstep, double ystep) 
{
  nativeIncrementGridCoords  (_pointer, xstep, ystep);
}

public void defineTransform      (int npoints,
                     final double   xloc[], final double   yloc[],   // input
                     final double  xgrid[], final double  ygrid[],   // input
                           double xresid[],       double yresid[])   // output
{
  nativeDefineTransform
            (_pointer, npoints, xloc, yloc, xgrid, ygrid, xresid, yresid);
}

//--------------------------- end of class -------------------------------//
//--------------------------- end of class -------------------------------//
//--------------------------- end of class -------------------------------//

}

//-------------------------------- end ---------------------------------//
//-------------------------------- end ---------------------------------//
//-------------------------------- end ---------------------------------//
