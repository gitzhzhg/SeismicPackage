package org.cpseis.util;

import org.cpseis.util.PC;
import org.cpseis.util.NC;

               // This is a java base class for CPS processes.
               // This class wraps a similar C++ class.

//-------------------------------- start of class ------------------------------//
//-------------------------------- start of class ------------------------------//
//-------------------------------- start of class ------------------------------//

public abstract class CpseisBase
{

//--------------------------------- data ---------------------------------------//
//--------------------------------- data ---------------------------------------//
//--------------------------------- data ---------------------------------------//

private long          _pointer      = 0;
private String        _name         = "none";
private boolean       _need_label   = false;
private boolean       _need_request = false;

//---------------------------- get values ----------------------------//
//---------------------------- get values ----------------------------//
//---------------------------- get values ----------------------------//

public final String getName()
{
  return (_name);
}

public final boolean isInputTool()
{
  return (_need_label && !_need_request);
}

//-------------------------- native prototypes ----------------------------//
//-------------------------- native prototypes ----------------------------//
//-------------------------- native prototypes ----------------------------//

static { System.loadLibrary("org_cpseis_util_library"); }

protected native long nativeCreate   ();
private   native void nativeDestroy  (long pointer);
private   native void nativeUpdate   (long pointer);
private   native void nativeWrapup   (long pointer);
private   native void nativePutTrace (long pointer, int itr, double hd[], float tr[]);
private   native void nativeGetTrace (long pointer, int itr, double hd[], float tr[]);
private   native int  nativeExecute  (long pointer, int ntr);

//---------------------------- constructor -----------------------------//
//---------------------------- constructor -----------------------------//
//---------------------------- constructor -----------------------------//

public CpseisBase(String name)
{
  super();
  _name = name;
  _pointer = nativeCreate();
}

//--------------------------- update ----------------------------//
//--------------------------- update ----------------------------//
//--------------------------- update ----------------------------//

// Before calling update:
// (1) Call PC.frontendUpdateNoprint() or PC.backendUpdate().
//     Noprint puts -6 into the parameter cache.  This allows
//     the CPS process to get unit number +6 from the parameter
//     cache even though the parameter cache will not print any
//     messages.
// (2) Then put parameters into the parameter cache.

// After calling update:
// (1) Get parameters from the parameter cache.
// (2) Then call PC.restore().
// (3) Then if this is a backend update, do the following, so that one
//     instance will be available for all processes during the run phase.
//     Must be done here while everything is still synchronous.
//        if(PC.exists() == false)
//          {
//          PC.backendUpdate();
//          PC.backendExecute();
//          }
//     It is OK if someone else does a PC.backendUpdate() and
//     PC.restore() combination after this point because that
//     will not affect the state of the parameter cache at
//     this point, since the state here will be restored.

public final void update()
{
  nativeUpdate(_pointer);

  _need_label   = PC.getControl ("need_label",  false);
  _need_request = PC.getControl ("need_request",  false);
}

//---------------------- put and get trace --------------------------//
//---------------------- put and get trace --------------------------//
//---------------------- put and get trace --------------------------//

public final void putTrace (int itr, double[] hd, float[] tr)
{
  nativePutTrace (_pointer, itr, hd, tr);
}

public final void getTrace (int itr, double[] hd, float[] tr)
{
  nativeGetTrace (_pointer, itr, hd, tr);
}

//--------------------------- execute -----------------------------//
//--------------------------- execute -----------------------------//
//--------------------------- execute -----------------------------//

public final int execute (int ntr)
{
  synchronized(CpseisBase.class)
      {
      ntr = nativeExecute(_pointer, ntr);
      }
  return ntr;
}

//---------------------------- wrapup ---------------------------------//
//---------------------------- wrapup ---------------------------------//
//---------------------------- wrapup ---------------------------------//

public final void wrapup()
{
  nativeWrapup(_pointer);
}

//--------------------------- destroy ---------------------------------//
//--------------------------- destroy ---------------------------------//
//--------------------------- destroy ---------------------------------//

public final void destroy()
{
  nativeDestroy(_pointer);
}

//---------------------------- end of class ----------------------------//
//---------------------------- end of class ----------------------------//
//---------------------------- end of class ----------------------------//

}

//-------------------------------- end ---------------------------------//
//-------------------------------- end ---------------------------------//
//-------------------------------- end ---------------------------------//
