package org.cpseis.util;

import org.cpseis.util.StringUtil;
import org.cpseis.util.SingleTimer;

/**
<pre>
               Measure and print a set of execution times.
--------------------------------------------------------------------------------
                            Timers.java
--------------------------------------------------------------------------------
                         RECENT REVISION HISTORY              

     Date        Author       Description
     ----        ------       -----------
  1. 2006-10-25  Stoeckley    Initial version.
--------------------------------------------------------------------------------
</pre>
*/


//-------------------------- start of class -----------------------------//
//-------------------------- start of class -----------------------------//
//-------------------------- start of class -----------------------------//

public class Timers
{

//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//


private static boolean ENABLED = false;

private String        _name   = null;
private SingleTimer[] _timers = null;
private int           _kount  = 0;

public static void setEnabled(boolean enabled) { ENABLED = enabled; }


//--------------------------- constructor --------------------------------//
//--------------------------- constructor --------------------------------//
//--------------------------- constructor --------------------------------//


/**
Timers are disabled if ENABLED is false at the time this constructor is called.
*/
public Timers(String name)
{
  if(!ENABLED) return;
  _name = name;
  _timers = new SingleTimer [99];
  for(SingleTimer timer : _timers) { timer = null; }
}


//-------------------------- methods ----------------------------------//
//-------------------------- methods ----------------------------------//
//-------------------------- methods ----------------------------------//


public void start(int indx, String message, String suffix)
{
  if(_name == null) return;
  if(_timers[indx] == null) _timers[indx] = new SingleTimer(_name, indx, message, suffix);
  _timers[indx].start();
}


public void stop(int indx)
{
  if(_name == null) return;
  _timers[indx].stop();
}


public void print()
{
  if(_name == null) return;
  for(SingleTimer timer : _timers) { if(timer != null) timer.print(); }
}


//---------------------------- end of class ---------------------------------//
//---------------------------- end of class ---------------------------------//
//---------------------------- end of class ---------------------------------//

}

//-------------------------------- end -------------------------------------//
//-------------------------------- end -------------------------------------//
//-------------------------------- end -------------------------------------//

