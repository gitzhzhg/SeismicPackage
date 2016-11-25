package org.cpseis.util;

import org.cpseis.util.StringUtil;

/**
<pre>
                    Measure and print execution times.
--------------------------------------------------------------------------------
                            SingleTimer.java
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

public class SingleTimer
{

//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//


private String _msg    = null;
private String _suffix = null;
private long   _time   = 0;
private long   _sum    = 0;
private long   _nsum   = 0;


//--------------------------- constructor --------------------------------//
//--------------------------- constructor --------------------------------//
//--------------------------- constructor --------------------------------//


public SingleTimer(String name, int indx, String msg, String suffix)
{
  _msg    = name + StringUtil.prependSpaces(StringUtil.toString(indx), 3) + " " + StringUtil.appendSpaces(msg, 60);
  _suffix = suffix;
}


//-------------------------- methods ----------------------------------//
//-------------------------- methods ----------------------------------//
//-------------------------- methods ----------------------------------//


public void start()
{
  _time = System.nanoTime();
}


public void stop()
{
  long newtime = System.nanoTime();
  _sum += (newtime - _time);
  _nsum++;
}


public void print()
{
  long average = 0;
  if(_nsum > 0) average = (long)Math.round((double)_sum / (1000.0 * _nsum));
  String prefix = _msg + "  " + StringUtil.prependSpaces(StringUtil.toString(average), 10);
  System.out.println(prefix + " microseconds " + _suffix);
}


//---------------------------- end of class ---------------------------------//
//---------------------------- end of class ---------------------------------//
//---------------------------- end of class ---------------------------------//

}

//-------------------------------- end -------------------------------------//
//-------------------------------- end -------------------------------------//
//-------------------------------- end -------------------------------------//

