///
/// TimeStamp.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  1. 10-08-2003 SMCook   Initial version.
///

package com.conoco.shared;

import java.util.Calendar;
import java.util.Date;
import java.text.SimpleDateFormat;

public class TimeStamp
{
  private static final String DEFAULT="MM-dd HH:mm:ss";
  private static SimpleDateFormat df=new SimpleDateFormat(DEFAULT);

  public TimeStamp() {
    this(DEFAULT);
  }

  public TimeStamp(String format) {
    df=new SimpleDateFormat(format);
  }

  public static String getTimeStamp() {
    return df.format(Calendar.getInstance().getTime());
  }
}
