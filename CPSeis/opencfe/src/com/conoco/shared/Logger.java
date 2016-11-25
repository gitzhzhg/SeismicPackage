///
/// Logger.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  5.
///  4. 08-02-2002 SMCook   Changed name of pauseForReturnPrompt() function to
///                          pauseForReturn().
///  3. 09-24-2001 SMCook   Added function to report platform that's running.
///

/**
 *
 * Collection of handy stuff one might use when debugging, for example.
 *
 * Easily-set output "stream" -- allows easily redirecting certain
 * output to a PrintStream (e.g. System.out, System.err, LogStream, disk file)
 * AND/OR can output as text in a Graphics object!
 *
 * Includes means to temporarily/permanently shut off output (set<type>Active())
 *
 * Static approach requires some caution but allows for "global" access as one
 * might typically want for a "Logger".
 *
 */

package com.conoco.shared;

import java.awt.Graphics;
import java.awt.Panel;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;

import java.util.Properties;

public class Logger
{

  /**
   * Variables controlling where "printout" occurs.
   */
  private static PrintStream stream=System.out;
  private static Panel       canvas;

  private static boolean streamIsActive;
  private static boolean canvasIsActive;

  /**
   * Platform codes.
   */
  public static final int PLATFORM_WINDOWS = 0;
  public static final int PLATFORM_LINUX   = 1;
  public static final int PLATFORM_SGI     = 2;
  public static final int PLATFORM_SOLARIS = 3;

  /**
   * Variable holds value of most recent String "printed".
   */
  private static String currentString="";

  /**
   * Method #1 for changing output location.
   */
  public static final void setPrintStream(PrintStream os) {
    stream = os;
    if(os==null) {
      streamIsActive = false;
    } else {
      streamIsActive = true;
    }
  }

  /**
   * Method #2 for changing output location.
   */
  public static final void setGraphicsPanel(Panel pan) {
    canvas=pan;
    if(pan==null) {
      canvasIsActive = false;
    } else {
      canvasIsActive = true;
    }
  }

  /**
   * Methods for turning on and off the PrintStream and Panel
   * without nullifying them (could turn them right back on).
   */
  public static final void setPrintStreamActive(boolean v) {
    streamIsActive = v;
  }
  public static final void setGraphicsPanelActive(boolean v) {
    canvasIsActive = v;
  }

  /**
   * Print out how much free memory is currently available (version 1).
   */
  public static void printFreeMemory(Object o) {
    printFreeMemory(o,"");
  }

  /**
   * Print out how much free memory is currently available (version 2).
   */
  public static void printFreeMemory(Object o,String s2) {
    System.gc();
    String s1="";
    if(o!=null) s1=o.getClass().getName();
    println(s1,s2 + " free mem = " + Runtime.getRuntime().freeMemory());
  }

  /**
   * Print to stream w/class label.
   */
  public static void println(Object o,String s) {
    println(o.getClass().getName(),s);
  }

  /**
   * Print to stream.
   */
  public static void println(String s1,String s2) {
    if(streamIsActive)stream.println(">>>> " + s1 + ": " + s2);
    if(canvasIsActive) {
      currentString=s1 + ":: " + s2; 
      canvas.paint(canvas.getGraphics());
    }
  }

  /**
   * Method returns currentString
   */
  public static String getCurrentString() {
    return currentString;
  }

  /**
   * Put thread to sleep.
   */
  public static void sleepMillis(Object o,int msec) {
    println(o,"sleeping");
    try {
      Thread.sleep(msec);
    }
    catch(InterruptedException e) {
    }
  }

  /**
   * Pause execution until user hits return key (no prompt).
   */
  public static void pauseForReturn() {
    byte[] buf=new byte[5];
    try {
      System.in.read(buf);
    }
    catch (IOException e) {
    }
  }

  /**
   * Pause execution until user hits return key (with prompt).
   */
  public static void pauseForReturn(String prompt) {
    System.out.print(prompt);
    System.out.flush();
    pauseForReturn();
  }

  /**
   * Sleep for desired number of milliseconds.
   */
  public static void sleepMillis(long msec) {
    long startTime=System.currentTimeMillis();
    while((System.currentTimeMillis() - startTime) < msec) {
      try {
        Thread.sleep(msec);
      }
      catch (InterruptedException e) {
      }
    }
  }

  /**
   * Function to find out what platform is running.
   */
  public static int getPlatformCode() {
    Properties p=System.getProperties();
    String operatingSystem=p.getProperty("os.name");
    if(operatingSystem.indexOf("indows")>0)
      return PLATFORM_WINDOWS;
    else if(operatingSystem.indexOf("inux")>0)
      return PLATFORM_LINUX;
    else if(operatingSystem.indexOf("rix")>0)
      return PLATFORM_SGI;
    else
      return PLATFORM_SOLARIS;
  }
}
