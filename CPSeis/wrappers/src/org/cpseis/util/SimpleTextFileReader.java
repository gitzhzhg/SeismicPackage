package org.cpseis.util;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.FileReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;

/**
<pre>
          A simple class for reading a text file line by line.
------------------------------------------------------------------------------
                         SimpleTextFileReader.java
------------------------------------------------------------------------------
 The file is opened by the constructor.
 The file is closed when an EOF is encountered or an error occurs.
 The file can also be closed by calling close(), which can be called
  more than once without a problem.
 Attempts to read the file after it is closed simply returns an error.
 All exceptions are caught by this class.
 If the filename is null, this class reads from standard in.
------------------------------------------------------------------------------
                          RECENT REVISION HISTORY              

     Date        Author       Description
     ----        ------       -----------
  1. 2006-04-11  Stoeckley    Initial version.
------------------------------------------------------------------------------
</pre>
*/


//---------------------------- start of class ----------------------------//
//---------------------------- start of class ----------------------------//
//---------------------------- start of class ----------------------------//


public class SimpleTextFileReader
{


//------------------------------- data --------------------------------------//
//------------------------------- data --------------------------------------//
//------------------------------- data --------------------------------------//


public static final int OPEN    = 0;  // if file is open in good standing.
public static final int ERROR   = 1;  // if closed after open or read error.
public static final int EOF     = 2;  // if closed after EOF was encountered.
public static final int UCLOSED = 3;  // if closed by user without error or EOF.

private static final String[] WHOOPS = { "OPEN", "ERROR", "EOF", "UCLOSED" };

private String         _filename  = "";
private int            _status    = OPEN;   // OPEN or ERROR or EOF or UCLOSED.
private String         _message   = "file open";  // error message.
private BufferedReader _reader    = null;


//---------------------------- get values ------------------------------//
//---------------------------- get values ------------------------------//
//---------------------------- get values ------------------------------//


public int     getStatus   ()  { return _status; }
public String  getWhoops   ()  { return WHOOPS[_status]; }
public String  getMessage  ()  { return _message; }

public boolean isOpen      ()  { return (_status == OPEN   ); }
public boolean isError     ()  { return (_status == ERROR  ); }
public boolean isEof       ()  { return (_status == EOF    ); }
public boolean isUserClosed()  { return (_status == UCLOSED); }
public boolean isClosed    ()  { return (_status != OPEN   ); }


//------------------------------ print status -----------------------------//
//------------------------------ print status -----------------------------//
//------------------------------ print status -----------------------------//


public void printStatus()
{
  System.out.println("SimpleTextFileReader: filename = " + _filename);
  System.out.println("SimpleTextFileReader: status   = " + WHOOPS[_status]);
  System.out.println("SimpleTextFileReader: message  = " + _message);
}


//---------------------------- constructor -------------------------------//
//---------------------------- constructor -------------------------------//
//---------------------------- constructor -------------------------------//


public SimpleTextFileReader(String filename)
{
  _filename = filename;
  try
      {
      if(filename == null)
          {
          _reader = new BufferedReader(new InputStreamReader(System.in));
          }
      else
          {
          _reader = new BufferedReader(new FileReader(filename));
          }
      }
  catch(FileNotFoundException e)
      {
      _status  = ERROR;
      _message = e.getMessage();
      }
}


//---------------------------- finalize --------------------------------//
//---------------------------- finalize --------------------------------//
//---------------------------- finalize --------------------------------//


protected void finalize() throws Throwable
{
  close();
}


//------------------------------ close -----------------------------------//
//------------------------------ close -----------------------------------//
//------------------------------ close -----------------------------------//


public void close()
{
  if(_status != OPEN) return;
  try
      {
      _reader.close();
      }
  catch(IOException e)
      {
      _status  = UCLOSED;
      _message = e.getMessage();
      return;
      }
  _status  = UCLOSED;
  _message = "file closed";
}


//--------------------------- read line -----------------------------------//
//--------------------------- read line -----------------------------------//
//--------------------------- read line -----------------------------------//

      // returns null if EOF is encountered.
      // returns null if read error occurred.
      // returns null if file is already closed.

public String readLine()
{
  if(_status != OPEN) return null;
  try
      {
      String line = _reader.readLine();
      if(line == null)
          {
          close();
          _status  = EOF;
          _message = "EOF encountered";
          return null;
          }
      return line;
      }
  catch(IOException e)
      {
      close();
      _status  = ERROR;
      _message = e.getMessage();
      return null;
      }
  // return null;  // unreachable statement.
}


//------------------------------ read file --------------------------------//
//------------------------------ read file --------------------------------//
//------------------------------ read file --------------------------------//


public static String[] readFile (String filename, String[] errmsg)
{
  SimpleTextFileReader reader = new SimpleTextFileReader(filename);
  if(reader.isError())
      {
      if(errmsg != null) errmsg[0] = reader.getMessage();
      return null;
      }
  ArrayList<String> arraylist = new ArrayList<String>();
  while(true)
      {
      String line = reader.readLine();
      if(line == null)
          {
          if(reader.isEof()) break;
          if(errmsg != null) errmsg[0] = reader.getMessage();
          return null;
          }
      if(line.trim().startsWith("#")) continue;
      arraylist.add(line);
      }
  if(errmsg != null) errmsg[0] = null;
  return arraylist.toArray(new String[0]);
}


//------------------------- end of class ----------------------------------//
//------------------------- end of class ----------------------------------//
//------------------------- end of class ----------------------------------//

}

//----------------------------- end --------------------------------------//
//----------------------------- end --------------------------------------//
//----------------------------- end --------------------------------------//

