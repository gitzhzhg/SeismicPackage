// URLReader.java

package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.gui.ErrorInfoWarningDialog;

import java.io.BufferedReader;
import java.io.InputStreamReader;

import java.net.URL;
import java.net.URLConnection;

/**
 * A general utility for reading data from a URL stream.
 * The URL stream is presumed to contain text data.
 */
public class URLReader {
  /**
   * Reads the text data from the specified URL.
   * 
   * @param urlStr string describing the URL 
   * @return the string that contains the text read from the URL stream
   */
  public static String read(String urlStr) {  
    try {
      URL url = new URL(urlStr);
      URLConnection conn = url.openConnection();
      
      StringBuffer result = new StringBuffer();
      BufferedReader in = new BufferedReader(new InputStreamReader(conn.getInputStream()));
      String line;
      while(null != ((line = in.readLine()))) {
        result.append(line);
      }
      in.close();
      return result.toString();
    } 
    catch (Exception e) {
      String[] messages = { "URLReader::Failed to Read Help", e.toString() };
      ErrorInfoWarningDialog.showErrorDialog(messages);
      return null;
    }        
  }
}