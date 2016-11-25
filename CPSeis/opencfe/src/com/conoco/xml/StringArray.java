// StringArray.java

package com.conoco.xml;

import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;
import java.io.IOException;

import com.conoco.cfe.utils.URLDecoder;
import java.net.URLEncoder;

import java.util.StringTokenizer;

/**
 * A class that provides some static utility methods
 * for strings.
 */
public abstract class StringArray {
  
  /**
   * This was copied almost completely from XML.java. (URL) Encodes
   * and outputs an array of strings.
   * 
   * @param array the array of strings to be encoded
   * @return the encoded string
   */
  public static String output(String[] array) {
    ByteArrayOutputStream bos = new ByteArrayOutputStream();
    PrintWriter pWriter = new PrintWriter(bos);

    output(pWriter, array);
    pWriter.flush();

    try {
      bos.close();
    } catch (IOException e) {
      // Do Nothing
    }

    return URLEncoder.encode(bos.toString());
  }

  /**
   * Outputs an array of tokens to a <code>java.io.PrintWriter</code>.
   *
   * @param writer The print writer used to output the array of tokens.
   * @param array An array of tokens.
   */
  public static void output(PrintWriter writer, String[] array) {
    for (int i = 0; i < array.length; i++) {
      if (array[i] == null) {
        writer.print("\r");
      }
      else
      if (array[i].length() == 0) {
        writer.print("\r");
      }
      else {
        writer.print(array[i]);
      }
      writer.print("\n");
    }
  }

  /**
   * This parses a whitespace-delimited string containing tokens.
   *
   * @param value A whitespace-delimited string containing tokens.
   */
  public static String[] parseStringArray(String value) {
    if (value == null) {
      return null;
    }

    StringTokenizer tokenizer;
    try {
      tokenizer = new StringTokenizer(URLDecoder.decode(value), "\n");
    }
    catch (Exception e) {
      System.exit(0);
      return null;
    }
    
    int count = tokenizer.countTokens();

    if (count == 0) {
      return null;
    }

    String[] retValues = new String[count];
    for (int i = 0; i < count; i++) {
      retValues[i] = tokenizer.nextToken();
      if (retValues[i].equals("\r")) {
        retValues[i] = "";
      }
    }
    return retValues;
  }
  
  public static void main(String[] args) {
    
    String s = "one\n\"#kdkdkdk\ndjdjdj";
    
    String[] arr = StringArray.parseStringArray(s);  
    
    for ( int i = 0; i < arr.length; i++) {
      System.err.println("-->" + arr[i]);  
    }
    
  }
}