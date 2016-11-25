package org.cpseis.util;

import org.cpseis.util.NC;

/**
<pre>
                  Various string manipulation utilities.
--------------------------------------------------------------------------------
                            StringUtil.java
--------------------------------------------------------------------------------
This class contains various utilities which apparently are not available
in the String class or are easier to use from this class.

Constructor not needed because all member methods are static.
--------------------------------------------------------------------------------
                       SUMMARY OF PUBLIC METHODS

      static int        maybeToInt          (String string)  throws NumberFormatException
      static long       maybeToLong         (String string)  throws NumberFormatException
      static float      maybeToFloat        (String string)  throws NumberFormatException
      static double     maybeToDouble       (String string)  throws NumberFormatException
      static boolean    maybeToBoolean      (String string)  throws NumberFormatException

      static int        toInt          (String string)   returns INIL  if decode error
      static long       toLong         (String string)   returns LNIL  if decode error
      static float      toFloat        (String string)   returns FNIL  if decode error
      static double     toDouble       (String string)   returns DNIL  if decode error
      static boolean    toBoolean      (String string)   returns false if decode error

      static String     toString       (int     number)
      static String     toString       (long    number)
      static String     toString       (float   number)
      static String     toString       (double  number)
      static String     toString       (boolean number)

      static int[]      maybeToIntArray     (String[] strings)  throws NumberFormatException
      static long[]     maybeToLongArray    (String[] strings)  throws NumberFormatException
      static float[]    maybeToFloatArray   (String[] strings)  throws NumberFormatException
      static double[]   maybeToDoubleArray  (String[] strings)  throws NumberFormatException
      static boolean[]  maybeToBooleanArray (String[] strings)  throws NumberFormatException

      static int[]      toIntArray     (String[] strings)
      static long[]     toLongArray    (String[] strings)
      static float[]    toFloatArray   (String[] strings)
      static double[]   toDoubleArray  (String[] strings)
      static boolean[]  toBooleanArray (String[] strings)

      static String[]   toStringArray  (int[]     numbers)
      static String[]   toStringArray  (long[]    numbers)
      static String[]   toStringArray  (float[]   numbers)
      static String[]   toStringArray  (double[]  numbers)
      static String[]   toStringArray  (boolean[] numbers)

      static String     addBackslash     (String string)
      static String     squeezeBlanks    (String string)
      static String     squeezeChar      (String string, char character)
      static String     separateWords    (String string)
      static String     makePrettyLabel  (String string)
      static String     makePrettyLabel  (String string, String label)
      static String     wordsToUpperCase (String string)
      static String     wordsToLowerCase (String string)
      static String     removeBackslash  (String string)
      static String     appendSpaces     (String string, int length)
      static String     prependSpaces    (String string, int length)
      static String     centerSpaces     (String string, int length)
      static String     makeExactLength  (String string, int length)

      static String     getStringFromTokens (String[] tokens)
      static String[]   getTokensFromString (String string)

      static String[]   removeEmptyStrings  (String[] strings)
      static String     joinWithNewline     (String... strings)
      static String[]   joinStringArrays    (String[] strings1, String[] strings2)
      static String[]   centerStringArray   (String... strings)
--------------------------------------------------------------------------------
                         USE AS A MAIN PROGRAM

 When this class is used as a main program, it modifies a string argument.
 See the documentation for the main method for more information.
--------------------------------------------------------------------------------
</pre>
*/


//-------------------------- start of class -----------------------------//
//-------------------------- start of class -----------------------------//
//-------------------------- start of class -----------------------------//

public class StringUtil
{

//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//


//--------------------------- constructor --------------------------------//
//--------------------------- constructor --------------------------------//
//--------------------------- constructor --------------------------------//


//------------- maybe convert from string to number ------------------------//
//------------- maybe convert from string to number ------------------------//
//------------- maybe convert from string to number ------------------------//


/**
Decodes the contents of a string to an integer number.

@param  string   a string which contains an integer number.
@return          the number represented in the string.
Returns {@link NC#INIL} (nil) if the string is null or empty.
Throws NumberFormatException if a decode error occurs.
*/
public static int maybeToInt(String string) throws NumberFormatException
{
  if(string == null || string.trim().equals("")) return NC.INIL;
  try
      {
      return Integer.parseInt(string);
      }
  catch(NumberFormatException e)
      {
      throw new NumberFormatException
                         ("error converting " + string.trim() + " to int");
      }
}


/**
Decodes the contents of a string to a long integer number.

@param  string   a string which contains a long integer number.
@return          the number represented in the string.
Returns {@link NC#LNIL} (nil) if the string is null or empty.
Throws NumberFormatException if a decode error occurs.
*/
public static long maybeToLong(String string) throws NumberFormatException
{
  if(string == null || string.trim().equals("")) return NC.LNIL;
  try
      {
      return Long.parseLong(string);
      }
  catch(NumberFormatException e)
      {
      throw new NumberFormatException
                         ("error converting " + string.trim() + " to long");
      }
}


/**
Decodes the contents of a string to a float number.

@param  string   a string which contains a float number.
@return          the number represented in the string.
Returns {@link NC#FNIL} (nil) if the string is null or empty.
Throws NumberFormatException if a decode error occurs.
*/
public static float maybeToFloat(String string) throws NumberFormatException
{
  if(string == null || string.trim().equals("")) return NC.FNIL;
  try
      {
      return Float.parseFloat(string);
      }
  catch(NumberFormatException e)
      {
      throw new NumberFormatException
                         ("error converting " + string.trim() + " to float");
      }
}


/**
Decodes the contents of a string to a double precision number.

@param  string   a string which contains a double precision number.
@return          the number represented in the string.
Returns {@link NC#DNIL} (nil) if the string is null or empty.
Throws NumberFormatException if a decode error occurs.
*/
public static double maybeToDouble(String string) throws NumberFormatException
{
  if(string == null || string.trim().equals("")) return NC.DNIL;
  try
      {
      return Double.parseDouble(string);
      }
  catch(NumberFormatException e)
      {
      throw new NumberFormatException
                         ("error converting " + string.trim() + " to double");
      }
}


/**
Decodes the contents of a string to a boolean number.

@param  string   a string which contains a boolean number.
@return          the number represented in the string.
Returns true  if the trimmed string (ignoring case) has one of these values: TRUE YES T Y
Returns false if the trimmed string (ignoring case) has one of these values: FALSE NO F N
Throws NumberFormatException if the string is null or empty.
Throws NumberFormatException if the trimmed string is not one of the above values.
*/
public static boolean maybeToBoolean(String string) throws NumberFormatException
{
  if(string == null) throw new NumberFormatException
                                   ("error converting null string to boolean");
  if(string.trim().length() == 0) throw new NumberFormatException
                                   ("error converting empty string to boolean");
  ///// return Boolean.parseBoolean(string); // does not accept "yes" as true.
  if(string.trim().equalsIgnoreCase("true" )) return true;
  if(string.trim().equalsIgnoreCase("yes"  )) return true;
  if(string.trim().equalsIgnoreCase("t"    )) return true;
  if(string.trim().equalsIgnoreCase("y"    )) return true;
  if(string.trim().equalsIgnoreCase("false")) return false;
  if(string.trim().equalsIgnoreCase("no"   )) return false;
  if(string.trim().equalsIgnoreCase("f"    )) return false;
  if(string.trim().equalsIgnoreCase("n"    )) return false;
  throw new NumberFormatException
                         ("error converting " + string.trim() + " to boolean");
}


//------------------- convert from string to number ------------------------//
//------------------- convert from string to number ------------------------//
//------------------- convert from string to number ------------------------//


/**
Decodes the contents of a string to an integer number.

@param  string   a string which contains an integer number.
@return          the number represented in the string.
Returns {@link NC#INIL} (nil) if the string is null or empty.
Returns {@link NC#INIL} (nil) if a decode error occurs.
*/
public static int toInt(String string)
{
  try { return maybeToInt(string); }
  catch (NumberFormatException e) { return NC.INIL; }
}


/**
Decodes the contents of a string to a long integer number.

@param  string   a string which contains a long integer number.
@return          the number represented in the string.
Returns {@link NC#LNIL} (nil) if the string is null or empty.
Returns {@link NC#LNIL} (nil) if a decode error occurs.
*/
public static long toLong(String string)
{
  try { return maybeToLong(string); }
  catch (NumberFormatException e) { return NC.LNIL; }
}


/**
Decodes the contents of a string to a float number.

@param  string   a string which contains a float number.
@return          the number represented in the string.
Returns {@link NC#FNIL} (nil) if the string is null or empty.
Returns {@link NC#FNIL} (nil) if a decode error occurs.
*/
public static float toFloat(String string)
{
  try { return maybeToFloat(string); }
  catch(NumberFormatException e) { return NC.FNIL; }
}


/**
Decodes the contents of a string to a double precision number.

@param  string   a string which contains a double precision number.
@return          the number represented in the string.
Returns {@link NC#DNIL} (nil) if the string is null or empty.
Returns {@link NC#DNIL} (nil) if a decode error occurs.
*/
public static double toDouble(String string)
{
  try { return maybeToDouble(string); }
  catch(NumberFormatException e) { return NC.DNIL; }
}


/**
Decodes the contents of a string to a boolean number.

@param  string   a string which contains a boolean number.
@return          the number represented in the string.
Returns true  if the trimmed string (ignoring case) has one of these values: TRUE YES T Y
Returns false if the trimmed string (ignoring case) has one of these values: FALSE NO F N
Returns false if the string is null or empty.
Returns false if the trimmed string is not one of the above values.
*/
public static boolean toBoolean(String string)
{
  try { return maybeToBoolean(string); }
  catch(NumberFormatException e) { return false; }
}


//------------------- convert from number to string ------------------------//
//------------------- convert from number to string ------------------------//
//------------------- convert from number to string ------------------------//


/**
Encodes a string with an integer value.

@param  number   the number to be converted to a string.
@return          the encoded string.
Returns an empty string if the number is {@link NC#INIL} (nil).
*/
public static String toString(int number)
{
  if(number == NC.INIL) return "";
  return String.valueOf(number);
}


/**
Encodes a string with a long integer value.

@param  number   the number to be converted to a string.
@return          the encoded string.
Returns an empty string if the number is {@link NC#LNIL} (nil).
*/
public static String toString(long number)
{
  if(number == NC.LNIL) return "";
  return String.valueOf(number);
}


/**
Encodes a string with an float value.

@param  number   the number to be converted to a string.
@return          the encoded string.
Returns an empty string if the number is {@link NC#FNIL} (nil).
*/
public static String toString(float number)
{
  if(number == NC.FNIL) return "";
  return String.valueOf(number);
}


/**
Encodes a string with a double value.

@param  number   the number to be converted to a string.
@return          the encoded string.
Returns an empty string if the number is {@link NC#DNIL} (nil).
*/
public static String toString(double number)
{
  if(number == NC.DNIL) return "";
  return String.valueOf(number);
}


/**
Encodes a string with a boolean value.

@param  number   the number to be converted to a string.
@return          a string set to "YES" or "NO".
*/
public static String toString(boolean number)
{
  ////// return String.valueOf(number);  // returns "true" or "false".
  if(number) return "YES";
  return "NO";
}


//--------------- maybe convert from string array to number array -----------------//
//--------------- maybe convert from string array to number array -----------------//
//--------------- maybe convert from string array to number array -----------------//


/**
Decodes the contents of an array of strings to an array of integers.

@param  strings  array of strings which contains integer values.
@return          array of numbers represented in the strings.
Returns null if the input strings array is null.
Returns {@link NC#INIL} (nil) for each string which is null or empty.
Throws NumberFormatException if a decode error occurs.
*/
public static int[] maybeToIntArray(String[] strings) throws NumberFormatException
{
  if(strings == null) return null;
  int[] numbers = new int[strings.length];
  for(int indx = 0; indx < strings.length; indx++)
      {
      numbers[indx] = maybeToInt(strings[indx]);
      }
  return numbers;
}


/**
Decodes the contents of an array of strings to an array of long integers.

@param  strings  array of strings which contains long integer values.
@return          array of numbers represented in the strings.
Returns null if the input strings array is null.
Returns {@link NC#LNIL} (nil) for each string which is null or empty.
Throws NumberFormatException if a decode error occurs.
*/
public static long[] maybeToLongArray(String[] strings) throws NumberFormatException
{
  if(strings == null) return null;
  long[] numbers = new long[strings.length];
  for(int indx = 0; indx < strings.length; indx++)
      {
      numbers[indx] = maybeToLong(strings[indx]);
      }
  return numbers;
}


/**
Decodes the contents of an array of strings to an array of floats.

@param  strings  array of strings which contains float values.
@return          array of numbers represented in the strings.
Returns null if the input strings array is null.
Returns {@link NC#FNIL} (nil) for each string which is null or empty.
Throws NumberFormatException if a decode error occurs.
*/
public static float[] maybeToFloatArray(String[] strings) throws NumberFormatException
{
  if(strings == null) return null;
  float[] numbers = new float[strings.length];
  for(int indx = 0; indx < strings.length; indx++)
      {
      numbers[indx] = maybeToFloat(strings[indx]);
      }
  return numbers;
}


/**
Decodes the contents of an array of strings to an array of doubles.

@param  strings  array of strings which contains double values.
@return          array of numbers represented in the strings.
Returns null if the input strings array is null.
Returns {@link NC#DNIL} (nil) for each string which is null or empty.
Throws NumberFormatException if a decode error occurs.
*/
public static double[] maybeToDoubleArray(String[] strings) throws NumberFormatException
{
  if(strings == null) return null;
  double[] numbers = new double[strings.length];
  for(int indx = 0; indx < strings.length; indx++)
      {
      numbers[indx] = maybeToDouble(strings[indx]);
      }
  return numbers;
}


/**
Decodes the contents of an array of strings to an array of booleans.

@param  strings  array of strings which contains boolean values.
@return          array of numbers represented in the strings.
Returns null if the input strings array is null.
Returns true  for any trimmed string (ignoring case) with one of these values: TRUE YES T Y
Returns false for any trimmed string (ignoring case) with one of these values: FALSE NO F N
Throws NumberFormatException if any string is null or empty.
Throws NumberFormatException if any trimmed string is not one of the above values.
*/
public static boolean[] maybeToBooleanArray(String[] strings) throws NumberFormatException
{
  if(strings == null) return null;
  boolean[] numbers = new boolean[strings.length];
  for(int indx = 0; indx < strings.length; indx++)
      {
      numbers[indx] = maybeToBoolean(strings[indx]);
      }
  return numbers;
}


//--------------------- convert from string array to number array -----------------//
//--------------------- convert from string array to number array -----------------//
//--------------------- convert from string array to number array -----------------//


/**
Decodes the contents of an array of strings to an array of integers.

@param  strings  array of strings which contains integer values.
@return          array of numbers represented in the strings.
Returns null if the input strings array is null.
Returns null if any string in the array suffers a decode error.
Returns {@link NC#INIL} (nil) for each string which is null or empty.
*/
public static int[] toIntArray(String[] strings)
{
  try { return maybeToIntArray(strings); }
  catch (NumberFormatException e) { return null; }
}


/**
Decodes the contents of an array of strings to an array of long integers.

@param  strings  array of strings which contains long integer values.
@return          array of numbers represented in the strings.
Returns null if the input strings array is null.
Returns null if any string in the array suffers a decode error.
Returns {@link NC#LNIL} (nil) for each string which is null or empty.
*/
public static long[] toLongArray(String[] strings)
{
  try { return maybeToLongArray(strings); }
  catch (NumberFormatException e) { return null; }
}


/**
Decodes the contents of an array of strings to an array of floats.

@param  strings  array of strings which contains float values.
@return          array of numbers represented in the strings.
Returns null if the input strings array is null.
Returns null if any string in the array suffers a decode error.
Returns {@link NC#FNIL} (nil) for each string which is null or empty.
*/
public static float[] toFloatArray(String[] strings)
{
  try { return maybeToFloatArray(strings); }
  catch (NumberFormatException e) { return null; }
}


/**
Decodes the contents of an array of strings to an array of doubles.

@param  strings  array of strings which contains double values.
@return          array of numbers represented in the strings.
Returns null if the input strings array is null.
Returns null if any string in the array suffers a decode error.
Returns {@link NC#DNIL} (nil) for each string which is null or empty.
*/
public static double[] toDoubleArray(String[] strings)
{
  try { return maybeToDoubleArray(strings); }
  catch (NumberFormatException e) { return null; }
}


/**
Decodes the contents of an array of strings to an array of booleans.
The boolean value will be true only if the string (ignoring case) is "true"
or "yes".

@param  strings  array of strings which contains boolean values.
@return          array of numbers represented in the strings.
Returns null if the input strings array is null.
Returns null if any string in the array suffers a decode error.
Returns true  for any trimmed string (ignoring case) with one of these values: TRUE YES T Y
Returns false for any trimmed string (ignoring case) with one of these values: FALSE NO F N
*/
public static boolean[] toBooleanArray(String[] strings)
{
  try { return maybeToBooleanArray(strings); }
  catch (NumberFormatException e) { return null; }
}


//--------------------- convert from number array to string array -----------------//
//--------------------- convert from number array to string array -----------------//
//--------------------- convert from number array to string array -----------------//


/**
Encodes an array of strings with an array of integer values.

@param  numbers  array of numbers to be converted to strings.
@return          array of encoded strings.
Returns null if the input numbers array is null.
Returns an empty string for each number which is {@link NC#INIL} (nil).
*/
public static String[] toStringArray(int[] numbers)
{
  if(numbers == null) return null;
  String[] strings = new String[numbers.length];
  for(int indx = 0; indx < numbers.length; indx++)
      {
      strings[indx] = toString(numbers[indx]);
      }
  return strings;
}


/**
Encodes an array of strings with an array of long integer values.

@param  numbers  array of numbers to be converted to strings.
@return          array of encoded strings.
Returns null if the input numbers array is null.
Returns an empty string for each number which is {@link NC#LNIL} (nil).
*/
public static String[] toStringArray(long[] numbers)
{
  if(numbers == null) return null;
  String[] strings = new String[numbers.length];
  for(int indx = 0; indx < numbers.length; indx++)
      {
      strings[indx] = toString(numbers[indx]);
      }
  return strings;
}


/**
Encodes an array of strings with an array of float values.

@param  numbers  array of numbers to be converted to strings.
@return          array of encoded strings.
Returns null if the input numbers array is null.
Returns an empty string for each number which is {@link NC#FNIL} (nil).
*/
public static String[] toStringArray(float[] numbers)
{
  if(numbers == null) return null;
  String[] strings = new String[numbers.length];
  for(int indx = 0; indx < numbers.length; indx++)
      {
      strings[indx] = toString(numbers[indx]);
      }
  return strings;
}


/**
Encodes an array of strings with an array of double values.

@param  numbers  array of numbers to be converted to strings.
@return          array of encoded strings.
Returns null if the input numbers array is null.
Returns an empty string for each number which is {@link NC#DNIL} (nil).
*/
public static String[] toStringArray(double[] numbers)
{
  if(numbers == null) return null;
  String[] strings = new String[numbers.length];
  for(int indx = 0; indx < numbers.length; indx++)
      {
      strings[indx] = toString(numbers[indx]);
      }
  return strings;
}


/**
Encodes an array of strings with an array of boolean values.

@param  numbers  array of numbers to be converted to strings.
@return          array of strings set to "YES" or "NO".
Returns null if the input numbers array is null.
*/
public static String[] toStringArray(boolean[] numbers)
{
  if(numbers == null) return null;
  String[] strings = new String[numbers.length];
  for(int indx = 0; indx < numbers.length; indx++)
      {
      strings[indx] = toString(numbers[indx]);
      }
  return strings;
}


//------------------------------ add backslash ---------------------------//
//------------------------------ add backslash ---------------------------//
//------------------------------ add backslash ---------------------------//


/**
Adds a backslash in front of certain special characters.
<pre>
The special characters are these:   / - * 
Also changes characters " to '
</pre>
@param  string   string which might contain special characters.
@return          a new string with the specified modifications.
*/
public static String addBackslash(String string)
{
  assert(string != null);

  char[] chars    = string.toCharArray();
  char[] newchars = new char [string.length() + 55];
  int count = 0;
  for(int indx = 0; indx < string.length(); indx++)
      {
      if(chars[indx] == '/')    // change / to \/
          {
          newchars[count] = '\\';
          count++;
          }
      else if(chars[indx] == '-')    // change - to \-
          {
          newchars[count] = '\\';
          count++;
          }
      else if(chars[indx] == '*')    // change * to \*
          {
          newchars[count] = '\\';
          count++;
          }
      else if(chars[indx] == '"')    // change " to '
          {
          chars[indx] = '\'';
          }
      newchars[count] = chars[indx];
      count++;
      }
  return new String(newchars,0,count);
}


//------------------------------ squeeze blanks --------------------------//
//------------------------------ squeeze blanks --------------------------//
//------------------------------ squeeze blanks --------------------------//


/**
Removes all space characters from a string.

@param  string   string which might contain space characters.
@return          a new string without any space characters.
*/
public static String squeezeBlanks(String string)
{
  return squeezeChar(string, ' ');
}


//------------------------------ squeeze char --------------------------//
//------------------------------ squeeze char --------------------------//
//------------------------------ squeeze char --------------------------//


/**
Removes all occurances of the specified character from a string.

@param  string     string which might contain the specified character.
@param  character  character to remove from string.
@return            a new string without any of the specified character.
*/
public static String squeezeChar(String string, char character)
{
  assert(string != null);

  char[] chars    = string.toCharArray();
  char[] newchars = new char [string.length()];
  int count = 0;
  for(int indx = 0; indx < string.length(); indx++)
      {
      if(chars[indx] != character)
          {
          newchars[count] = chars[indx];
          count++;
          }
      }
  return new String(newchars,0,count);
}


//------------------------- separate words -------------------------------//
//------------------------- separate words -------------------------------//
//------------------------- separate words -------------------------------//


/**
Add a space character before each capital letter except the first letter.

@param  string   string which might contain capital letters.
@return          a new string with inserted space characters.
Input string example:   HelloToYou3D
Output string example:  Hello To You3D
*/
public static String separateWords(String string)
{
  assert(string != null);

  if(string.equals(string.toUpperCase())) return string;

  char[] chars    = string.toCharArray();
  char[] newchars = new char [2 * string.length()];
  int count = 0;
  for(int indx = 0; indx < string.length(); indx++)
      {
      if(indx > 0 && Character.isLetter   (chars[indx])
                  && Character.isUpperCase(chars[indx])
                  && Character.isLetter   (chars[indx-1]))
          {
          newchars[count] = ' ';
          count++;
          }
      newchars[count] = chars[indx];
      count++;
      }
  return new String(newchars,0,count);
}


//------------------------ make pretty label ---------------------------//
//------------------------ make pretty label ---------------------------//
//------------------------ make pretty label ---------------------------//

public static String makePrettyLabel(String string, String label)
{
  if(label != null && label.trim().length() > 0
          && !label.trim().equals(string.trim())
          && !label.trim().equals(string.trim() + ":")) return label;
  return makePrettyLabel(string);
}

public static String makePrettyLabel(String string)
{
  if(string.contains("."))
      {
      int indx = string.lastIndexOf(".");
      string = string.substring(0, indx);
      }
  string = separateWords(string);
  string = string.replace(" ", "_");
  string = wordsToUpperCase(string);
  return separateWords(string).trim();
}

//------------------- get short capital name -----------------------------//
//------------------- get short capital name -----------------------------//
//------------------- get short capital name -----------------------------//


/**
Convert a class name to a short all-capital name.
For example:
Class name = org.cpseis.util.StringUtil
Returned name = STRING UTIL
*/
public static String getShortCapitalName(Object object)
{
  String classname = object.getClass().getName();
  int    index     = classname.lastIndexOf(".");
  String shortname = classname.substring(index+1);
  String separated = StringUtil.separateWords(shortname);
  return separated.toUpperCase();
}


//------------------------ words to upper case ---------------------------//
//------------------------ words to upper case ---------------------------//
//------------------------ words to upper case ---------------------------//


/**
Capitalize the first letter of each word in the string.
Subsequent letters in each word will be set to lower case.
The words should be separated by underscores (not spaces).
The underscores are removed.

@param  string   string which might contain underscore characters.
@return          a new string with capitalized words but no underscores.
Input string example:   heLLO_to_you
Output string example:  HelloToYou
*/
public static String wordsToUpperCase(String string)
{
  assert(string != null);

  char[] chars = string.toCharArray();
  for(int indx = 0; indx < string.length(); indx++)
      {
      if(indx == 0 || chars[indx-1] == '_')
          {
          chars[indx] = Character.toUpperCase(chars[indx]);
          }
      else
          {
          chars[indx] = Character.toLowerCase(chars[indx]);
          }
      }
  String newstring = new String(chars);
  return squeezeChar(newstring, '_');
}


//------------------------ words to lower case ---------------------------//
//------------------------ words to lower case ---------------------------//
//------------------------ words to lower case ---------------------------//


/**
Set the first letter of each word in the string to lower case.
Subsequent letters in each word will also be set to lower case.
Underscores will be added to separate the words.

@param  string   string which might contain underscore characters.
@return          a new string with capitalized words but no underscores.
Input string example:   HelloToYou
Output string example:  hello_to_you
*/
public static String wordsToLowerCase(String string)
{
  String newstring = separateWords(string);
/*
  return newstring.replace(" ", "_").toLowerCase();
*/
  return newstring.replace(' ', '_').toLowerCase();
}


//------------------------- remove backslash -------------------------------//
//------------------------- remove backslash -------------------------------//
//------------------------- remove backslash -------------------------------//


/**
Removes all backslash characters from a string.

@param  string   string which might contain backslash characters.
@return          a new string without any backslash characters.
*/
public static String removeBackslash(String string)
{
  assert(string != null);

  char[] chars    = string.toCharArray();
  char[] newchars = new char [string.length()];
  int count = 0;
  for(int indx = 0; indx < string.length(); indx++)
      {
      if(chars[indx] != '\\')
          {
          newchars[count] = chars[indx];
          count++;
          }
      }
  return new String(newchars,0,count);
}


//---------------------------- append spaces -------------------------------//
//---------------------------- append spaces -------------------------------//
//---------------------------- append spaces -------------------------------//


/**
Appends space characters to the end of a string.
Space characters are appended only if the input string is shorter than the
desired length of the output string.

@param  string   string which needs appending of space characters.
@param  length   desired length of output string.
@return          a new string with the appended space characters.
*/
public static String appendSpaces(String string, int length)
{
  assert(string != null);
  if(string.length() >= length) return string;

  char[] chars    = string.toCharArray();
  char[] newchars = new char [length];
  for(int indx = 0; indx < string.length(); indx++)
      {
      newchars[indx] = chars[indx];
      }
  for(int indx = string.length(); indx < length; indx++)
      {
      newchars[indx] = ' ';
      }
  return new String(newchars,0,length);
}


//---------------------------- prepend spaces -------------------------------//
//---------------------------- prepend spaces -------------------------------//
//---------------------------- prepend spaces -------------------------------//


/**
Prepends space characters to the beginning of a string.
Space characters are prepended only if the input string is shorter than the
desired length of the output string.

@param  string   string which needs prepending of space characters.
@param  length   desired length of output string.
@return          a new string with the prepended space characters.
*/
public static String prependSpaces(String string, int length)
{
  assert(string != null);
  if(string.length() >= length) return string;

  char[] chars    = string.toCharArray();
  char[] newchars = new char [length];
  for(int indx = 0; indx < length - string.length(); indx++)
      {
      newchars[indx] = ' ';
      }
  for(int indx = length - string.length(); indx < length; indx++)
      {
      newchars[indx] = chars[indx - length + string.length()];
      }
  return new String(newchars,0,length);
}


//---------------------------- center spaces -------------------------------//
//---------------------------- center spaces -------------------------------//
//---------------------------- center spaces -------------------------------//


/**
Adds space characters equally to the beginning and end of a string.
Space characters are added only if the input string is shorter than the
desired length of the output string.

@param  string   string which needs adding of space characters.
@param  length   desired length of output string.
@return          a new string with the added space characters.
*/
public static String centerSpaces(String string, int length)
{
  assert(string != null);
  if(string.length() >= length) return string;

  int length2 = string.length() + (length - string.length())/2;
  string = prependSpaces(string, length2);
  return appendSpaces(string, length);
}


//--------------------------- make exact length -------------------------//
//--------------------------- make exact length -------------------------//
//--------------------------- make exact length -------------------------//


public static String makeExactLength(String string, int length)
{
  if      (string.length() > length) string = string.substring(0, length);
  else if (string.length() < length) string = StringUtil.appendSpaces(string, length);
  return string;
}


//----------------------- get string from tokens ------------------------//
//----------------------- get string from tokens ------------------------//
//----------------------- get string from tokens ------------------------//


public static String getStringFromTokens(String[] tokens)
{
  String string = "";
  if(tokens == null) return string;
  for(int i = 0; i < tokens.length; i++)
      {
      String token = tokens[i].trim();
      if(token.length() == 0) token = " ";
      token = token.replace(' ', '^');
      if(i > 0) string += "  ";
/*
      token = token.replace("|", "||");
      if(i > 0 && i < tokens.length - 1) string += " | ";
*/
      string += token;
      }
  return string;
}


//----------------------- get tokens from string ------------------------//
//----------------------- get tokens from string ------------------------//
//----------------------- get tokens from string ------------------------//


public static String[] getTokensFromString(String string)
{
/*
  String[] tokens = string.split(" | ", 0);
  for(int i = 0; i < tokens.length; i++)
      {
      tokens[i] = tokens[i].replace("||", "|").trim();
      }
*/
  if(string == null) return new String[0];
  String[] temp = string.split(" ", 0);
  int kount = 0;
  for(int i = 0; i < temp.length; i++)
      {
      if(!temp[i].equals("")) kount++;
      }
  String[] tokens = new String [kount];
  kount = 0;
  for(int i = 0; i < temp.length; i++)
      {
      if(!temp[i].equals("")) tokens[kount++] = temp[i].replace('^', ' ');
      }
  return tokens;
}


//------------------------ remove empty strings --------------------------//
//------------------------ remove empty strings --------------------------//
//------------------------ remove empty strings --------------------------//


public static String[] removeEmptyStrings(String[] strings)
{
  int count = 0;
  for(int i = 0; i < strings.length; i++)
      {
      if(strings[i].length() > 0) count++;
      }
  String[] strings2 = new String[count];
  count = 0;
  for(int i = 0; i < strings.length; i++)
      {
      if(strings[i].length() > 0) strings2[count++] = strings[i];
      }
  return strings2;
}


//---------------------------- join with newline --------------------------//
//---------------------------- join with newline --------------------------//
//---------------------------- join with newline --------------------------//


public static String joinWithNewline (String... strings)
{
  return joinWithSeparator("\n", strings);
}


//---------------------------- join with separator --------------------------//
//---------------------------- join with separator --------------------------//
//---------------------------- join with separator --------------------------//


public static String joinWithSeparator (String separator, String... strings)
{
  if(strings.length == 0) return "";
  StringBuilder builder = new StringBuilder(strings[0]);
  for(int i = 1; i < strings.length; i++)
      {
      builder.append(separator);
      builder.append(strings[i]);
      }
  return builder.toString();
}


//---------------------------- join string arrays -------------------------//
//---------------------------- join string arrays -------------------------//
//---------------------------- join string arrays -------------------------//


public static String[] joinStringArrays (String[] strings1, String[] strings2)
{
  if(strings1.length == 0) return strings2;
  if(strings2.length == 0) return strings1;
  String[] strings3 = new String[strings1.length + strings2.length];
  System.arraycopy(strings1, 0, strings3, 0,               strings1.length);
  System.arraycopy(strings2, 0, strings3, strings1.length, strings2.length);
  return strings3;
}


//---------------------------- center string array ------------------------//
//---------------------------- center string array ------------------------//
//---------------------------- center string array ------------------------//


public static String[] centerStringArray (String... strings)
{
  int maxlength = 0;
  for(int i = 0; i < strings.length; i++)
      {
      if(strings[i].length() > maxlength) maxlength = strings[i].length();
      }
  String[] newstrings = new String[strings.length];
  for(int i = 0; i < strings.length; i++)
      {
      newstrings[i] = centerSpaces(strings[i], maxlength);
      }
  return newstrings;
}


//-------------------------------- main -------------------------------------//
//-------------------------------- main -------------------------------------//
//-------------------------------- main -------------------------------------//


/**
Modifies a string argument.
The operation is selected by specifying the first argument as the desired
function name preceded by a hyphen (e.g. -addBackslash).  The second argument
is the string to be modified.  In some cases a third or fourth argument is
needed.  Sometimes the first argument does not correspond to a function.
The result is printed to standard out.
The string "xxxx" is returned if an error occurs.

Command line options:
<pre>
      -removeBackslash    string
      -addBackslash       string
      -squeezeBlanks      string
      -separateWords      string     // first removes "Auto" if string starts with "Auto".
      -wordsToUpperCase   string
      -wordsToLowerCase   string
      -toUpperCase        string                    // uses the String toUpperCase method.
      -toLowerCase        string                    // uses the String toLowerCase method.
      -appendSpaces       string  length
      -replace            string  oldpart  newpart  // uses the String replace method.
</pre>

<p>
Examples:
<pre>
 To set a shell variable to another shell variable with no backslashes:
   set bbb = `java org.cpseis.util.StringUtil -removeBackslash "$aaa"`

 To set a shell variable to another shell variable with added backslashes:
   set bbb = `java org.cpseis.util.StringUtil -addBackslash "$aaa"`

 To write a file with the word "hello" (squeezed version of " he llo"):
   java org.cpseis.util.StringUtil -squeezeBlanks " he llo" > outfile

 To set a shell variable to another shell variable with enough appended spaces
 to make the output shell variable contain at least 20 characgters:
   set bbb = `java org.cpseis.util.StringUtil -appendSpaces "$aaa" 20`
</pre>

@param  args     two or more command line arguments (see above).
*/
public static void main(String[] args)
{
  String newstr = "arguments should be option and string";

  if(args.length >= 2)
      {
      String option = args[0];
      String string = args[1];
      if(option.equals("-removeBackslash"))
          {
          newstr = removeBackslash(string);
          }
      else if(option.equals("-addBackslash"))
          {
          newstr = addBackslash(string);
          }
      else if(option.equals("-squeezeBlanks"))
          {
          newstr = squeezeBlanks(string);
          }
      else if(option.equals("-separateWords"))
          {
          if(string.startsWith("Auto")) string = string.substring(4);
          newstr = separateWords(string);
          }
      else if(option.equals("-wordsToUpperCase"))
          {
          newstr = wordsToUpperCase(string);
          }
      else if(option.equals("-wordsToLowerCase"))
          {
          newstr = wordsToLowerCase(string);
          }
      else if(option.equals("-toUpperCase"))
          {
          newstr = string.toUpperCase();
          }
      else if(option.equals("-toLowerCase"))
          {
          newstr = string.toLowerCase();
          }
      else if(option.equals("-appendSpaces") && args.length >= 3)
          {
          int length = toInt(args[2]);
          newstr = appendSpaces(string, length);
          }
      else if(option.equals("-prependSpaces") && args.length >= 3)
          {
          int length = toInt(args[2]);
          newstr = prependSpaces(string, length);
          }
      else if(option.equals("-replace") && args.length >= 4)
          {
          String oldpart = args[2];
          String newpart = args[3];
          if(newpart.equals("blank")) newpart = " ";
/*
          newstr = string.replace(oldpart, newpart);
*/
          char oldchar = oldpart.charAt(0);
          char newchar = newpart.charAt(0);
          newstr = string.replace(oldchar, newchar);
          }
      }

  System.out.println(newstr);
}


//---------------------------- end of class ---------------------------------//
//---------------------------- end of class ---------------------------------//
//---------------------------- end of class ---------------------------------//

}

//-------------------------------- end -------------------------------------//
//-------------------------------- end -------------------------------------//
//-------------------------------- end -------------------------------------//

