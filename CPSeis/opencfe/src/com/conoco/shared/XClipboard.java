package com.conoco.shared;

/*
     originally from package util.nmeyers

     XClipboard.java:
     	Implement some X-specific methods to access X selections.

     Copyright (c) 1999 Nathan Meyers

     Permission is hereby granted, free of charge, to any person obtaining
     a copy of this software and associated documentation files (the
     "Software"), to deal in the Software without restriction, including
     without limitation the rights to use, copy, modify, merge, publish,
     distribute, sublicense, and/or sell copies of the Software, and to
     permit persons to whom the Software is furnished to do so, subject
     to the following conditions:

     The above copyright notice and this permission notice shall be
     included in all copies or substantial portions of the Software.

     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
     KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
     WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
     NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
     BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
     AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
     IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
     THE SOFTWARE.
*/

/**
 * Implements some X-specific methods for cut and paste.
 * <p></p>The X Window System has several mechanisms to support cut
 * and paste:
 * <dl>
 * <dt>CLIPBOARD selection</dt>
 * <dd>This is the mechanism used by applications that implement an
 * explicit cut and paste mechanism. It's used
 * by the java.awt.datatransfer.Clipboard methods.</dd>
 * <dt>PRIMARY selection</dt>
 * <dd>This is the mechanism used when you simply drag a selection without
 * cutting or copying. For example, when you drag a selection in one
 * terminal window and paste it into another with the middle mouse button,
 * you're using primary selection.</dd>
 * <dt>SECONDARY selection</dt>
 * <dd>An adjunct to the primary selection mechanism.</dd>
 * <dt>CUTBUFFER0</dt>
 * <dd>An outdated way of moving data around. Unlike selection, which
 * supports moving arbitrary data types through an active transfer
 * between programs, the cutbuffer statically stores text data in
 * properties on the root window. Tends to be used only by ancient
 * X clients.</dd>
 * </dl>
 * <p></p>This class provides some simple methods for extracting
 * text selection strings. It can be subclassed to support more
 * generality: the protected readSelection() method can support
 * arbitrary selection atom names and arbitrary target types.
 * More information than you could ever want to know about this can
 * be found in the XConvertSelection() man page and the ICCCM
 * specifications.
 *
 * @author Nathan Meyers
 */
public class XClipboard
{
    private static int errLevel;
    private static boolean successful=false;
    private static String origErrorMessage="";

    // Static constructor: get our library
    static {
	try
		{
		System.loadLibrary("XClipboard");
		successful=true;
		}
	catch(Throwable e)
		{
		successful=false;
		origErrorMessage=e.toString();
		}
    }
    /**
     * The class constructor opens a connection to the X server. Since
     * Java will not share its X connections, XClipboard needs one of
     * its own.
     */
    public XClipboard(int errLevel)
    {
	this.errLevel=errLevel;

	if(successful)
		{
		if(errLevel>0) System.err.println("XClipboard loadLibrary succeeded...");
		setErrorLevel(errLevel);
		}
	else
		{
		if(errLevel>0)
			{
			System.err.println("Warning: XClipboard loadLibrary failed...");
			System.err.println("Warning: Message was: " + origErrorMessage);
			}
		return;
		}

	if(!successful) return;

	SecurityManager security = System.getSecurityManager();
	if (security != null) security.checkSystemClipboardAccess();
	try
		{
	    	privateXData = openXConnection();
		}
	catch(Throwable e)
		{
		if(errLevel > 0) System.err.println("Warning: " + e);
		}
    }

    /**
     * Allow the outside world to see if loadLibrary was successful.
     */
    public final boolean wasSuccessful()
    {
	return successful;
    }

    /**
     * Shut down the X connection at finalization.
     */
    protected void finalize()
    {
    	closeXConnection(privateXData);
    }

    // Public methods for buffer access

    /**
     * Write to PrimarySelection (Style #1).
     */
    public int writePrimarySelection(String s)
    {
	return writePrimarySelection(s.getBytes());
    }
    /**
     * Write to PrimarySelection (Style #2).
     */
    public int writePrimarySelection(byte[] b)
    {
    	return writePrimarySelection(privateXData,b);
    }

    /**
     * Write to CutBuffer0 (Style #1).
     */
    public int writeCutBuffer0(String s)
    {
	return writeCutBuffer0(s.getBytes());
    }
    /**
     * Write to CutBuffer0 (Style #2).
     */
    public int writeCutBuffer0(byte[] b)
    {
    	return writeCutBuffer0(privateXData,b);
    }

    /**
     * Read the contents of CutBuffer0. This is an ancient X cut/paste
     * mechanism, and of very limited use.
     */
    public byte[] readCutBuffer0()
    {
    	return readCutBuffer0(privateXData);
    }

    /**
     * Request and read the current selection. The entire selection
     * is read and returned, rather than setting up an InputStream
     * mechanism to pump the bits as needed (which might be preferable
     * for big selections).
     * <p></p>This protected class is not the primary interface,
     * but supports adding generality not supported by the various
     * read* functions.<p></p>
     *
     * @param dp The private X data created by the class constructor.
     * @param selection The selection being requested - name of an X
     * atom. The usual cut/paste buffer is "PRIMARY". Also of interest:
     * "SECONDARY" and "CLIPBOARD".
     * @param target The target type - name of an X atom. "STRING" is
     * the most common.
     * @return a byte[], short[], or int[] with the selection value.
     * The type is determined by the type returned by the selection.
     */

    private synchronized native void setErrorLevel(int errLevel);

    private synchronized native byte[] openXConnection();
    private synchronized native long closeXConnection(byte[] dp);

    private synchronized native byte[] readCutBuffer0(byte[] dp);
    private synchronized native int 
	writeCutBuffer0      (byte[] dp,byte[] writeThis);

    protected synchronized native Object
    	readSelection (byte[] dp, byte[] selection, byte[] target);
    private synchronized native int
	writePrimarySelection(byte[] dp,byte[] writeThis);

    /**
     * Hold platform-specific data here: X window connection and
     * window ID.
     */
    protected byte[] privateXData;

    /**
     * Read the primary selection as a string.
     * If an application has some text selected, this will return it.
     */
    public String readPrimarySelectionString()
    {
	try
		{
		return new String((byte[])readSelection(privateXData,
		    	new String("PRIMARY").getBytes(),
			new String("STRING").getBytes()));
		}
	catch(Exception e)
		{
		return null;
		}
    }

    /**
     * Read the secondary selection as a string.
     * <i>The secondary selection is rarely used.</i>
     */
    public String readSecondarySelectionString()
    {
	byte[] result = (byte[])readSelection(privateXData,
	    	new String("SECONDARY").getBytes(),
		new String("STRING").getBytes());
    	return (result == null) ? null : new String(result);
    }

    /**
     * Read the clipboard selection as a string.
     * If an application has cut or copied some text,
     * this will return it. This duplicates, in a limited way, capabilities
     * in java.awt.datatransfer.Clipboard.
     */
    public String readClipboardSelectionString()
    {
	byte[] result = (byte[])readSelection(privateXData,
	    	new String("CLIPBOARD").getBytes(),
		new String("STRING").getBytes());
    	return (result == null) ? null : new String(result);
    }


    public void printAll()
	{
	printAll(this);
	}
    public static void printAll(XClipboard xc)
	{
	String selection;

	if(errLevel>4)System.out.println("printing p0");
	selection = xc.readPrimarySelectionString();
	if (selection != null)
	    System.out.println("Primary Selection (length = " +
			   selection.length() + "): " + selection);

	if(errLevel>4)System.out.println("printing p1");
	selection = xc.readSecondarySelectionString();
	if (selection != null)
	    System.out.println("Secondary Selection (length = " +
			   selection.length() + "): " + selection);

	if(errLevel>4)System.out.println("printing p2");
	selection = xc.readClipboardSelectionString();
	if (selection != null)
	    System.out.println("Clipboard Selection (length = " +
			   selection.length() + "): " + selection);

	if(errLevel>4)System.out.println("printing p3");
	selection = new String(xc.readCutBuffer0());
	if (selection != null)
	    System.out.println("CutBuffer0 (length = " +
			   selection.length() + "): " + selection);
	}

    /**
     * A simple test of the selection mechanisms. Call the four
     * public read* calls and report any non-null results.
     */
    public static void main(String[] argv)
	throws Exception
    {
    	XClipboard xc = new XClipboard(0);

	int iret;

	iret=xc.writeCutBuffer0      (new String("cut buf test!").getBytes());
	System.out.println("iret2=" + iret);

	iret=xc.writePrimarySelection(new String("pri buf test!").getBytes());
	System.out.println("iret1=" + iret);

	while(true)
		{
		printAll(xc);
		Thread.sleep(2000);
		}
    }

}
