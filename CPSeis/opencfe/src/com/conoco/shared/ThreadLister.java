///
/// ThreadLister.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  2.
///  1. 08-20-2002 SMCook   Original version.
///

/**
 * Class to help with thread analysis/debugging.
 */

package com.conoco.shared;

import java.io.PrintWriter;

public class ThreadLister {

  /**
   */
  public static final void listAllThreads(PrintWriter writer) {

    ThreadGroup parent = Thread.currentThread().getThreadGroup();
    ThreadGroup root   = parent;

    while(true) {
      parent = parent.getParent();
      if(parent != null)
        root = parent;
      else
        break;
    }

    printGroupInfo(writer, root);

    writer.flush();
  }

  /**
   */
  public static final void printGroupInfo(PrintWriter writer, ThreadGroup g) {
    if(g == null) return;
    int nThreads = g.activeCount();
    int nGroups  = g.activeGroupCount();
    Thread[]     threads = new Thread[nThreads];
    ThreadGroup[] groups = new ThreadGroup[nGroups];

    g.enumerate(threads, false);
    g.enumerate(groups, false);

    writer.println("Group: " + g.getName());

    for(int i = 0; i < nThreads; i++)
      printThreadInfo(writer, threads[i]);

    for(int i = 0; i < nGroups; i++)
      printGroupInfo(writer, groups[i]);  //recursive call
  }

  /**
   */
  public static final void printThreadInfo(PrintWriter writer, Thread t) {
    if(t == null) return;

    writer.println("  Thread: " + t.getName());
  }

  /**
   *
   */
  public static void main(String[] args) {
    listAllThreads(new PrintWriter(System.out));
  }
}
