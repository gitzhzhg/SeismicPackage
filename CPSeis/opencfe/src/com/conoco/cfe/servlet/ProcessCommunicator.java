package com.conoco.cfe.servlet;

import java.io.EOFException;
import java.io.InputStream;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;

public class ProcessCommunicator implements Runnable {
  public static final int SUCCESS = 0;
  public static final int LOGIN_FAILED = 1;
  public static final int TRANSPORT_ERROR = 2;
  public static final int LOGIN_NOT_FOUND = 3;
  public static final int PASSWORD_NOT_FOUND = 4;
  public static final int PROCESS_EXITED = 5;
    
  private boolean _active = false;
  private Process _process;
  private Thread  _waitThread;
  private boolean _readStarted;

  private boolean _processExited = false;
  private int    _exitCode = 0;
  
  private OutputStream      _processInput;
  private BufferedInputStream _processOutput;

  private byte [] _buffer=new byte[256];
    private int     _bufferCount=0;
  
    public ProcessCommunicator() {
    }
    
    
    public void startProcess(String command) {

      try {
        _readStarted = false;
        _active      = true;
        _process     = Runtime.getRuntime().exec(command);
        
        _waitThread  = new Thread(this);
        _waitThread.start();
        
        _processOutput = new BufferedInputStream(_process.getInputStream());
        _processInput  = _process.getOutputStream();
      }
      catch (IOException e) {
        _active = false;
      }
    }
    


  private String readUntil() throws EOFException {
    int oldBufferCount = _bufferCount;

    for (;;) {
      try {
        while (_processOutput.available() > 0) {
          int ch = _processOutput.read();

            _readStarted = true;

          if (ch == -1) throw new EOFException("unexpected eof");

                if (ch == 10) {
            String s = new String(_buffer, 0, _bufferCount);
            _bufferCount = 0;
            return s;
          }

          _buffer[_bufferCount++] = (byte) ch;
        }
      }
      catch (IOException e) {
        throw new EOFException("unexpected ioexception");
      }

      if (_bufferCount != oldBufferCount) break;

      try {
        Thread.currentThread().sleep(500);
      }
      catch (InterruptedException e) {
        System.out.println("Interrupted sleep");
      }
    }

    return new String(_buffer, 0, _bufferCount);
  }
    
    
    public int waitFor(String [] tokens) throws IllegalStateException, EOFException {
      if (!_active) throw new IllegalStateException("Process is not active");
  
      try {
        while (_active) {
          char c;
            
          String line = readUntil();
          
          for (int i=0; i < tokens.length; i++) {
            String token = tokens[i];
              
            if (line.indexOf(token) != -1) {
              return i;
            }
          }
        }
        
      }
      catch (IOException e) { 
        System.out.println("Process failed to start");
        System.out.println(e);
      }
      
      return -1;
    }
    
    public void send(String s) throws IllegalStateException {
      if (!_active) throw new IllegalStateException("Process is not active");
      
    s = s + '\n';
    try {
        _processInput.write(s.getBytes());
        _processInput.flush();
    }
    catch (IOException e) { 
      System.out.println("Error on output");
      e.printStackTrace();
    }
    }
    
    
    public void complete() {
      _waitThread.stop();
      _waitThread = null;
      
      _active = false;
      _processInput  = null;
      _processOutput = null;
      //_process.destroy();
      //_process = null;
    }
    
    public void run() {
      try {
        _process.waitFor();
      _processExited = true;
      }
      catch (InterruptedException e) {
        System.out.println("Waitfor interrupted");
      }
      
    _active = false;
    
    }
    
    public boolean isActive() {
      return _active;
    }

  public boolean isProcessExited() {
    return _processExited;
  }
    
    public static int runCommand(String host, String userName, String password, String command) {

      ProcessCommunicator test = new ProcessCommunicator();
      test.startProcess("telnet " + host);
      String [] loginPrompt = { "login:" };
    int returnStatus = SUCCESS;
    
      try {
        int foundLogin = test.waitFor(loginPrompt);
        
        if (foundLogin == 0) {
          test.send(userName);
          
          String [] passwordMatch = { "Password:" };
          
          int foundPassword = test.waitFor(passwordMatch);
               

          if (foundPassword == 0) {
            test.send(password);
            
            String [] loginStatus = { "Last login", "Login incorrect" };
            int foundSuccess = test.waitFor(loginStatus);
            
            if (foundSuccess == 0) {
            test.send(command);
            returnStatus = SUCCESS; // Successful
            }
            else {
              returnStatus = LOGIN_FAILED;
            }
          }
          else {
            returnStatus = PASSWORD_NOT_FOUND;
          }
        }
        else {
          returnStatus = LOGIN_NOT_FOUND;
        }
      }
      catch (EOFException eof) {
      returnStatus = TRANSPORT_ERROR;
      }
      catch (IllegalStateException ise) {
      returnStatus = TRANSPORT_ERROR;
      }
      finally {
      if (test.isProcessExited()) {
        returnStatus = PROCESS_EXITED;
      }

        test.complete();
      }

    return returnStatus;
    }
}