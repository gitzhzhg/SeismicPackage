/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/
#ifndef N_TREE_ERROR_HANDLER_HH
#define N_TREE_ERROR_HANDLER_HH

enum NTreeErrorCodes {

  NTN_SUCCESSFUL, NTN_MEMORY_ALLOCATION_ERROR

 ,T_SUCCESSFUL, T_MEMORY_ALLOCATION_ERROR, T_BAD_NODE_DATA

 ,H_SUCCESSFUL, H_MEMORY_ALLOCATION_ERROR, H_BAD_INPUTS,
  H_UNEXPECTED_RESULT

 ,NT_SUCCESSFUL, NT_MEMORY_ALLOCATION_ERROR, NT_UNEXPECTED_RESULT,
  NT_USER_ABORTED

};

#endif
