/*

Matlab help:

  [status] = crhtmlhelp(command,parameter)

crhtmlhelp displays the Microsoft Window HTML Help viewer with the user-supplied help file.

 Input arguments:
  command ... help command: toc or topic
  parameter ... parameter for command
 Output arguments:
  None at present


 Examples:  
    The following example shows the syntax to display the table of contents for a compiled help file:
    crhtmlhelp('toc','Helpfile.chm');

        Where Helpfile.chm is the name of the compiled help file.

    To specify a topic within a compiled help file, use the following syntax:

    crhtmlhelp('topic','Helpfile.chm::/Topic.htm');

        Where Helpfile.chm is the name of the compiled help file, Topic.htm is
        the name of the HTML file that you want to open and Window name is the 
        name of the help window.

  Note that in most cases, you will want to give the full path to the helpfile.  For example:
    crhtmlhelp('topic','C:\Matlab6p1\toolbox\crewes\synth\synth.chm::/intro.htm');
        
To compile this code from within Matlab: 
 
    mex synthhelp.c 'C:\Program Files\Html Help Workshop\lib\htmlhelp.lib' 'C:\Program Files\Microsoft Visual Studio\VC98\Lib\advapi32.lib'
    
*/

#include <string.h>
#include <windows.h>
#include "mex.h"
#include "C:\\Program Files\\HTML Help Workshop\\include\\htmlhelp.h"


static char * hcbGetString(mxArray *prhs[], int arg) {
    int len;
    char *buf;
    
    if (mxIsChar(prhs[arg]) != 1) {
            mexErrMsgTxt("Input must be a string");
    }
    len = (mxGetM(prhs[arg]) * mxGetN(prhs[arg])) + 1;
    buf = mxCalloc(len, sizeof(char));
    mxGetString(prhs[arg], buf, len);
    return buf;
}

void mexFunction (int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    long status;

    char *fmtStr;
	char *command;
	char *topic;
    
    if (nrhs >= 2) {   
        command = hcbGetString(prhs, 0);
        if (strcmp(command,"toc")==0) {
            char *filename;
            filename = hcbGetString(prhs,1);
            HtmlHelp(0, filename, HH_DISPLAY_TOC, 0);            
            mxFree(filename);
        } else if (strcmp(command,"topic")==0) {
            char *topic;
            topic = hcbGetString(prhs,1);
            HtmlHelp(0, topic, HH_DISPLAY_TOPIC, 0);
            mxFree(topic);
        } else {
            mxErrMsgTxt("Unknown command %s : please use commands 'toc' or 'topic'",command);
        }
        mxFree(command);
    } else {
        mxErrMsgTxt("Please supply two arguments: command, parameter");
    }
}
