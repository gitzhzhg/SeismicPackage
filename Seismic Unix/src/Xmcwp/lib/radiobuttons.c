/*********************** self documentation **********************/
/******************************************************************************
RADIOBUTTONS -  convenience functions creating and using radio buttons

XtcwpCreateStringRadioButtons		create an XmFrame containing radio
						buttons labeled with strings

*******************************************************************************
Function Prototypes:
Widget XtcwpCreateStringRadioButtons (Widget parent, char *label,
	int nstrings, char **strings, int first,
	void (*callback)(int selected, void *clientdata), void *clientdata);

*******************************************************************************
Input:
parent		parent widget
label		label for this collection of radio bottons
nstrings	number of strings
strings		array[nstrings] of character strings, one per button
first		index of button to be initially selected
callback	function called when radio buttons change state
clientdata	pointer to client data to be passed to callback

******************************************************************************
Notes:
This code depends on the Motif Developer's Package.

An integer index of the selected button (along with the clientdata pointer)
is passed to the callback function.

The returned XmFrame is not managed.

*******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 08/28/90
******************************************************************************/
/**************** end self doc ********************************/

#include "Xmcwp/Xmcwp.h" 
/* #include "Xtcwp/Xtcwp.h"  */
/* #include <Xm/XmStrDefs.h> */
/* #include <Xm/Frame.h> */
/* #include <Xm/Form.h> */
/* #include <Xm/LabelG.h> */
#include <Xm/RowColumn.h>
#include <Xm/ToggleBG.h>

/* data structure used internally for toggle button callback */
typedef struct ButtonCBDataStruct {
	Widget *w;
	Widget wcurrent;
	void (*callback)(int selected, void *call_data);
	void *clientdata;
} ButtonCBData;

/* internal toggle button callback */
static void buttonCB (Widget w, ButtonCBData *bcbd, char * call_data);

Widget XtcwpCreateStringRadioButtons (Widget parent, char *label,
	int nstrings, char **strings, int first,
	void (*callback)(int selected, void *clientdata), void *clientdata)
/*****************************************************************************
create an XmFrame containing radio buttons labeled with strings
******************************************************************************
Input:
parent		parent widget
label		label for this collection of radio bottons
nstrings	number of strings
strings		array[nstrings] of character strings, one per button
first		index of button to be initially selected
callback	function called when radio buttons change state
clientdata	pointer to client data to be passed to callback
******************************************************************************
Notes:
An integer index of the selected button (along with the clientdata pointer)
is passed to the callback function.

The returned XmFrame is not managed.
******************************************************************************
Author:		Dave Hale, Colorado School of Mines, 08/19/90
*****************************************************************************/
{
	int i,n,labeled;
	XmString labels;
	Arg args[20];
	Widget framew,formw,labelw=NULL,radiow,togglew;
	ButtonCBData *bcbd;
	
	/* allocate space for toggle button callback data */
	bcbd = (ButtonCBData*)malloc(sizeof(ButtonCBData));
	bcbd->w = (Widget*)malloc(nstrings*sizeof(Widget));
	
	/* set callback function and data */
	bcbd->callback = callback;
	bcbd->clientdata = clientdata;
	
	/* frame (unmanaged) */
	n = 0;
	framew = XmCreateFrame(parent,"",args,n);
	
	/* form to manage geometry */
	n = 0;
	formw = XmCreateForm(framew,"",args,n);
	XtManageChild(formw);
	
	/* determine whether or collection is to be labeled */
	labeled = label!=NULL && label[0]!='\0';
	
	/* label */
	if (labeled) {
		n = 0;
		XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
		XtSetArg(args[n],XmNleftOffset,2); n++;
		XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
		XtSetArg(args[n],XmNtopOffset,2); n++;
		XtSetArg(args[n],XmNlabelType,XmSTRING); n++;
		labels = XmStringCreateLtoR(label,CHARSET);
		XtSetArg(args[n],XmNlabelString,labels); n++;
		labelw = XmCreateLabelGadget(formw,"",args,n);
		XtManageChild(labelw);
	}
	
	/* radio box */
	n = 0;
	XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNleftOffset,2); n++;
	if (labeled) {
		XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
		XtSetArg(args[n],XmNtopWidget,labelw); n++;
	} else {
		XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
		XtSetArg(args[n],XmNtopOffset,2); n++;
	}
	radiow = XmCreateRadioBox(formw,"",args,n);
	XtManageChild(radiow);
	
	/* toggle buttons */
	for (i=0; i<nstrings; ++i) {
		n = 0;
		labels = XmStringCreateLtoR(strings[i],CHARSET);
		XtSetArg(args[n],XmNlabelString,labels); n++;
		if (i==first) {XtSetArg(args[n],XmNset,True); n++;}
		togglew = XmCreateToggleButtonGadget(radiow,"",args,n);
		XtManageChild(togglew);
		XtAddCallback(
				(Widget) togglew,
				XmNvalueChangedCallback,
				(XtCallbackProc) buttonCB,
				(XtPointer) bcbd);
		bcbd->w[i] = togglew;
		if (i==first) bcbd->wcurrent = togglew;
	}
	
	/* return frame */
	return framew;
}

static void buttonCB (Widget w, ButtonCBData *bcbd, char * call_data)
{
	int i;
	
	/* if selected toggle button is same as current, simply return */
	if (w==bcbd->wcurrent) return;

	/* set current selected button */
	bcbd->wcurrent = w;
	
	/* determine index of selected toggle button */
	for (i=0; bcbd->w[i]!=w; ++i);
	if (i<0/*False*/) buttonCB(w,bcbd,call_data); /* keep compiler happy */
	
	/* call the user-specified callback */
	bcbd->callback(i,bcbd->clientdata);
}

#ifdef TEST
void testCB (int i, void *clientdata)
{
	printf ("selected %d\n",i);
}
main (int argc, char **argv)
{
	int nstrings=4;
	char *strings[]={"button 1","button 2","button 3","button 4"};
	Widget app,frame;

	app = XtInitialize(argv[0],"RadioButtons",NULL,0,&argc,argv);
	frame = XtcwpCreateStringRadioButtons(app,"Radio Buttons",
		nstrings,strings,0,testCB,NULL);
	XtManageChild(frame);
	XtRealizeWidget(app);
	XtMainLoop();
}
#endif
