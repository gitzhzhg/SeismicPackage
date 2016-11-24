function editlinesinit(anchorinfo)
% EDITLINESINIT is called to initiate editing with EDITLINES.
% To use EDITLINES in a tool of your own crafting, it is usually necessary 
% to have the graphics handles of the lines to be edited if precise control
% over the editing process is to be maintained. Suppose that there are three
% (x,y) curves on display in the current axes of the current figure and let
% their handles be hline1, hline2, and hline3 and let their coordinates be
% given by vectors: x1,y1,x2,y2,x3,y3.  Then the following sequence
% initiates  editing, terminates editing, and retrieves the edited (x,y)'s:
%
% editlinesinit; .... Begin editing. All three curves are available for
%                     editing. Editing occurs for an indefinite period of time
% editlinesfini; .... Terminate the editing session
% x1=get(hline1,'xdata'); ... get line 1's x coodinates after editing
% y1=get(hline1,'ydata'); ... get line 1's y coodinates after editing
%
% By supplying an argument to editlinesinit, you can control which curves
% may be edited and if there are any initial anchor points on the curves.
% (See anchor points below). (Note that editlinesinit with no argument means
% everything is editable).  For example:
%
% editlinesinit([hline1 0 hline3 0]) ... line 2 may not be edited. Lines
%                                        1&3 have no anchor points
% editlinesinit([hline1 1 x1(10) y1(10) hline2 2 x2(5) y2(5) x2(15) ...
%               y2(15) hline3 0])
%	... this means all three lines may be edited. Line one has a single
%	    anchor at the point x1(10) y1(10); line2 has 2 anchors at the
%	    fifth and 15th points; while line 3 has no anchors.
%
% The argument to editlinesinit is called the anchor vector and gives
% quite general control over editing. editlinesfini returns an updated
% anchor vector in the same format as the input but with any changes which
% the user has made. Thus it can be saved and supplied to editlinesinit to
% resume editing in the same state. 
%
% There are also several different modes that editlines can be run in.
% Modes are activated at anytime in a editing session by an apporpriate 
% call to editlines:
%
% editlines('xonly') ... changes cannot be made to the y coordinates of 
%                        any point.
% editlines('yonly') ... changes cannot be made to the x coordinates of 
%                        any point.
% editlines('xandy') ... frees both x and y for changes
% editlines('linkmode') ... toggles between link mode and normal mode
% editlines('dragmode') ... toggles between constant drag and elastic drag
% editlines('constantdrag') ... sets drag mode to constant
% editlines('elasticdrag') ... sets drag mode to elastic
%
% Note that these calls are on-off toggles. That is if editing is already 
% in 'xonly' mode then: editlines('xonly') will free up the x coordinates 
% for editing.   Also, if both 'xonly' and 'yonly' are on then no changes 
% can be made.
%
% Lastly, editlines('undo') will undo changes made since the last mouse down.
%
% G.F. Margrave December 1993
% 
% NOTE: This SOFTWARE may be used by any individual or corporation for any purpose
% with the exception of re-selling or re-distributing the SOFTWARE.
% By using this software, you are agreeing to the terms detailed in this software's
% Matlab source file.

% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by
% its 'AUTHOR' (identified above) and the CREWES Project.  The CREWES
% project may be contacted via email at:  crewesinfo@crewes.org
%
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) This SOFTWARE may be used by any individual or corporation for any purpose
%    with the exception of re-selling or re-distributing the SOFTWARE.
%
% 2) The AUTHOR and CREWES must be acknowledged in any resulting publications or
%    presentations
%
% 3) This SOFTWARE is provided "as is" with no warranty of any kind
%    either expressed or implied. CREWES makes no warranties or representation
%    as to its accuracy, completeness, or fitness for any purpose. CREWES
%    is under no obligation to provide support of any kind for this SOFTWARE.
%
% 4) CREWES periodically adds, changes, improves or updates this SOFTWARE without
%    notice. New versions will be made available at www.crewes.org .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE
	set(gcf,'windowbuttondownfcn','editlines(''buttondown'')');
	set(gcf,'windowbuttonupfcn','');
	set(gcf,'windowbuttonmotionfcn','');
	
	if(nargin>0)
		set(gca,'userdata',anchorinfo);
	else
		set(gca,'userdata',[]);
	end
	
	editlines('init');