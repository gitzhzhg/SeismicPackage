function editlinesinit(anchorinfo)
% EDITLINESINIT: called to initiate editing with EDITLINES.
%
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
% editlinesinit(archorarray) 
%
% The argument to editlinesinit is called the anchor array and gives quite
% general control over editing. Its construction is described in the next
% paragraph. editlinesfini returns an updated anchor vector in the same
% format as the input but with any changes which the user has made. Thus it
% can be saved and supplied to editlinesinit to resume editing in the same
% state.
%
%STRUCTURE OF ANCHORSARRAY: Revision Jan 2015 was caused by the Matlab
%graphics engine update which elevated graphics handles from doubles to
%objects. Formerly the anchors vector had the format [h1 na1 xa11 ya11 xa12
%ya12 ... h2 na2 ...] where the h1, h2 etc are the handles of lines
%available to be edited, the na1, na2 etc are the numbers of anchors on
%each line, and the xa11,ya11,xa12,ya12 are the x and y coordinates for
%each anchor for each line. Additionally the graphics handles could be
%signed with a negative indicating the line was available for immediate
%editing. This worked when grapics handles were real numbers and could be
%stored in an ordinary vector. With the graphics update the anchors vector
%became the anchors array and its format is a 2D cell array with the
%structure {h1 na1 pts1; h2 na2 pts2; ...} where the h1, h2 etc are now
%handle objects, the na1, na2 are integers giving the number of anchors,
%and the pts1, pts2 are are na1-by-2 and na2-by-2 matrices giving the x and
%y coordinates of each anchor for each line (x in column1 y in column 2).
%The negative sign that was formerly attached to the graphics handle to
%indicate editing availability is now attached to the number of anchors.
%This presents a special problem when the number of anchors is 0 so the
%convention is adopted that the actual anchor number is abs(round(na)).
%With this convention, when the number of anchors is zero and the line is
%ready for editing, then na=-eps is the proper setting for the anchor
%number (eps is the built in Matlab constant for the machine precision,
%usually about 10^(-16). In this new format, the anchors array for two
%lines with no anchors would be anchors={hline1, -eps, [];hline2, -eps, []}.
%Similarly, if only line 1 is allowed to be edited then 
%anchors={hline1, -eps, [];hline2, 0, []}.
%
%Thus the number of lines is size(anchors,1), and 
%anchors(:,1) ... cell array of line handles 
%abs(round(anchors(:,2))) ... cell array of anchor numbers for each line 
%anchors(:,3) ... cell array of anchor points for each line
%
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