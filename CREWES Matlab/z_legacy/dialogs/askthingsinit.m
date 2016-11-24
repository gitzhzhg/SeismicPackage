function askthingsinit(transfer,q,a,flags,titlestr,ttstr)
% DO NOT USE FOR NEW PROGRAMS - USE "askthingsle" instead.  MUCH EASIER!
%
% askthingsinit(transfer,q,a,flags,titlestr)
% askthingsinit(transfer,q,a,flags)
% askthingsinit(transfer,q,a)
% askthingsinit(transfer,q)
% ASKTHINGSINIT initiates a dialog which allows the user to answer any
% number of questions which can be answered with strings or popup menu
% selections.  No attempt is made to determine the legitmacy of the answers,
% that is up to the master program. The simplest form, which does not supply
% 'a', results in a dialog which displays each question in a static text
% uicontrol and places an edit text along side for the answer to be typed
% in. If a is supplied, then it must have the same number of rows as q. To get
% the same behavior as just described for any particular question, leave its
% row blank. To cause a popup menu to appear in place of the edit text field,
% put a string in the corresponding row of a which will be the label string of
% the popup. That is, if you wish question 3 to only have the possible answers
% Fred or Bill or Sam, then code the third row of a as 'Fred|Bill|Sam'. The
% popup will always appear with the first % string as the default answer.  In
% its simplest mode, the dialog will not dismiss until either all questions
% have a string answer or the user pushes the cancel button. (No check is made
% on popups since the user may be accepting the default.) This behavior can be
% modified by suppliying the flags matrix. Flags must have one entry for each
% question.  % as q. The entry is a boolean flag that is the answer to "Must
% this question be answered?" That is, if it is all ones then all questions
% are required to have answers. A zero for any question means it is
% optional. For popups, this flag is the index of the default choice in the
% list of alternatives.
% 
% transfer = a string matlab command to be evaluated with EVAL at the
%            completion of the dialog. Usually this will re-invoke the
%            calling program
%        q = a matrix of strings containing questions which the user
%            must answer. One question per row, any number of questions
%        a = a matrix of strings supplying alternatives for the possible
%            answers. If a is supplied, it must have the same number of
%            rows as q. If a row of a is blank then an edit text is supplied
%            to receive the answer. Otherwise the row should contain the
%            label string for a popup menu with the "|" symbol separating
%            alternatives
%	     **** default is a matrix of blanks ****
%    flags = one entry for each question. If a question is to be answered 
%            with an edit text then flag is a boolean for that question. 
%            If 1 then the dialog will not dismiss until the question
%            is answered. If 0, the question is optional. 
%            If the dialog is to be answered with a popupmenu, then flag
%            indicates the default setting of the menu. If flag is 5, then
%            the fifth choice will be shown in the popup and will be
%            selected if no action is taken.  Note that any entry for flag is
%            valid for edit text questions but, for popups, entries outside
%            of 1->n (n being the number of alternate choices) will result
%            in errors.
%	     **** default is all ones ****
% titlestr = title string for the dialog. 
%            **** default: 'Please supply this information:' ****
%
% ttstr    = tool tip string
%            a matrix of strings containing tool tips for each question
%            **** default: no tool tips ****
%
% IMPORTANT: To make the dialog stay as the top figure and insist on an
% answer, follow askthingsinit with the line:
% set(gcf,'windowstyle','modal')
%
% See ASKTHINGS for more info on how to use the dialog
% See ASKTHINGSFINI for a description of how to terminate the dialog
%
% G.F. Margrave November 1993
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
    if(iscell(q))
        nq=length(q);
    else
        [nq,c]=size(q);
    end
	
	if( nargin < 6)
		ttstr=32*ones(nq,1);
	end
	if( nargin < 5)
		titlestr=[];
	end
	if(nargin < 4)
		flags=ones(1,nq);
	end
	if(nargin < 3)
		a=32*ones(nq,1);
	end
	
	
	if(length(flags)~=nq)
		error(' there must be one flags per question');
    end
	
    if(iscell(a))
        na=length(a);
    else
        [ra,ca]=size(a);
    end
	
	if(ra~=nq)
		error(' a must have the same number rows as q');
    end
	
    if(iscell(ttstr))
        rt=length(ttstr);
    else
        [rt,ct]=size(ttstr);
    end
	
	if(rt~=nq)
		error(' ttstr must have the same number rows as q');
	end
	
	
	if(isempty(titlestr))
		dat=[nq c ca ct abs(q(:))' abs(a(:))' abs(ttstr(:))' flags(:)'  abs(transfer(:))'];
	else
		dat=[nq c ca ct abs(q(:))' abs(a(:))' abs(ttstr(:))' flags(:)' abs(transfer(:))' nan abs(titlestr)];
	end
	set(gca,'userdata',dat);
	
	askthings('init');