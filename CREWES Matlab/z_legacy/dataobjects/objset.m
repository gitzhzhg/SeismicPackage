function object=objset(objin,item,value,arg4)

% object = objset( objin, item, value);
% object = objset( objin, item, value, datanum);
% object = objset( objin, item, value, dataname);
%
% OBJSET accepts an EarthObject (created by RANDOBJ, GRIDOBJ)
% and sets the values of one of it's items. If the
% item already exists, then its values are replaced; otherwise
% a new item is added to the object and its values inserted.
% New items are assumed to be data (as opposed to headers) and must be
% the same size and geometry as other data items already in the object. The
% identity of header fields in the object is fixed so new header information
% cannot be added; however, the existing headers can be changed (updated) at
% any point. A data item may be protected from accidental overwrite by setting its
% protect flag to 'on'. The first form of the command is used most often
% and sets items that are not specific to a particular dataset within the 
% object. Examples: (let myObj be an existing object)
%    myObj = objset(myObj, 'username', 'barney'); ... change the username to barney
%    yourObj = objset(myObj,'username','barney'); ... as above but create a new object
%    myObj = objset(myObj,' Leduc ',newGrid); ... a grid referred to by the matlab
%		variable "newGrid" is put in the object and named ' Leduc '
%    myObj = objset(myObj,'protect','on',' Leduc '); ... the ' Leduc ' grid is 
%		protected.
%    myObj = objset(myObj,'protect','off',' Leduc '); ... the ' Leduc ' grid is 
%		un-protected.
%    myObj = objset(myObj,'protect','on',3); ... the third grid is un-protected.
%
% objin = the variable name of the object to be updated
% item = a string specifying the item to be set (max of 30 characters)
% value = the value(s) of the item
% datanum = the sequential number of one of the data fields in the object
% dataname = string giving the name of one of the data fields in the object
%
% object = the output object. Normally this will be the same matrix as objin
%
% For more information type: help earthobj
%
%
% by G.F. Margrave, November 1993
% Updated July 1997
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

% determine which type of object this is

type = oget(objin,'objtype');

% branch accordingly

if( strcmp(type,'cont') )
	if(nargin == 4)
		object=ocset(objin,item,value,arg4);
	else
		object=ocset(objin,item,value);
	end
else
	if(nargin == 4)
		object=oset(objin,item,value,arg4);
	else
		object=oset(objin,item,value);
	end
end