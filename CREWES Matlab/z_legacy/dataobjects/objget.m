function itemValue = objget( object, item, arg3)

% itemValue = objget(object, item)
% itemvalue = objget(object, item, datanum)
% itemvalue = objget(object, item, dataname)
%
% OBJGET performs the inverse operation to OBJSET in that it
% retrieves data from an EarthObject (created by RANDOBJ, GRIDOBJ)
% Most invocations will use the first syntax. Examples:
%	objget(myobject,'name') ... returns the object's name
%	objget(myobject,'x') ... returns the object's x coordinates
%   objget(myobject,'fred') ... returns the data field named fred
%	objget(myobject,5) ... returns the fifth dataset in the object
%
% The second syntax is provided to obtain fields specific to of a dataset 
% when only its name or number are known. For example, the name of the 
% 5th dataset is obtained with:
%   objget(myobject,'dataname',5) ... will return the name of the 5th data grid
% If the returned name is 'peace river' for example, then its last 
% modification date can be obtained by either of:
%   objget(myobject,'datamodified',5); ... 'peace river' is the 5th dataset
%   objget(myobject,'datamodified','peace river'); ... ask for it my name
%
% object = the EarthObject to be interrogated
% item = a string identifying the item to be retrieved, or an integer
%          denoting the index of the item. This last case is useful if 
%          a query such as objget( object, 'fieldnames') has first been
%          made to determine the names of available fields in the object.
%          Any field can then be obtained by simply specifying it's number
%          (the first is number 1 etc...)
% datanum = an integer referring to one of the data fields in the object. 
%           Valid numbers are 1 through m where m is the number of data 
%           fields stored.  
%           (and the number of rows returned by objget(myObject,'namesmatrix'))
% dataname = a string giving the name of a dataset stored in the object. 
%            Blanks are important: 
%            the string 'leduc' will not match ' leduc' or 'leduc '
%
% itemValue =  the returned value of the item. The form of itemValue
%     may be any of scalar, vector, or matrix depending on what
%     was stored.
%
% by G.F. Margrave, November 1993.
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

% determine the type of object and branch

type=object.objtype;

if( strcmp(type,'cont') )
	if( nargin==3 )
		itemValue = ocget(object, item, arg3);
	else
		itemValue = ocget(object, item);
	end
else
	if( nargin==3 )
		itemValue = oget(object, item, arg3);
	else
		itemValue = oget(object, item);
	end
end