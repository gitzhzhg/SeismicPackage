function val=fmget(fmat,action)
%
% val=fmget(fmat,action)
%
% FMGET is used to retrieve information from a flexi-matrix (see FMSET for a 
% definition. There are three possible values for action which result in three
% different return values:
%
% action = 'x' ... returns the x coordinates (positions of the columns) 
%                  of the matrix
% action = 'y' ... returns the y coordinates (positions of the rows) of 
%                  the matrix
% action = 'mat' ... returns the matrix
%
% Note that, due to the definition of a flexi-matrix, y values will be a 
% regular ordered set while the x values may be completely irregular 
% and in any order.  Also, the matrix will most likely contain a number of
% nan's in each column as these are used to pad the columns to equal length
%
% by G.F. Margrave
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

[nr,nc]=size(fmat);

if(nr*nc==0)
	val=[];
	return;
end

if(strcmp(action,'mat'))
	val=fmat(2:nr,2:nc);
	return;
end

if(strcmp(action,'x'))
	val=fmat(1,2:nc);
	return;
end

if(strcmp(action,'y'))
	val=fmat(2:nr,1);
	return;
end
	