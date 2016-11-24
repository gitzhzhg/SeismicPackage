function flag=isearthobj(object)

% ISEARTHOBJ performs a necessary but not sufficient test for an
% earth object by getting the objects type and comparing it versus
% the known types. Returns 1 is the type is known and zero otherwise
%
% G.F. Margrave November 1993
% Updated July 1997
%
% NOTE: ISEARTHOBJ will no longer function with objects created in
% version 4.x.
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

if( isstruct(object) )
   test=object.objtype;
   if( strcmp(test,'grid') )
	flag=1;
	return;
   elseif( strcmp(test,'rand') )
	flag=1;
	return;
   elseif( strcmp(test,'cont') )
	flag=1;
	return;
   else
	flag=0;
   end
else
   flag=0;
end