function smat=strmat(s1,s2)
% smat=strmat(s1,s2)
%
% STRMAT can be used to form string matricies. It differs from MATLAB's
% STR2MAT mainly in that it pads strings with 1's instead of blanks.
% This makes detection of the pad easier when the string itself may
% contain blanks. 1's are non-displaying ASCII characters.
% If s1 is a string matrix (each separate string is a row) with n rows,
% then smat will have n+1 rows and as many columns as the larger of
% s1 & s2.
% See also STRPAD STRUNPAD
%
% G.F. Margrave, March 1994
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
	[nrow,ncol]=size(s1);
	[n,m]=size(s2);
 if( n~=1 & m==1 ) s2=s2'; end
 % determine pad 
 if( m>ncol )
		s1=[s1 ones(nrow,m-ncol)];
	elseif( ncol>m )
		s2=[s2 ones(n,ncol-m)];
	end
	smat=[s1;s2];