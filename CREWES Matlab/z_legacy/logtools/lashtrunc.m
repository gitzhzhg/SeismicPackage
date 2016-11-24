function lash=lashtrunc(lash)
% lashout=lashtrunc(lash)
%
% LASHTRUNC resizes an LAS header so that contains no blank columns. The
% output header will have a number of columns such that at least one non-blank
% ascii character appears inthe last column. It also detects and removes
% any non-ascii characters in the header, replacing them with blanks
% lash ... string matrix containing the input LAS header
% lashout ... string matrix containing the output LAS header
%
%
% G.F. Margrave, Department of Geology and Geophysics,
%	University of Calgary, 1996
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
%search for non ascii
ind=find(abs(lash)<32)
if(~isempty(ind))
	lash(ind)=setstr(32*ones(size(ind)));
end
ind=find(abs(lash)>217)
if(~isempty(ind))
	lash(ind)=setstr(32*ones(size(ind)));
end
%search for blank columns
[nlines,m]=size(lash);
test=sum(abs(lash))/nlines;
ind=find(test~=32);
maxcol=max(ind);
if(maxcol<m)
	lash=lash(:,1:maxcol);
end
	
	