function svec=strmat2vec(smat)
% Given a string matrix with one (possibly padded) name per row,
% convert the matrix into an equivalent row vector with names
% separated by '|' and pads removed
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
[n,m]=size(smat);
svec=32*ones(1,m*n+n-1);
kvec=1;
for k=1:n
	name=smat(k,:);
	%remove 1's pad
	name=strunpad(name);
	%remove any blanks pad
	ind=find(abs(name)~=32);
	name=name(ind(1):ind(length(ind)));
	nk=length(name);
	if(k~=n)
		svec(kvec:kvec+nk-1)=name;
		svec(kvec+nk)='|';
		kvec=kvec+nk+1;
	else
		svec(kvec:kvec+nk-1)=name;
	end
end
svec=setstr(svec);