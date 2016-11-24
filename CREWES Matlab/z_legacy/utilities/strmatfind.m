function ind=strmatfind(smat,s)
% ind=strmatfind(smat,s)
%
% STRMATFIND searches a string matrix, smat, for a row whos contents
% contain the same string as s. ind is returned as a vector of row
% numbers indicating which strings match.
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
ind=[];
% unpad s
s=strunpad(s);
s=deblank(s);
ii=find(s~=32);
if(isempty(ii))
	return;
end
s=s(ii(1):length(s));
[nrows,ncols]=size(smat);
%form smat into a big row vector with string boundary markers
smat=[smat '|'*ones(nrows,1)];
smat=smat';
smat=['|' smat(:)' '|'];
ii=find(smat==1);
if(~isempty(ii))
	smat(ii)=[];
end
ii=find(smat==0);
if(~isempty(ii))
	smat(ii)=[];
end
ibnd=find(smat=='|');
ii=findstr(smat,s);
%check each ii for full match
for k=1:length(ii)
	i=ii(k);
	ib=surround(ibnd,i);
	s1=smat(ibnd(ib)+1:ibnd(ib+1)-1);
	s1=deblank(s1);
	ik=find(s1~=32);
	s1=s1(ik(1):length(s1));
	%test
	if(strcmp(s1,s))
		ind=[ind ib];
	end
end