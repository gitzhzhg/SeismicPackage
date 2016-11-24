function block=lashblkcreate(bname,cmnts,irc,mnems,units,values,descs)
% block=lashblkcreate(bname,cmnts,irc,mnems,units,values,descs)
%
% LASHBLKCREATE creates an LAS logical data block in a string matrix
%
% bname ... string containing the block name (no ~, it is automatically
%				prepended
% cmnts ... string matrix containing any desired comments
% irc ... row numbers for the comments lines. Refers to rows in the
%		final block
% mnems ... string matrix of mnemonics
% units ... string matrix of units
% values ... string matrix of values
% descs ... string matrix of descriptions
%
% G.F. Margrave, Department of Geology and Geophysics,
%	University of Calgary, 1996
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
[rc,cc]=size(cmnts);
[rm,cm]=size(mnems);
[ru,cu]=size(units);
[rv,cv]=size(values);
[rd,cd]=size(descs);
if(strcmp(lower(bname),'tops'))
	sep1=' . ';
	sep2='   ';
	sep3=' : ';
else
	sep1='.';
	sep2='   ';
	sep3=' : ';
end
l1=length(sep1);
l2=length(sep2);
l3=length(sep3);
ltot=cm+cu+cv+cd+l1+l2+l3;
nlines=rc+rm+1;
ncols=max([length(bname)+1 cc ltot]);
block=setstr(32*ones(nlines,ncols));
tit=['~' upper(bname)];
block(1,1:length(tit))=tit;
lines=1:nlines;
lines(1)=nan;
for k=1:rc
	if(irc(k)>nlines)
		irc(k)=nlines-k+1;
	end
	block(irc(k),1:cc)=cmnts(k,:);
	lines(irc(k))=nan;
end
ind=find(isnan(lines));
lines(ind)=[];
sep1=ones(rm,1)*sep1;
sep2=ones(rm,1)*sep2;
sep3=ones(rm,1)*sep3;
block(lines,1:ltot) = [mnems sep1 units sep2 values sep3 descs];
	
 