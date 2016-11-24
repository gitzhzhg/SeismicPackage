function namesnum=addnumname(names,newname)
% function namesnum=addnumname(names,newname)
%
% Given a string matrix of possibly numbered names (see NUMBERNAMES)
% addnumname adds a neww name to the matrix with a sequential number
% if needed
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
	l=length(newname);
	[n,c]=size(names);
	
	if(l>c-2)
		error(' new name too long ');
	end
	
	ind=strmatfind(names(:,1:c-2),newname);
	
	%add without index
	namesnum=[names; [newname blanks(c-l)] ];
	if(~isempty(ind))
		% determine index
		num=str2num(names(ind(length(ind)),c-1:c));
		if(isempty(num)) num=1; end
		num=num+1;
		snum=int2str(num);
		ls=length(snum);
		
		namesnum(n+1,c-ls+1:c)=snum;
	end