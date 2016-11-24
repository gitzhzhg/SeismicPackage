function [names,newname]=repnumname(names,oldname,newname)
%
% [names,newname]=addnumname(names,oldname,newname)
%
% Given a string matrix of possibly numbered names (see NUMBERNAMES)
% repnumname finds an old numbered name and replaces it with a new name 
% which is numbered as needed
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
	ln=length(newname);
	lo=length(oldname);
	[n,c]=size(names);
	
	if(ln>c-2)
		error(' new name too long ');
	end
	
	%find the old name
	indold=strmatfind(names,oldname);
	
	
	%find any names matching the newname
	ind=strmatfind(names(:,1:c-2),newname);
	
	%replace without index
	newname=[newname blanks(c-ln)];
	names(indold,:)=newname;
	
	if(~isempty(ind))
		num=-1;
		for k=ind
			% determine index
			anum=str2num(names(k,c-1:c));
			if(isempty(anum)) anum=1; end
			if(anum>num) num=anum; end
			
		end
		num=num+1;
		snum=int2str(num);
		ls=length(snum);
		
		names(indold,c-ls+1:c)=snum;
		newname(c-ls+1:c)=snum;
	end