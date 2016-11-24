function lash=lashtops(lash,topnames,ztops)
% lash=lashtops(lash,topnames,ztops)
%
% LASHTOPS searches an LAS header for a tops block and replaces it
% with one containing the topnames and tops given. It also searches
% for and eliminates multiple tops blocks (as were occaisionally
% created by logedit.)
%
% lash ... las header as a string matrix
% tops ... string matrix of tops names
% ztops ... vector of tops (numerical formation depths)
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
%first eliminate any tops blocks
[tblk,l1,l2]=lashgetblk(lash,'tops');
while(~isempty(tblk))
	lash(l1:l2,:)=[];
	[tblk,l1,l2]=lashgetblk(lash,'tops');
end
if(~isempty(ztops))
	%make new tops block
	n=length(ztops);
	stops=32*ones(n,20);
	lm=1;
	for k=1:length(ztops)
		s=sprintf('%g',ztops(k));
		l=length(s);
		stops(k,1:l)=s;
		if(l>lm) lm=l; end
	end
	stops=setstr(stops(:,1:lm));
	
	%make comment line
	ln=size(topnames,2);
	cmnt='#TOPS NAME          .        DEPTH:';
	if(ln<20)
		topnames=[topnames setstr(32*ones(n,20-ln-1))];
	end
		
	tblk=lashblkcreate('tops',cmnt,2,topnames,[],stops,[]);
	
	%insert
	lash=lashsetblk(lash,'tops',tblk);
end
	
	