function [delay]=delayt(fbtime,fbcoord,cvpavg,v2rec,shotcoord,nshots,recelev,slim,elim,plust)
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

delay=NaN*ones(nshots,length(recelev));
for n=1:nshots
    ind=find(plust(1,:)==shotcoord(n));
    ts=plust(2,ind)/2;
    if (length(ts)~=0)
	  validn = find(~isnan(fbtime(n,:)));
	  indl = find(fbcoord(n,validn)<slim & fbcoord(n,validn)<cvpavg(n,1));
	  indr = find(fbcoord(n,validn)>elim & fbcoord(n,validn)>cvpavg(n,2));
	if (length(indl)>0)
	    for h=indl
 	      indcoord(h)=find(recelev(1,:)==fbcoord(n,validn(h)));
 	      delay(n,indcoord(h)) = fbtime(n,validn(h))-ts-(shotcoord(n)-fbcoord(n,validn(h)))/v2rec(1,indcoord(h));
	    end
	end
	if (length(indr)>0)
	    for f=indr
 	      indcoord(f)=find(recelev(1,:)==fbcoord(n,validn(f)));
 	      delay(n,indcoord(f)) = fbtime(n,validn(f))-ts-(fbcoord(n,validn(f))-shotcoord(n))/v2rec(1,indcoord(f));
	    end
	end
    end
end