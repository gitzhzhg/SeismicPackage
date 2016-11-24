function sout=nmor_srm(s,t,x,v,n,dir,params)
% NMOR_SRM: forward or reverse nmo for surface related multiples
%
% sout=nmor_srm(s,t,x,v,n,dir,params)
%
% NMOR does forward or reverse normal moveout removal using sinc function
% interpolation. Either single traces or gathers are accommodated.
%
% s	= input seismic matrix, one trace per column.
% t	= time coordinates for s, one entry per row of s
% x = offset coordinates for s, one entry per column of s
% v	= moveout velocities, same size as t. A single constant is also
%       allowed.
% n = order of the multiple. The moveout equation used is
%       tx=sqrt(((n+1)*t0)^2+x^2/v^2). Where t0 and v are the zero offset
%       time ans the stacking velocity of the primary and n counts the
%       number of times the multiple bounces off the free surface. n=0 is
%       the primary.
% dir	= 1 remove normal moveout
%	= -1 unremove normal moveout
% **************** Default = 1 **************************
% params(1) ... maximum allowed moveout stretch (percent)
% **************** Default = 30% ************************
% params(2) ... percent taper used in the stretch mute
% **************** Default = 10% ************************
% params(3) ... 0 for linear taper, 1 for raised cosine taper
% **************** default = 0 **************************
% NOTE: you can give a value of nan for a param and that will trigger the
% default
%
% sout= normal moveout corrected trace
%
% G.F. Margrave, CREWES Project, 1995, 2004
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

if(nargin<7)
	params=nan*ones(1,3);
end
if(nargin<6)
	dir=1;
end
if(isnan(params(1)))
    smax=20;
else
    smax=params(1);
end
if(isnan(params(2)))
    taperpct=10;
else
    taperpct=params(2);
end
if(isnan(params(3)))
    tapertype=0;
else
    tapertype=params(3);
end
[nr,nc]=size(s);
flip=0;
if(nr==1)
    s=s';
    flip=1;
    [nr,nc]=size(s);
end

if(length(t)~=nr);
    error('t vector is the wrong size')
end
if(size(t,1)==1)
    t=t';
end
    

if(length(v)==1)
    %allow for v=constant
    v=v*ones(size(t));
end

%test size(v)
nrv=size(v,1);
if(nrv~=nr)
    if(nrv==nc)
        %velocity may be transposed relative to s
        if(length(v)==length(s))
            v=v';
        end
    else
        error('velocity vector must be the same size as s')
    end
end
%test size(t)
nrt=size(t,1);
if(nrt~=nr)
    if(nrt==nc)
        %t may be transposed relative to s
        if(length(t)==length(s))
            t=t';
        end
    else
        error('velocity vector must be the same size as s')
    end
end

trflag=0;
if(nr==1) %handle row vector input
    if(nc==1)
        disp('Only one sample in seismic matrix. Forget it')
        return;
    else
        trflag=1;
        s=s.';
        nc=size(s,2);
    end
end

sout=zeros(size(s));

for k=1:nc %loop over traces
 
	if(dir==1) %remove moveout
	
			%compute the offset times, here t is t0
			tx=sqrt( (x(k)./v).^2 + ((n+1)*t).^2 );

			%remove nmo with sinc interpolation

			ind=between(t(1),t(end),tx,2);
			if(ind == 0) % this means we have not recorded data to NMO
				continue;
			end
		
			sout(ind,k)=sinci(s(:,k),t,tx(ind))';
            
%             %stretch mute
%             sm=(tx-t)./(t+100*eps);
%             ind2=find(sm>(smax/100));
%             if(~isempty(ind))
%                 %stretch mute taper
%                 n=length(ind2);
%                 nt=round(.01*taperpct*n);%length of taper
%                 itaper=n-nt+1:n;
%                 if(length(itaper)>1)
%                     if(tapertype==1)
%                         mask=.5+.5*cos(pi*itaper/nt-pi*n/nt);
%                     else
%                         mask=itaper/nt+1-n/nt;
%                     end
%                     sout(itaper,k)=sout(itaper,k).*mask(:);
%                     sout(1:itaper(1)-1,k)=0;
%                 else
%                     sout(ind2,k)=0;
%                 end
%             end
	
	elseif(dir==-1)
        
%         if(x(k)==0)
%             sout(:,k)=s(:,k);
%         else
            % here t is tx
            %construct tx,to relation
            tx=sqrt((n+1)^2*t.^2+(x(k)./v).^2);
            %now, interpolate off this curve to get the to's that map to
            %regular tx's
            t0=interpextrap(tx,t,t);
%             test=t.^2 - (x(k)./v).^2;
            ind=between(t(1),t(end),t0);
%             to=-1*ones(size(t));
%             to(ind)=sqrt( test(ind) )/(n+1);
%             
%             ind=between(t(1),t(length(t)),to);
            if(ind(1)>0)
                sout(ind,k)=sinci(s(:,k),t,t0(ind))';
            end
%         end
    else
        error('invalid direction')
		
	end

end

if(trflag); sout=sout.'; end