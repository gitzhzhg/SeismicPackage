function vf=velf(v,Q,f,f0)
%VELF: compute frequency-dependent phase velocity due to Q.
%
% vf=velf(v,Q,f0)
%
% Constant Q theory predicts that Q causes velocity to vary with frequency such that
% higher frequencies have facter velocities. Here we compute the this
% effect.
% 
% v ... vector of velocities
% Q ... either a sclar or a vector the same size as v
% f ... vector of frequencies to computed velocity at
% f0 ... reference frequency. This is the frequency at which v was
%        measured. If v is from well logging, then choose 12500 unless you
%        know the actual value for the sonic tool employed.
% *********** default 12500 Hz ************
%
% vf ... a length(v)-by-length(f) matrix of frequency-dependent velocities
%

if(nargin<4)
    f0=12500;
end
if(length(Q)>1)
    if(size(Q)~=size(v));
        error('Q and v must be the same size')
    end
end

%check for f=0
ind=find(f==0);
inot=find(f~=0);

vf=zeros(length(v),length(f));
if(~isempty(ind))
    vf(:,ind)=nan;
end

for k=inot
   vf(:,k)=v.*((1-(1./(pi*Q))*log(f(k)/f0))).^(-1); 
end