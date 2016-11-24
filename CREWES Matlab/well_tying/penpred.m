function pep=penpred(s,r,w,tw)
%
% pep=penpred(s,r,w,tw)
% 
% PEP = portion of energy predicted
%
% s ... seismic trace
% r ... reflectivity
% NOTE: s and r must be the same length
% w ... wavelet
% tw ... time coordinate for wavelet.(Needed to deterine time zero of the
%       wavelet)
% NOTE: If w and tw are not provided, then r is used directly as if it is already convolved.
%

if(length(s)~=length(r))
    error(' s and r must be the same length');
end

if(nargin>2)
    if(length(w)~=length(tw))
        error(' w and tw must be the same length');
    end
    izero=near(tw,0);
    s2=convz(r,w,izero(1),length(r),0);
else
    s2=r;
end

%s2b=balans(s2,s);
%s2b=s2;
[~,a]=lsqsubtract(s,s2);
s2b=a*s2;

pep=1-sum((s-s2b).^2)/sum(s.^2);





