function [Qint,tqint]=qave2qint(Qave,tq,tnot,tqin)
%
% [Qint,tqint]=qave2qint(Qave,tq,tnot,tqin)
%
% Convert average Q to interval Q
% 
% Qave ... vector of average Q's
% tq ... vector of times, the same length as Qave.
% tnot ... reference time to which the Qave's are measured from
% *********** default tnot=0 **********
% Note: tnot must be less than tq(1)
% tqin ... vector of times at which interval Qs are desired. The
%       more finely sampled this is, the more likely that your interval Qs
%       will be garbage. Generally, with measured Qave you must experiment
%       with tqin until you get a good result. The default is likely too
%       finely sampled. If you specify tqin, then all of its values must
%       lie between the beginning and end of tq.
% ************** default tqin = tq ***********
%
% Note: When tqin~=tq, then a new set of Qave values in interpolated from
% the input values at the times tqin using 1D spline interpolation. These
% interpolated values are used in the computation described below.
% 
% Qint ... vector of interval Q's the same length as Qave
% tqint ... vector of times of length one longer than Qint and equal to
%       [tnot;tqin(:)]; That is it is just the same as tqin but with tnot
%       tacked on the front. See qint2qave for an explanation.
%
% The Qave(1) is assumed to be the Q at the bottom of the first layer of a
% stack of layers. While Qave(2) is the Q at the bottom of the second
% layer. The Qint(1) will always equal Qave(1). Qint(2) is given by
% (tq(2)-tq(1))/Qint(2) = (tq(2)-tnot)/Qave(2) - (tq(1)-tnot)/Qave(1). And
% so forth for the remaining layers. Because the calculation of interval Q
% involves a difference of adjacent average Q's, it is possible for
% interval Q's to become negative which is unphysical. This happens most
% commonly when the average Q's are measured from noisy data and they are
% closely spaced in time. Such values are flagged and set to pi. (Setting
% them to zero asks for trouble later in a zero divide.)
% 

tq=tq(:);
Qave=Qave(:);

if(nargin<4)
    tqin=tq;
end
if(nargin<3)
    tnot=0;
end

tqm=max(tq);
if(any(tqin>tqm))
    error('tqin values must be less than max(tq)');
end
tqm=min(tq);
if(any(tqin<tqm))
    error('tqin values must be greater than min(tq)')
end

%interpolate qave values at the locations tqin
Qa=interp1(tq,Qave,tqin,'spline');

Qint=zeros(size(Qa));
Qint(1)=Qa(1);

term=(tqin(2:end)-tnot)./Qa(2:end) - (tqin(1:end-1)-tnot)./Qa(1:end-1);

Qint(2:end)=(tqin(2:end)-tqin(1:end-1))./term;

ind= Qint<0;
if(~isempty(Qint))
    Qint(ind)=pi;
end

tqint=[tnot;tqin];