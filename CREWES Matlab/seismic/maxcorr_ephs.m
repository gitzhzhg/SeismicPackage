function [x,str1,str2]=maxcorr_ephs(trace1,trace2,n)
% MAXCORR_EPHS: determine best shift and phase rotation between two traces
% 
% [x,str1,str2]=maxcorr_ephs(trace1,trace2,n)
%
% MAXCORR_EPHS cross correlates the envelopes of two given traces to find
% the maximum correlation and its lag. Then trace2 is shifted by the
% discovered lag and the best constant phase rotatation is estimated between
% trace1 and the shifted trace2. The value of the maximum correlation, its
% lag, and the phase rotation (in degrees) are returned as x(1) x(2) and x(3).
% The maximum correlation and lag after shifting and rotating are returned in x(4) and x(5).
%
% See also: maxcorr_phs and maxcorr
% 
% NOTE: to adjust trace2 to look like trace 1, do
%       trace2=phsrot(stat(trace2,t,dt*x(2)),x(3))
%
% trace1= input trace number 1
% trace2= input trace number 2
% NOTE: trace1 and trace2 should be the same length (required to determine
%       the phase rotation)
% n= 2*n +1 lags will be computed
% ******* default= round(length(trace1)/10) *********
%
% x= output: x(1)-> interpolated maximum cross correlation (between signals)
%            x(2)-> interpolated lag of maximum correlation (between envelopes)
%            x(3)-> best constant phase rotation (degrees) after shifting
%            x(4)-> max cc (between signals) after shift and rotation
%            x(5)-> lag of max cc after shift and rotation
% Note: a negative result for x(2) indicates trace2 is delayed
%       relative to trace 1
%
% str1= string giving values for x(4), x(2), and x(3) in a convenient fashion (after shift and rotation)
% str2= string giving values for x(1), x(2), and x(3) in a convenient fashion (before)
%
% by G.F. Margrave, Aug 2016
%

nsamps=length(trace1);
if(length(trace2)~=nsamps)
    error('traces must be the same length');
end
if(nargin<3)
    n=round(length(trace1)/10);
end

e1=env(trace1);
e2=env(trace2);
cc=maxcorr(trace1,trace2,n);
cce=maxcorr(e1,e2,n);

dt=.001;%pretend we are at 1 mil. It does not matter.
t=dt*(0:nsamps-1)';
trace2s=stat(trace2,t,dt*cce(2));

phs=constphase(trace2s,trace1);

trace2sr=phsrot(trace2s,phs);

cc2=maxcorr(trace1,trace2sr);

x=[cc(1) cce(2) phs cc2];

str1=['ccmax(a)= ' num2str(sigfig(x(4),2)) ', lag= ' num2str(x(2)) ', phs= ' int2str(x(3))];
str2=['ccmax(b)= ' num2str(sigfig(x(1),2)) ', lag= ' num2str(x(2)) ', phs= ' int2str(x(3))];
