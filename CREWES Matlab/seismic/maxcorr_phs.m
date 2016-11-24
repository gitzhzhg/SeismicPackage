function [x,str]=maxcorr_phs(trace1,trace2,n)
% MAXCORR_PHS: measure max cc, lag, and phase rotation between two traces
% 
% [x,str]=maxcorr_phs(trace1,trace2,n)
%
% MAXCORR_PHS cross correlates two traces to determine the maximum cc (interpolated) and the
% lag at which it occurs. The best constant phase rotation is also estimated. The correlation
% analysis is done with maxcorr and the phase rotation is measured with constphase. No shift is
% applied prior to measuring the phase.
%
% See also: maxcorr_ephs and maxcorr
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
% x= output: x(1)-> interpolated maximum cross correlation 
%            x(2)-> interpolated lag (in samples) of maximum correlation
%            x(3)-> apparent constant phase rotation (degrees)
% str= a text string with the results useful plot titles and legends
% 
% Note: a negative result for x(2) indicates trace2 is delayed
%       relative to trace 1
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

cc=maxcorr(trace1,trace2,n);

phs=constphase(trace2,trace1);

x=[cc phs];

str=['ccmax= ' num2str(sigfig(x(1),2)) ', lag= ' num2str(x(2)) ', phs= ' int2str(x(3))];
