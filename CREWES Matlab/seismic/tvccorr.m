function [tshift,ccmax,s1_shift]=tvccorr(s1,s2,t,twin,tinc,maxlag)
% tvccorr: estimates time-variant time shifts and crosscorrelation coefficients betweem two 1-D traces
% 
% [tshift,ccmax,s1_shift]=tvccorr(s1,s2,t,twin,tinc,maxlag)
%
% The trace s1 is windowed in time with a Gaussian function and then the
% the time shift to maximize the correlation between the windowed trace 
% and s2 is computed (see maxcorr). This process is repeated until all the
% specified times are analyzed. Their maximum crosscorrelation coefficient is
% computed in the same time-variant way between the time shifted trace s1
% and the trace s2 after being both windowed. Tvccorr is an acronym for 
% time-variant crosscorrelation. The algorithm is described in the 2015 
% CREWES report by Cui and Margrave, and the 2015 M.Sc. thesis by Cui. 
%
% s1 ... input trace to be time shifted
% s2 ... reference trace. Time shifts are w.r.t this trace. s1 and s2 
%        must have the same length
% t ... time coordinate for s1 and s2
% twin ... half-width of the Gaussian windows (sec)
% tinc ... temporal increment between windows (sec)
% maxlag ... 2*maxlag+1 lags will be searched
% ************* default = 100 ***********

% tshift ... estimated time-variant time shifts
% ccmax ... estimated time-variant maximum crosscorrelation coefficients  
% s1_shift ... trace s1 after being time shifted
%
% by T. Cui, 2015
%
% NOTE: It is illegal for you to use this software for a purpose other
% than non-profit education or research UNLESS you are employed by a CREWES
% Project sponsor. By using this software, you are agreeing to the terms
% detailed in this software's Matlab source file.
 
% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by 
% its author (identified above) and the CREWES Project.  The CREWES 
% project may be contacted via email at:  crewesinfo@crewes.org
% 
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) Use of this SOFTWARE by any for-profit commercial organization is
%    expressly forbidden unless said organization is a CREWES Project
%    Sponsor.
%
% 2) A CREWES Project sponsor may use this SOFTWARE under the terms of the 
%    CREWES Project Sponsorship agreement.
%
% 3) A student or employee of a non-profit educational institution may 
%    use this SOFTWARE subject to the following terms and conditions:
%    - this SOFTWARE is for teaching or research purposes only.
%    - this SOFTWARE may be distributed to other students or researchers 
%      provided that these license terms are included.
%    - reselling the SOFTWARE, or including it or any portion of it, in any
%      software that will be resold is expressly forbidden.
%    - transfering the SOFTWARE in any form to a commercial firm or any 
%      other for-profit organization is expressly forbidden.
%
% END TERMS OF USE LICENSE

if(nargin<6)
    maxlag=100;
end

s1=s1(:);
s2=s2(:);
t=t(:);
dt=t(2)-t(1);
% determine the number of windows, tinc will be adjusted to make the last window 
% precisely ended on tmax
tmax=max(t);
tmin=min(t);
nwin=round((tmax-tmin-twin)/tinc)+1; %number of windows
tinc=(tmax-tmin-twin)/(nwin-1); %reassign tinc
tout1=(0:nwin-1)*tinc+twin/2;
tout1=tout1(:);
lag=zeros(1,nwin);
ccmax=zeros(1,nwin);
% calculate time-variant time shifts
for k=1:nwin
    gwin=exp(-((t-tout1(k))/twin).^2);
    gwin=gwin(:);
    s1win=s1.*gwin;
    cc=maxcorr(s1win,s2,maxlag,1);%s1 windowed, s2 not
    lag(k)=-cc(2);
end
tshift=lag'*dt;
tshift=interp1(tout1,tshift,t,'linear','extrap');
s1_shift=drift_corr(s1,t,tshift);% time shift s1
% calculate time-variant maximum crosscorrelation coefficients 
for k=1:nwin
    gwin=exp(-((t-tout1(k))/twin).^2);
    gwin=gwin(:);
    s1_shiftwin=s1_shift.*gwin;% s1_shift and s2 both windowed
    s2win=s2.*gwin;
    ccmax(k)=ccorr(s1_shiftwin,s2win,0,1);
end
ccmax=interp1(tout1,ccmax,t,'linear','extrap');
end
