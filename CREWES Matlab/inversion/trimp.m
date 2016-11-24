function [imp,I0,b]=trimp(trin,impwell,t,n)
% TRIMP ... impedance inversion with trend correction from well control
%
%  imp=trimp(trin,impwell,t,n)
%
% TRIMP estimates acoustic impedence from a seismic trace using a well log
% to provide the background trend. The algorithm has three steps: (1) A
% least-squares problem is solved to determine the best starting impedance
% and scalar for the sesimic data.  These two values provide minimum L2
% error between the log-well-impedance and the integrated-scaled
% seismic trace. (2) Then the seismic data is scaled and run through
% rcs2imp1 to compute a first impedance estimate where the trend is
% determined entirely by the seismic data. (3) Finally a low-order
% polynomial is fit to both the well impedance and the impedance computed
% in part 2. The trend of the seismic impedance is removed and replaced
% with that of the well. Prior to this computation, it is assumed that the
% seismic trace has been fully deconvolved (i.e. the residual wavelet is a
% bandlimited spike) and aligned (time shifted) to tie the well.
%	
% trin ... input seismic trace (in time)
% impwell ... input impedance from a well (in time)
% t ... time coordinate vector for trin
% REQUIREMENT: trin,impwell, and t must all have the same length.
% NOTE: While this is written for time input, all three above arguments
% could equally well be in depth.
%
% n ... order of the polynomial fit used in trend replacement. A value of 0
%       means no trend replacement, while 1 means the trend is determined
%       as linear. Should be an integer greater than or equal to 0 and less
%       than or equal to 10.
% *********** default = 1 ************
%
% imp ... vector of the same length as impwell containing the estimated
%       impedance
% I0 ... starting impedance determined by least squares
% b  ... trace scalar determined by least squares. The first impedance
%       estimate (step 2) is made by rcs2imp1(trin*b,I0);
%
% G.F. Margrave, CREWES, U of Calgary, 2016
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

if(nargin<4)
    n=1;
end
if(n<0 || n>10)
    error(' n must be in the set [0, 1, 2, 3, ....10]')
end
if(floor(n)~=n)
    error('n must be an integar')
end

nsamps=length(trin);
if(length(impwell)~=nsamps || length(t)~= nsamps)
    error('trin, impwell, and t must all have the same length')
end

trint=cumsum(trin);%integrated trace
y=log(impwell);%log impedance
p=polyfit(trint,y,1);
b=p(1)/2;
I0=exp(p(2));

imp=rcs2imp1(trin(1:end-1)*b,I0);

if(n>0)
    imp=imp-trend(imp,t,n)+trend(impwell,t,n);
end
