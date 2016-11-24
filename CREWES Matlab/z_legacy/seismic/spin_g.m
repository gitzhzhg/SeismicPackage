function trout=spin_g(trin,t,smooth_f,smooth_t,phase,delt,win_length)
% trout=spin_g(trin,t,smooth_f,smooth_t,phase,delt,win_length)
% trout=spin_g(trin,t,smooth_f,smooth_t,phase,delt)
% trout=spin_g(trin,t,smooth_f,smooth_t,phase)
% trout=spin_g(trin,t,smooth_f,smooth_t)
% trout=spin_g(trin,t,smooth_f)
% trout=spin_g(trin,t)
% 
% Time variant spectral inversion 'generic' method.
% Algorithm is described in: 
%               "A New Method of Time Variant Spectral Inversion"
%                 by G.F. Margrave, 1989 ICGC
%
% trin= input trace
% t= time coordinate for trin
% smooth_f= frequency smoother length in Hz
%   *********** default = 10 hz *************
% smooth_t= temporal smoother length in seconds
%   *********** default = .4 secs **********
% phase= phase flag, 0= zero phase, 1= minimum phase
%   *********** default = 0 ********** 
% delt= temporal increment (seconds) for computing the tvs of trin
% *************** default = .1 ************
% win_length= window length (seconds) for computing the tvs of trin
% ************** default = .3 *************
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
 if nargin < 7, win_length=.3; end
 if nargin < 6, delt=.1; end
 if nargin < 5, phase=0; end
 if nargin < 4, smooth_t=.4; end
 if nargin < 3, smooth_f=10; end
 
% compute the time variant amplitude spectrum of the input trace
 [tvs,trow,fcol]=tvspec(trin,t,delt,win_length);
 tvs=abs(tvs)+i*eps; % this peculiar statement computes the amplitude spectrum
%                    while keeping the tvs complex.
 nt=length(trow);nf=length(fcol);
% smooth the tvs in frequency and time using a 2-d convolve
% The time coordinate is constant for each row of the tvs and
% likewise, frequency is constant for each column.
% 
 df=fcol(2)-fcol(1);
 dt=trow(2)-trow(1);
 nfs=ceil(smooth_f/df);
 nts=ceil(smooth_t/dt);
 smooth_2d= ones(nts,nfs);
 tvs=conv2(tvs,smooth_2d);
% now extract the "central part" of the smoothed tvs
 nf2=round(nfs/2);nt2=round(nts/2);
 tvs=tvs(nt2:nt+nt2-1,nf2:nf+nf2-1); 
% now compute the minimum phase spectrum of each row
 if phase==1,
   tvs = -log(real(tvs))+i*eps;
 end
trout= tvs;