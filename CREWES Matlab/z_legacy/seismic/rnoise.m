function noise=rnoise(v,s_to_n,irange,flag)
% RNOISE ... create a random noise signal with a given s/n
%
% noise= rnoise(v,s_to_n,irange,flag)
% noise= rnoise(v,s_to_n,irange)
% noise= rnoise(v,s_to_n)
% noise= rnoise(v)
%
% returns a vector of zero mean pseudo random noise
% of the same dimensions as v.
%
% v= input signal vector used to determine the dimensions
%    of the noise vector and the rms signal level.
% s_to_n= desired signal to noise level (rms). Rnoise measures
%         rms power of v and sets the standard deviation of the 
%         noise as: noise_power = signal_power/s_to_n
%      ******* default = 2 *******
% irange= vector of indicies pointing to the range of v over
%         which the signal is to be measured. For example, '1:50'
%         would use the first 50 samples, or near(t,.72,.99) would
%         use the time zone from .72 to .99 (assuming t is the time
%         coordinate for v)
%      ********* default= 1:length(v) ********
% flag= 1 ... noise is normally distributed
%       0 ... noise is uniformly distributed
%      ******** default = 1 *******
%
% by G.F. Margrave, May 1991
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

%
 if nargin<4
   flag=1;
 end
 if nargin<3
   irange=1:length(v);
 end
 if nargin<2
   s_to_n=2;
 end
% 
 done=0;
 if flag==1
  done=1;
 end
 if flag==0
   done=1; 
 end
 if done==0
   error(' invalid flag');
 end
%
 c=clock;
 n=fix(10.*c(6));
 if flag == 1
    randn('seed',n);
 end
 if flag == 0
	rand('seed',n);
  end  
 noise=rand(size(v));
% adjust to zero mean
 noise=noise-mean(noise);

% measure signal and noise powers
 [m,n]=size(v);
 if m==1
   ps= sqrt(v(irange)*v(irange)');
   pn= sqrt(noise(irange)*noise(irange)');
   scalar= ps/(pn*s_to_n);
 end
 if n==1 
   ps= sqrt(v'*v);
   pn= sqrt(noise'*noise);
   scalar= ps/(pn*s_to_n);
 end
% adjust noise power
 noise=noise*scalar;


 