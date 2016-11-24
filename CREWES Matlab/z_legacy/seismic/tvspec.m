function [tvs,trow,fcol]=tvspec(trin,t,win_length,ovlpct)
% [tvs,trow,fcol]=tvspec(trin,t,win_length,ovlpct)
% [tvs,trow,fcol]=tvspec(trin,t,win_length)
% 
% TVSPEC computes a complex valued time variant Fourier spectrum. Most
% SPIN applications will want only the amplitude spectrum obtained by
%  tvs=abs(tvs); 
% The raised cosine style MWINDOW is used with a hard-wired 30% taper.
%
% trin= input trace
% t= time coordinate vector
% win_length= length of time window (sec)
% ovlpct= overlap percentage of the windows
%    ********* default= 80.0 **********
% tvs= ouput time variant spectal matrix (complex). There will be
%      one spectrum per row of tvs.
% trow= vector specifying the time of each row
% fcol= vector specifying the frequency of each column
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
% set defaults
 if nargin<=3
  ovlpct=80.;
 end
% compute the window function 
  dt=t(2)-t(1);
  ntwind=2.^nextpow2(win_length/dt+1); % pad window to next power of 2
  window=mwindow(ntwind,30);
% main loop setup
  nend=ntwind;
  nt=length(trin);
  factor=(100.-ovlpct)/100.; % window moveup factor
  numspec= floor((nt-ntwind)/(factor*ntwind))+1;% number of spectra to becomputed
  nspec=1;
% do first spectrum and pre-allocate arrays
% note: all spectra are computed at length nt even though they have
%     only ntwind live samples. This achieves interpolation to the 
%     frequency sample rate needed for application via the SPIN 
%     algorithm.
    temp= [trin(1:nend).*window zeros(1,nt-nend)];
    [spc,fcol]=fftrl(temp,t);
  tvs= zeros(numspec,length(spc));
  tvs(nspec,:)=spc;
  nend=floor(nend+factor*ntwind);
  nspec=nspec+1;
% now enter main loop
  while nend<=nt
    nbeg=nend-ntwind+1; % first sample in window
    trow(nspec)=dt*((nend+nbeg)/2.-1);
% apply window and compute spectrum 
    temp= [zeros(1,nbeg-1) trin(nbeg:nend).*window zeros(1,nt-nend)];
    [spc,fcol]=fftrl(temp,t);
    tvs(nspec,:)=abs(spc);
    nend=floor(nend+ntwind*factor); % last sample in next window
    nspec=nspec+1;
  end
      