function q=sprat(A1,A2,f,t1,t2,fsmo)
% SPRAT: Estimate Q by spectral ratios
% 
% q=sprat(A1,A2,f,t1,t2,fsmo)
%
% SPRAT applies the spectral ratio method to estimate an apparent
% interval attenuation factor, q, between the traces (wavelets) 
% whose amplitude spectra are A1 and A2. Both spectra must be the
% same length, one sided (positive frequencies only), and described 
% by the frequency vector f. To use sSPRAT: (1) Calculate two amplitude 
% spectra from short temporal windows, (2) input these spectra into sprat 
% and also provide the window center times and a smoother length. 
% (3) sprat will then make a plot on which you must define a frequency 
% band for the least squares fit. Q predicts a spectral ratio that 
% slopes down to the right. (4) Click the mouse once at the lowest 
% frequency for which you see the expected linear slope and again at the 
% highest and hit "enter". sprat will then tell you the Q.
% 
% Spectra are smoothed prior to ratioing with a boxcar of length
% fsmo. The log spectral ratio is plotted and the user enters a box
% with the mouse (as in the manner of a zoom with PLT) which 
% determines the frequency range for the least squares fit. The 
% best fit straight line is then determined and plotted and the
% apparent q is shown.
%
% Even with synthetic data, you rarely see the expected linear slope on 
% the spectral ratio plot over anything but a very narrow frequency band. 
% This is due to a number of effects including: noise and limited precision 
% in both the data and the computation, the effect of reflectivity (which 
% tends to make the spectra have notches), and the unavoidable contamination 
% of the spectrum by the window. You can address all of these a little bit. 
% The smoother in sprat is intended to help the reflectivity notches. 
% The choice of frequency band over which to do the least squares fit is 
% intended to address the noise and precision issue. Finally, you can 
% experiment with different spectral windows to help with the bias of the 
% window on the spectrum. Often the good results are obtained with the Burg 
% rather than the Fourier spectrum. The Burg spectrum is designed to limit
% the bias of short windows. There is "burg.m" in the CREWES toolbox. 
% To use it, select a short portion of a trace using a boxcar window 
% (i.e. just take the samples directly) and input to BURG. 
% By using smooth Burg spectra you can often bypass the need for further 
% smoothing in the spectral ratio calculation.
% 
%  A1 ... amplitude spectrum at time t1
%  A2 ... amplitude spectrum at time t2
%  f ... frequency vector for A1 and A2
%  t1 ... time for spectrum A1
%  t2 ... time for spectrum A2
%  fsmo ... length (Hz) of a frequency smoother
%
% by G.F. Margrave, July 1991
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
% first smooth the spectra
if(fsmo>0)
 A1=convz(A1,boxf(fsmo,f));
 A2=convz(A2,boxf(fsmo,f));
end
delt=t2-t1;
% now ratio them
 rat=log(A2./A1);
% plot the ratio
 plot(f,rat)
 grid;
 xlabel('frequency');
 ylabel('log ratio');
 text(.1,.05,'Select frequency range','sc');
% get the frequency range of the fit
[x1,y1]=ginput;
 while ~isempty(x1)
  fmin=min(x1);
  fmax=max(x1);
  if fmin==fmax, error(' Zero length frequency band chosen'),end
% fit the straight line
  infit=near(f,fmin,fmax);
  p=polyfit(f(infit),rat(infit),1);
  line=polyval(p,f(infit));
% determine interval Q
  q=-pi*delt/p(1);
% draw new graph
  plot(f,rat,f(infit),line,'+')
  grid;
  xlabel('frequency');
  ylabel('log ratio');
  text(.1,.05,sprintf(' q estimate = %g',q),'sc');
  text(.1,.00,'Select frequency range','sc');
  [x1,y1]=ginput;
end
  
  
  