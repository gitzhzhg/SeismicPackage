function [wavelet,tw,pseudo]=wwowf(trin,t,fsmoth,flag)
% [wavelet,tw]=wwowf(trin,t,fsmoth,flag)
%
% WWOWF performs the 'wwow' method of wavelet extraction which
% is identical to a single channel BOOT. The final wavelet is 
% estimated by a frequency domain match filter
%
% trin= input trace
% t= time coordinate for input trace
% fsmoth= length of frequency smoother in Hz.
% flag=0 ... smooth the filter spectrum prior to inverse transform
%     =1 ... smooth the spectra of trin and trdsign before division
%     =2 ... both 0 and 2   
%  ********* default= 0 ********  
% wavelet= output wavelet
% tw= time coordinate for wavelet
%
% by G.F. Margrave, June 1991 
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
 if nargin<4, flag=0;end
% get the hilbert mask
 [mask,htrin]=hmask(trin);
% find the constant phase rotation
  hsum=maxima(htrin,mask);
% iex=find(mask==1);
% hsum=htrin(iex);
 top= 2.*sum(real(hsum).*imag(hsum));
 bot= sum(imag(hsum).^2-real(hsum).^2);
 theta=.5*atan2(top,bot);
% test theta and theta+pi/2
 trot1=phsrot(trin,theta*180/pi);
 trot2=phsrot(trin,(theta+pi/2)*180/pi);
 rms1=norm(trot1.*mask);
 rms2=norm(trot2.*mask);
 if rms2>rms1, theta=theta+pi/2; trot1=trot2; end
% compute the pseudo reflectivity
 pseudo=mask.*trot1;
 wavelet=theta*180/pi;
%
% match filter for the wavelet
%
  [wavelet,tw]=matchf(pseudo,trin,t,fsmoth,flag);
 