function [mfilt,tm]=matchf(trin,trdsign,t,fsmoth,flag)
% [mfilt,tm]=matchf(trin,trdsign,t,fsmoth,flag)
%
% MATCHF designs a frequency domain match filter.
% The match filter will always have time zero in the middle and
% should be applied with convz.
%
% trin= input trace to be matched to trdsign
% trdsign= input trace which is to be matched
% t= time coordinate vector for trin
% ***** note: trin and trdsign must be the same length
% fsmoth= length of frequency smoother in Hz.
% flag=0 ... smooth the filter spectrum prior to inverse transform
%     =1 ... smooth the spectra of trin and trdsign before division
%     =2 ... both 0 and 2   
%  ********* default= 0 ********      
%
% mfilt= output mlength match filter
% tm= time coordinate for the match filter
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
 if nargin<5, flag=0; end
% forward fft's
 [Trin,f]=fftrl(trin,t);
 [Trdsign,f]=fftrl(trdsign,t);
% smooth
 if flag ~=0,
   Trin=convz(Trin,boxf(fsmoth,f));
   Trdsign=convz(Trdsign,boxf(fsmoth,f));
 end
% compute operator spectrum
 Mf=Trdsign./Trin;
% smooth
 if flag~=1,
   Mf=convz(Mf,boxf(fsmoth,f));
 end
% inverse transform
 mfilt=ifftrl(Mf,f);
 mfilt=fftshift(mfilt);
 tm=xcoord(-max(t)/2.,.002,mfilt);
   
 
 