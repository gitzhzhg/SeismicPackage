function wnorm = wavenorm(w,tw,flag)
% WAVENORM: normalize a wavelet
%
% wnorm = wavenorm(w,tw,flag)
%
% WAVENORM normalizes a wavelet by one of several criteria.
% The choices are: (1) normalize the maximum absolut value to unity
% (2) normalize such that a sine wave at the dominant frequency passes
% with unit amplitude (3) normalize the rms amplitude to unity.
%
% w ... input wavelet
% tw ... time coordinate vector for w
% flag ... (1) normalize the maximum absolute value to unity
%          (2) normalize such that a sine wave at the dominant frequency 
%              passes with unit amplitude
%          (3) normalize the rms amplitude to unity
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

if(flag==1)
    wnorm = w/max(abs(w));
elseif(flag==2)
    %compute spectrum
    [W,f]=fftrl(w,tw);
    A=real(todb(W));
    %pick maximum amplitude
    ind=find(A==max(A));
    fdom = f(ind(1));
    refwave=sin(2*pi*fdom*tw);
    reftest=convz(refwave,w);
%     reftest=convz(refwave,w)/sum(abs(w));
    fact=max(refwave)/max(reftest);
    wnorm=w*fact;
elseif(flag==3)
    rms_ave=norm(w)/sqrt(length(w));
    wnorm=w/rms_ave;
else
    error('invalid flag')
end