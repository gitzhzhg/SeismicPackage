function Sdb= todb(S,refamp)
% TODB: converts from (real,imaginary) to (decibels, phase)
%
% Sdb = todb(S,refamp);
% Sdb = todb(S);
%
% converts a complex spectrum from (real,imaginary) to 
% (decibels,phase angle). Thus if S is a complex spectrum,
% then plot(real(todb(S))) will plot the decibel amplitude
% and plot(imag(todb(S))) will plot the phase spectrum (radians)
%
% S= input complex spectrum
% refamp = input reference amplitude
%           decibels are dbdown from this value. If defaulted,
%           refamp=max(abs(S))
% Sdb= output complex spectrum in the special format:
%       (dbdown, phase angle)
% 
% the INVERSE of this process is the function FROMDB found in
% the seismic toolbox
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

 amp=abs(S); 
 phs=angle(S); 
if(nargin==1)
   refamp=max(max(amp));
end

amp=20*log10(amp/refamp);
Sdb=amp + i*phs;
    