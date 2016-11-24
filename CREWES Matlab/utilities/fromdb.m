function S= fromdb(Sdb,refamp)
% FROMDB: convert from (db,phase) to (real,imaginary)
%
% S = fromdb(Sdb,refamp);
% S = fromdb(Sdb);
%
% FROMDB converts a complex spectrum from (decibels,phase angle) to 
% (real,imaginary). Thus if Sdb is a complex spectrum created by
% TODB, then the original complex spectrum can be recreated
% by fromdb(Sdb,refamp) if refamp is known or, to within a scale
% factor if refamp is not known.
%
% Sdb= input complex spectrum in (dbdown, phase angle) format
% refamp = input reference amplitude
%           decibels are dbdown from this value. If defaulted,
%           refamp= 1.0
% S= output complex spectrum in the (real, imaginary) format
%     
% 
% the INVERSE of this process is the function TODB found in
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

if(nargin==1)
   refamp=1.0;
 end
ten=10.0;
amp= refamp*ten.^(real(Sdb)/20.);
S= amp.*cos(imag(Sdb)) + i*amp.*sin(imag(Sdb));