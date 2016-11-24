function trout=phsrot(trin,theta)
% PHSROT Constant-phase rotate a trace
%
% trout=phsrot(trin,theta)
%
% PHSROT performs a constant phase rotation of the input trace
% through an angle of theta degrees.
%
% trin= input trace or trace gather
% theta= phase rotation angle in degrees
% trout= phase rotated trace
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

%test for section input

[nsamps,ntraces]=size(trin);

if((nsamps-1)*(ntraces-1)==0)
    %single trace
    
    %calculate Hilbert transform
    trinh=imag(hilbert(trin));
    
    %rotate
    trout=cos(theta*pi/180)*trin-sin(theta*pi/180)*trinh;
else
    %multi trace
    trout=zeros(size(trin));
    for k=1:ntraces
        tmp=trin(:,k);
        tmph=imag(hilbert(tmp));
        trout(:,k)=cos(theta*pi/180)*tmp-sin(theta*pi/180)*tmph;
    end
end