function D=scale_fgft(D)
% This function removes the sqrt(nh) factor from forward transformed FGFT
% data
%
%% Input
%   D: 1D array that contains the coefficients of FGFT (1 x nk)
%
%% Output
%   D: Scales data
%
% Author: Mostafa Naghizadeh; Copyright (C) 2010
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


% Determining the size of coefficients
nk=length(D);

%PK=zeros(nk,nk);

n2=nextpow2(nk);

for ia=2:n2-1
    % At each ia step we will take the fft of positive and
    % negative frequencies with variant window size.
    pfs=2^ia-2^(ia-1)+2;
    pfe=2^ia+1;
    nfs=(2^n2-(2^ia-2^(ia-1)+1)+1)-2^(ia-1)+2;
    nfe=2^n2-(2^ia-2^(ia-1)+1)+2;
    
    
    % Positive frequencies
    D(pfs:pfe)=sqrt(2^(ia-1))*D(pfs:pfe);
        
    % Negative frequencies
    D(nfs:nfe)=sqrt(2^(ia-1))*D(nfs:nfe);
    
end