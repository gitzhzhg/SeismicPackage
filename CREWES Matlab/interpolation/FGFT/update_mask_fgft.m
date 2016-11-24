function M=update_mask_fgft(K)
% This function updates the mask function for irls fitting of FGFT
%% Input
%   K: 1D general Fourier transform  
%
%% Output
%   M: Mask function
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

% Determining the size of data
nk=length(K);

% Going to the next power of 2
n2=nextpow2(nk);

%Putting absolute value of K as mask function
M=abs(K);

% Starting loop to pick parts of K and apply FFT in smaller sizes
for ia=2:n2-1
    % At each ia step we will take the fft of positive and
    % negative frequencies with variant window size.
    pfs=2^ia-2^(ia-1)+1;
    pfe=2^ia;
    nfs=(2^n2-(2^ia-2^(ia-1)+1)+1)-2^(ia-1)+1;
    nfe=2^n2-(2^ia-2^(ia-1)+1)+1;
    
    % Just smoothing high frequencies 
    if ia>3
        %smothing the spectrum
        % Length of hanning window for smoothing the spectrum (must be odd)
        %nw=2*(ia-2)+1;
        nw=5;
        % Positive frequencies
        TMP1=conv(abs(K(pfs:pfe)),hanning(nw))/nw;
        M(pfs:pfe)=TMP1((nw+1)/2:pfe-pfs+(nw+1)/2);

        % Negative frequencies
        TMP2=conv(abs(K(nfs:nfe)),hanning(nw))/nw;
        M(nfs:nfe)=TMP2((nw+1)/2:nfe-nfs+(nw+1)/2);
    end
end