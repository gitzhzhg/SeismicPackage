function PK=plot_fgft(K)
% This function plots the coefficients of a generalized Fourier transform. 
% It transforms 1D coefficients into a 2D array. 
%
%% Input
%   K: 1D array that contains the coefficients of FGFT (1 x nk)
%
%% Output
%   PK: 2D array (nk x nk)
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
nk=length(K);

%PK=zeros(nk,nk);

n2=nextpow2(nk);

% Upscaling first and second coefficients
PKp1=matrix_scaling(K(1),1,nk);
PKp2=matrix_scaling(K(2),1,nk);
PKn1=matrix_scaling(K(nk-1),1,nk);
PKn2=matrix_scaling(K(nk),1,nk);

% Starting loop to pick parts of K and upscale them
PPK=[];
NPK=[];
for ia=2:n2-1
    % At each ia step we will take the fft of positive and
    % negative frequencies with variant window size.
    pfs=2^ia-2^(ia-1)+1;
    pfe=2^ia;
    nfs=(2^n2-(2^ia-2^(ia-1)+1)+1)-2^(ia-1)+1;
    nfe=2^n2-(2^ia-2^(ia-1)+1)+1;
    
    
    % Positive frequencies
    PPK=[PPK; matrix_scaling(K(pfs:pfe),2^(ia-1),nk)];
        
    % Negative frequencies
    NPK=[matrix_scaling(K(nfs:nfe),2^(ia-1),nk); NPK];
    
end

PK=fliplr([PKp1;PKp2;PPK;NPK;PKn1;PKn2]);