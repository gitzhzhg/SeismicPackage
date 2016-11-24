function INTD=ls_mask_fitting_fgft(D,H,M,nh,iter_cg)
% This function fits the known mask function and available data using fgft.
%
%  Author(s): Mostafa Naghizadeh (University of Calgary)
%  Copyright 2010 Naghizadeh
%
%%% Note: Variables with upper-case letters are matrices or vector and
%%%       variables with lower-case letters are scalars
%
%%% Input:
% D: 1D Data (just available samples)
% Vector containig available data (offsets)
% M: Mask function in generalized Fourier domain
% nh: total number of offsets
% iter_cg: Number of Conjugate Gradient steps
%
%
%%% Output:
% INTD: Interpolated data
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

X= zeros(1,nh);
S=[D];
R=forward_operator(S,H,M,nh);
P=R;
rr_new = R*R';
err_old=S*S';
% Internal loop for CG fitting
for j = 1:iter_cg;
    Q=adjoint_operator(P,H,M,nh);
    alpha=rr_new/(Q*Q');
    X=X+alpha*P;
    S=S-alpha*Q;
    R=forward_operator(S,H,M,nh);
    rr_old=rr_new;
    rr_new=R*R';
    beta=rr_new/rr_old;
    P=R+beta*P;
    % error term computations
    err_new=S*S';
    err_old = err_new ;
end
INTD= sqrt(nh)*inverse_fgft(X);

%%%%%%%%%#################%%%%%%%%%%%%
function R=forward_operator(S,H,M,nh)
% Adjoint operator for band_limited reconstruction
TMP1=zeros(1,nh);
TMP1(H)= S;
R = (1/(sqrt(nh)))*forward_fgft(TMP1).*M; %length of nh


%%%%%%%%%#################%%%%%%%%%%%%
function Q=adjoint_operator(P,H,M,nh)
% Forward operator for band-limited reconstruction
TMP1 = (sqrt(nh))*inverse_fgft(M.*P);
Q = TMP1(H); %length ns