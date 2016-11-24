function INTD=irls_fitting_fgft(D,H,nh,iter_cg,iter_bl)
% This function fits the available data using iterative reweighting least
% squares fgft.
%
%  Author(s): Mostafa Naghizadeh (University of Calgary)
%  Copyright 2010 Naghizadeh
%
%%% Note: Variables with upper-case letters are matrices or vector and
%%%       variables with lower-case letters are scalars
%
%%% Input:
% D: 1D Data (just available samples)
% H: Vector containg samples number of available data (offsets)
% nh: total number of offsets
% iter_cg: Number of Conjugate Gradient steps
% iter_bl: external iteration to sparsify the mask function
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

% Initiating mask function
M=ones(1,nh);

% External loop to update a mask function driven from data
for ia=1:iter_bl

    X= zeros(1,nh);
    S=[D];
    R=forward_operator(S,H,M,nh);
    P=R;
    rr_new = R*R';
    err_old=S*S';
    
    % Last CG fitting is done with more percision
    if (ia==iter_bl)
        iter_cg=5*iter_cg;
    end
    
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
    %Updating mask function
    M=update_mask_fgft(X);
    
    % Last updated mask function is reduced to 0 and 1's
    if (ia==(iter_bl-1))
        % Tresholding smaller values of M
        pctg=0.75;
        cfs = sort(abs(M));
        nb = round(pctg*length(cfs));
        cutoff = cfs(nb);
        % Set small coefficients to zero
        for w=1:length(M)
            M(w)= (abs(M(w))>cutoff);
        end
    end

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