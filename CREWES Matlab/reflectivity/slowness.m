function [pslowness,sslowness,ss_freq] = slowness(n_Layers,Qp,Qs,wc_norm,phase,vp,vs)

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

% computetion of the squared complex frequency dependent velocities and slowness squared,

%for i = 1 : n_Layers
i = sqrt(-1);
 for n=1:n_Layers
    
    % p-wave
    v = vp(n);
    Qp1 = v / (pi * Qp(n));    
    Qp2 = v / (2 * Qp(n));
    r = v + Qp1 * log(wc_norm);  % Muller (1985) Equation 132% a trick with "w_complex=A_0*exp(j*angle)"
    %im = Qp2 - Qp1 * phase;
    im = Qp2 + Qp1 * phase; %checked with Eqn. 132 should be "+"
    aux1 = r * r;
    aux2 = im * im;
    aux = 1/((aux1 + aux2) ^ 2);
    aux3 = (aux1 - aux2) * aux;
    pslowness(n) = complex(aux3, -2 * r * im * aux);
    
    %for s-wave
    v = vs(n);
    Qs1 = v / (pi * Qs(n));
    Qs2 = v / (2 * Qs(n));
    r = v + Qs1 * log(wc_norm);  % Muller (1985) Equation 132
    im = Qs2 + Qs1 * phase;
    
    ss_freq(n) = complex(r * r - im * im,2 * r * im);
    sslowness(n) = 1 / ss_freq(n);
    
end