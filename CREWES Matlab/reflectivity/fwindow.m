function window_s= fwindow(perc_U,u1,u2,delta_U)
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

%
    wl = (u2 - u1) * perc_U;
    p1 = u1;
    p2 = u1 + wl;
    p3 = u2 - wl;
    p4 = u2;
    nu = (u2 - u1) / delta_U + 1;
    p = u1 - delta_U;
%
    window_s = zeros(nu,1);
%
for i = 1 : nu
    p = p + delta_U;
    window_s(i) = 1;
    if p <= p2
       window_s(i) = 0.5 + 0.5 * cos(pi * (p2 - p)/(p2 - p1));
%        window_s(i)=1;
    end
    if p >= p3
       window_s(i) = 0.5 + 0.5 * cos(pi * (p - p3)/(p4 - p3));
    end
end