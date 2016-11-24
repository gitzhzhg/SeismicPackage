function[J0,J1] = bessels(arg,rec,v1,v2,window_S)
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

        % THE BESSEL FUNCTION COMPUTATION
        
            if arg <= 2.75
                arg3 = arg / 3;
                x2 = arg3 * arg3;
                x4 = x2 * x2;
                x6 = x2 * x4;
                x8 = x4 * x4;
                x10 = x4 * x6;
                x12 = x6 * x6;
                     
                J0 = 1. - 2.2499997 * x2 + 1.2656208 * x4 - 0.3163866 * x6 + 0.0444479 * x8 - 0.0039444 * x10 + 0.00021 * x12;
                J1 = (0.5 - 0.56249985 * x2 + 0.21093573 * x4 - 0.03954289 * x6 + 0.00443319 * x8 - 0.00031761 * x10 + 0.00001109 * x12) * arg;
            else
                sqrx = sqrt(2. / (pi * arg));
                J0 = sqrx * cos(arg - pi * 0.25);
                J1 = sqrx * cos(arg - pi * 0.75);
            end