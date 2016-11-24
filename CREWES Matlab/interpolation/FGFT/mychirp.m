function [d,t,cf] = mychirp(f0,f1,dt,nt);
%mychirp: computes a hyperbolic chirp
%
%
% IN.   f0,f1  : freq at t=0 and freq at t=tmax
%       dt     : sample interval
%       nt     : number of samples
% OUT.  d(nt)  : chirp
%       cf     : Instantaenous frequency of the chirp
%       t      : time axis
%
% Example:
%          [d,t,cf] = mychirp(10,40,0.004,200); 
%          subplot(221); plot(t,d);
%          subplot(222); plot(t,cf);
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


 t = (0:1:nt-1)*dt;
 c = sqrt(f0/f1);
 b = max(t)/(1-c);
 w0 = 2*pi*f0;
 a = w0*b*b;
 d = cos(a./(b-t));
 d=taper(d',5,5);
 cf = a./(b-t).^2;
 cf = cf/2/pi;
 return;