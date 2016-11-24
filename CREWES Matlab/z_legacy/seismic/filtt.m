function trout=filtt(trin,t,fmin,fmax,phase,npts)
% trout=filtt(trin,t,fmin,fmax,phase,npts)
% trout=filtt(trin,t,fmin,fmax,phase)
% trout=filtt(trin,t,fmin,fmax)
%
% FILTT filters the input trace in the time domainm using a
% finite impulse response filter designed using FIR1 from the
% signal toolbox. If a minimum phase filter is called for
% it is computed from its zero phase equivalent using TOMIN from
% the seismic toolbox.
%
% trin= input trace
% t= input trace time coordinate vector
% fmin = the lowend filter cutoff
% fmax = the highend filter cutoff
% phase= 0 ... zero phase filter
%       1 ... minimum phase filter
%  ****** default = 0 ********
% npts= number of points in the filter
%   ******* default= 64 *********
%
% by G.F. Margrave, May 1991
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
 
% set defaults
 if nargin < 6
   npts=64;
 end
 if nargin < 5
   phase=0;
 end
 if length(fmax)>1
   fmax=fmax(1);
 end
 if length(fmin)>1
   fmin=fmin(1);
 end
% make filter
  fnyq=1./(2.*(t(2)-t(1)));
  fltr=fir1(npts-1,[fmin/fnyq fmax/fnyq]); % problem here 
  if phase==1
    fltr=tomin(fltr);
  end
% apply filter
  if phase==0
    trout=convz(trin,fltr);
    trout=trout.*mwindow(length(trout),10);
  else
    trout=convm(trin,fltr);
  end  
  