function sect = sectfilt(sect,t,fmin,fmax,phase,max_atten)
% SECTFILT: applies a bandpass filter to a seismic section
%
% sectout = sectfilt(sectin,t,fmin,fmax,phase,max_atten)
%
% SECTFILT applies a bandpass filter to a seismic section.
% It simply loops over columns and calls filtf for each trace.
%
% sectin ... input section of size nsamp x ntr. That is one trace per
%	column.
% t ... nsamp long time coordinate vector for sectin
% fmin = a two element vector specifying:
%        fmin(1) : 3db down point of filter on low end (Hz)
%        fmin(2) : gaussian width on low end
%   note: if only one element is given, then fmin(2) defaults
%         to 5 Hz. Set to [0 0] for a low pass filter  
% fmax = a two element vector specifying:
%        fmax(1) : 3db down point of filter on high end (Hz)
%        fmax(2) : gaussian width on high end
%   note: if only one element is given, then fmax(2) defaults
%         to 10% of Fnyquist. Set to [0 0] for a high pass filter. 
% phase= 0 ... zero phase filter
%       1 ... minimum phase filter
%  ****** default = 0 ********
% note: Minimum phase filters are approximate in the sense that
%  the output from FILTF is truncated to be the same length as the
%  input. This works fine as long as the trace being filtered is
%  long compared to the impulse response of your filter. Be wary
%  of narrow band minimum phase filters on short time series. The
%  result may not be minimum phase.
% 
% max_atten= maximum attenuation in decibels
%   ******* default= 80db *********
%
% sectout ... output section of size nsamp x ntr.
%
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

% G.F. Margrave, CREWES Project, University of Calgary, 1996

[nsamp,ntr]=size(sect);

if(nargin < 5)
	for k=1:ntr
		sect(:,k) = filtf( sect(:,k),t,fmin,fmax);
	end
elseif( nargin < 6)
	for k=1:ntr
		sect(:,k) = filtf( sect(:,k),t,fmin,fmax,phase);
	end
else
	for k=1:ntr
		sect(:,k) = filtf( sect(:,k),t,fmin,fmax,phase,max_atten);
	end
end