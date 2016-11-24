function bhead = SEGY_GetBinaryHeader
% bhead = SEGY_GETBINARYHEADER;
%
% This function will return a binary header for a segy data set. It will
% be full of all the necessary header fields, but they are unset (or set
% to some kind of default). Set the values that you need once you get
% this binary header.
%
% Chad Hogan, 2004
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

% $Id: SEGY_GetBinaryHeader.m,v 1.3 2009/07/24 15:49:18 kwhall Exp $

bhead.jobid   = 0;      % job id number
bhead.lino    = 0;      % line number, one line per reel
bhead.reno    = 0;      % reel number
bhead.ntrpr   = 0;      % number of traces in a record
bhead.nart    = 0;      % number of auxiliary traces in a record
bhead.hdt     = 2000;      % sample interval in microsec for this reel
bhead.dto     = 2000;      % sample interval in microsec for original field
                        % recording
bhead.hns     = 0;      % number of samples per trace for this reel
bhead.nso     = 0;      % number of samples per trace for oroginal field
                        % recording
bhead.format  = 1;      % data sample format code:
                        % 1 = floating point, 4 bytes
                        % 2 = fixed point, 4 bytes
                        % 3 = fixed point, 2 bytes
                        % 4 = fixed point with gain code, 4 bytes

bhead.fold    = 1;      % expected CDP fold per ensemble
bhead.tsort   = 1;      % trace sorting code
                        % 1 = as recorded (no particular sorting)
                        % 2 = CDP ensemble
                        % 3 = single fold continuous profile
                        % 4 = horizontally stacked

bhead.vscode  = 1;      % vertical sum code:
                        % 1 = no sum
                        % 2 = two sum ...
                        % N = N sum

bhead.hsfs    = 0;      % starting sweep frequency
bhead.hsfe    = 0;      % ending sweep frequency
bhead.hslen   = 0;      % sweep length in ms

bhead.hstyp   = 1;      % sweep type code:
                        % 1 = linear
                        % 2 = parabolic
                        % 3 = exponential
                        % 4 = something else

bhead.schn    = 0;      % trace number of sweep channel
bhead.hstas   = 0;      % sweep trace taper length at start if tapers.
bhead.hstae   = 0;      % sweep trace taper at the end
bhead.htatyp  = 1;      % sweep trace taper type code:
                        % 1 = linear
                        % 2 = cos-squared
                        % 3 = other

bhead.hcorr   = 1;      % correlated data traces code, 1 = no 2 = yes
bhead.bgrcv   = 1;      % binary gain recovered code,  1 = yes 2 = no
                        % Seriously? Nice work guys. 

bhead.rcvm    = 1;      % amplitude recovery method:
                        % 1 = none
                        % 2 = spherical divergence
                        % 3 = AGC
                        % 4 = something else

bhead.mfeet   = 1;      % units, 1 = meters, 2 = feet
bhead.polyt   = 1;      % impulse signal polarity code.
                        % 1 = increase in pressure or upward geophone
                        % case movement gives negative number
                        % 2 = opposite to 1.

bhead.vpol    = 1;      % vibe polarity code
                        % 1 = signal lags pilot by 337.5 - 22.5 degrees
                        % 2 = 22.5 - 67.5 degrees
                        % 3 = 67.5 - 112.5 degrees
                        % 4 = 112.5 - 157.5
                        % 5 = 157.5 - 202.5
                        % 6 = 202.5 - 247.5
                        % 7 = 247.5 - 292.5
                        % 8 = 292.5 - 337.5

bhead.rev = 256;        % 0   = SEG-Y
                        % 256 = SEG-Y revision 1
                        
bhead.trfix = 1;        % 1 = fixed trace length
                        % 0 = variable trace length
                       
bhead.nthdr = 0;        % number of extended textual headers