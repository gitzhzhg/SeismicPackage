function SEGY_WriteTrace(FILE, trace, numsamps)
% SEGY_WRITETRACE(FILE, trace, numsamps)
% 
% This function will stick the trace onto the FILE. 'trace' is a
% filled-out SEGY trace structure. You can get a skeleton of one from the
% function SEGY_GETTRACE. They you fill that out with data and set the
% header values as appropriate and feed it into here. The actual trace
% should be in the form of an array of floats. 
%
% In the file, the numbers will be endian according to how you opened the
% file. Read the help for FOPEN to figure out how to choose between big
% endian and little endian.
%
% 'numsamps' is the number of samples you want to write. Any extras in the
% 'trace' data portion will be ignored. If you specify more samples than 
% are actually contained in the trace, it will be padded out to match.
% This is important because a lot of processing tools expect that every 
% trace in a SEGY file contain the same number of samples. 
%
% You will want to do any fseeking before this point. This function
% sticks the trace in exactly where your file has been fseeked to. So, if
% you're not sure where to put it, you should do something like this:
%
% fseek(FILE, 0, 'eof');
% SEGY_WRITETRACE(FILE, trace);
%
% And you will end up with a shiny new SEGY trace appended to the file
% after everything else..
%
% Note that Seismic Unix (I think) uses files that are SEGY-formatted,
% except that they haven't got the text/binary header on top. So to write SU
% files you just dump out a pile of these traces, without starting with a
% text or binary header.
%
% Chad Hogan, 2004
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

% $Id: SEGY_WriteTrace.m,v 1.2 2004/07/30 21:23:35 kwhall Exp $

if(fwrite(FILE, trace.tracl, 'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.tracr, 'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.fldr,  'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.tracf, 'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.ep,    'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.cdp,   'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.cdpt,  'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.trid,  'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.nvs,   'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.nhs,   'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.duse,  'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.offset,'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.gelev, 'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.selev, 'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.sdepth,'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.gdel,  'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.sdel,  'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.swdep, 'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.gwdep, 'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.scalel,'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.scalco,'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.sx,    'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.sy,    'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.gx,    'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.gy,    'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.counit,'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.wevel, 'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.swevel,'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.sut,   'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.gut,   'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.sstat, 'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.gstat, 'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.tstat, 'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.laga,  'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.lagb,  'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.delrt, 'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.muts,  'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.mute,  'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.ns,    'unsigned short')  ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.dt,    'unsigned short')  ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.gain,  'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.igc,   'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.igi,   'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.corr,  'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.sfs,   'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.sfe,   'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.slen,  'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.styp,  'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.stas,  'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.stae,  'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.tatype,'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.afilf, 'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.afils, 'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.nofilf,'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.nofils,'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.lcf,   'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.hcf,   'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.lcs,   'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.hcs,   'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.year,  'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.day,   'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.hour,  'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.minute,'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.sec,   'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.timbas,'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.trwf,  'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.grnors,'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.grnofr,'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.grnlof,'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.gaps,  'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.otrav, 'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.d1,    'float') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.f1,    'float') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.d2,    'float') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.f2,    'float') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.ungpow,'float') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.unscale,'float')~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.ntr,   'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, trace.mark,  'short') ~= 1) disp(ferror(FILE)); error; end
for i=1:15
    if(fwrite(FILE, 0, 'short')  ~= 1) disp(ferror(FILE)); error; end % pack in the filler
end

realsize = length(trace.data);
% Now we pad if we have less data in the trace than we want from numsamps.
if (realsize < numsamps)
    data(1:numsamps) = 0;
    data(1:realsize) = trace.data;
else
    % If we have enough data, then we'll just use it.
    data = trace.data(1:numsamps);
end

if(fwrite(FILE, data, 'float') ~= numsamps) disp(ferror(FILE)); error; end