function trace = SEGY_ReadTrace(segy, tracenum)
% trace = SEGY_ReadTrace(segy, tracenum)
% 
% 'segy' is a segy struct returned by SEGY_OpenFile. 'tracenum' is the
% number of the trace you want to read.
%
% Chad Hogan, 2008
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

% $Id: SEGY_ReadTrace.m,v 1.1 2008/03/04 22:37:48 cmhogan Exp $

FILE = segy.FILE;
SEGY_TraceSeek(segy, tracenum);

trace.tracl = fread(FILE, 1, 'int');
trace.tracr = fread(FILE, 1, 'int');
trace.fldr = fread(FILE, 1,  'int');
trace.tracf = fread(FILE, 1, 'int');
trace.ep = fread(FILE, 1,    'int');
trace.cdp = fread(FILE, 1,   'int');
trace.cdpt = fread(FILE, 1,  'int');
trace.trid = fread(FILE, 1,  'short');
trace.nvs = fread(FILE, 1,   'short');
trace.nhs = fread(FILE, 1 ,  'short');
trace.duse = fread(FILE, 1,  'short');
trace.offset = fread(FILE, 1,'int');
trace.gelev = fread(FILE, 1, 'int');
trace.selev = fread(FILE, 1, 'int');
trace.sdepth = fread(FILE, 1,'int');
trace.gdel = fread(FILE, 1,  'int');
trace.sdel = fread(FILE, 1,  'int');
trace.swdep = fread(FILE, 1, 'int');
trace.gwdep = fread(FILE, 1, 'int');
trace.scalel = fread(FILE, 1,'short');
trace.scalco = fread(FILE, 1,'short');
trace.sx = fread(FILE, 1,    'int');
trace.sy = fread(FILE, 1,    'int');
trace.gx = fread(FILE, 1,    'int');
trace.gy = fread(FILE, 1,    'int');
trace.counit = fread(FILE, 1,'short');
trace.wevel = fread(FILE, 1, 'short');
trace.swevel = fread(FILE, 1,'short');
trace.sut = fread(FILE, 1,   'short');
trace.gut = fread(FILE, 1,   'short');
trace.sstat = fread(FILE, 1, 'short');
trace.gstat = fread(FILE, 1, 'short');
trace.tstat = fread(FILE, 1, 'short');
trace.laga = fread(FILE, 1,  'short');
trace.lagb = fread(FILE, 1,  'short');
trace.delrt = fread(FILE, 1, 'short');
trace.muts = fread(FILE, 1,  'short');
trace.mute = fread(FILE, 1,  'short');
trace.ns = fread(FILE, 1,    'ushort') ;
trace.dt = fread(FILE, 1,    'ushort') ;
trace.gain = fread(FILE, 1,  'short');
trace.igc = fread(FILE, 1,   'short');
trace.igi = fread(FILE, 1,   'short');
trace.corr = fread(FILE, 1,  'short');
trace.sfs = fread(FILE, 1,   'short');
trace.sfe = fread(FILE, 1,   'short');
trace.slen = fread(FILE, 1,  'short');
trace.styp = fread(FILE, 1,  'short');
trace.stas = fread(FILE, 1,  'short');
trace.stae = fread(FILE, 1,  'short');
trace.tatype = fread(FILE, 1,'short');
trace.afilf = fread(FILE, 1, 'short');
trace.afils = fread(FILE, 1, 'short');
trace.nofilf = fread(FILE, 1,'short');
trace.nofils = fread(FILE, 1,'short');
trace.lcf = fread(FILE, 1,   'short');
trace.hcf = fread(FILE, 1,   'short');
trace.lcs = fread(FILE, 1,   'short');
trace.hcs = fread(FILE, 1,   'short');
trace.year = fread(FILE, 1,  'short');
trace.day = fread(FILE, 1,   'short');
trace.hour = fread(FILE, 1,  'short');
trace.minute = fread(FILE, 1,'short');
trace.sec = fread(FILE, 1,   'short');
trace.timbas = fread(FILE, 1,'short');
trace.trwf = fread(FILE, 1,  'short');
trace.grnors = fread(FILE, 1,'short');
trace.grnofr = fread(FILE, 1,'short');
trace.grnlof = fread(FILE, 1,'short');
trace.gaps = fread(FILE, 1,  'short');
trace.otrav = fread(FILE, 1, 'short');
trace.d1 = fread(FILE, 1,    'float');
trace.f1 = fread(FILE, 1,    'float');
trace.d2 = fread(FILE, 1,    'float');
trace.f2 = fread(FILE, 1,    'float');
trace.ungpow = fread(FILE, 1,'float');
trace.unscale = fread(FILE, 1,'float');
trace.ntr = fread(FILE, 1,   'int');
trace.mark = fread(FILE, 1,  'short');
for i=1:15
   trace.filler(i)  = fread(FILE, 1, 'short') ; % pack in the filler
end

trace.data = fread(FILE, trace.ns, 'float');
trace.bytelength = (1920 + (trace.ns * 32)) / 8;