function obj=SEGY_getTraces(tracehead,varargin)
% traces=SEGY_getTraces(tracehead)
% traces=SEGY_getTraces(tracehead,word,value);
% traces=SEGY_getTraces(tracehead,word1,value1,word2,value2,word2,value3,...);
%
% SEGY_getTraces is a function that allows the user to only read in some of
% the traces.  This is done by comparing value to the traceheader data.
% word must be one of the names defined in the definitions file.  For SEG-Y
% revision 1 standards the word must be one of the following:
%     'tracl','tracr','fldr','tracf','ep','cdp','cdpt','trid','nvs','nhs',
%     'duse','offset','gelev','selev','sdepth','gdel','sdel','swdep',
%     'gwdep','scalel','scalco','sx','sy','gx','gy','counit','wevel',
%     'swevel','sut','gut','sstat','gstat','tstat','laga','lagb','delrt',
%     'muts','mute','ns','dt','gain','igc','igi','corr','sfs','sfe','slen',
%     'styp','stas','stae','tatype','afilf','afils','nofilf','nofils',
%     'lcf','hcf','lcs','hcs','year','day','hour','minute','sec','timbas',
%     'trwf','grnors','grnofr','grnlof','gaps','otrav','cdpx','cdpy',
%     'iline','xline','sp','scalsp','tval','tconstm','tconste','tunit',
%     'devtr','scalt','stypeo','sedm','sede','smmtm','smmte','smmtu',
%     'fbpicks','scalfb';
%
% It is possible to search using multiple words.  An example would be 
%  traces=SEGY_getTraces(tracehead,'gx',1:50,'cdpx',60);  
% this would search for traces that had a x coordinate from 1:50 and has a
% common depth point of 60.
%
% Input:
%  tracehead= can be either a TraceHeader object which can be created by 
%     SEGY_readHeaders or a filename.
%  word= a name defined in the definitions file, SEG-Y Revision 1 Standard
%     words are given above
%  value= a value is either scalar values or numerical arrays
%
% Output:
%  traces= a Trace object.  To get the traceheader values use
%       SEGY_getHeader.  To get the trace data values use SEGY_getData.
%
% Heather Lloyd 2010, Kevin Hall 2009, Chad Hogan 2004
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

obj=Trace(tracehead);
obj=obj.getTraces(varargin);
end