function [dump,words,inotempty]=traceheaderdump(traceheaderobj)
% TRACEHEADERDUMP: does a complete dump of the SEGY trace headers from readsegy
%
% traceheaderobj ... a trace header object
% dump ... nwords by ntraces array of header values. nwords is the number
%   of defined header words that can be stored in a trace header according
%   to SEGY Rev 1.  Currently nwords=91.
% words ... cell array of all 91 defined header words
% inotempty ... vector of indices into words of the non-empty parts of the
% trace headers
% 
% To see a ist of header words that are non empty and hence can be usefully
% retrieved with SEGY_getHeader, do:
% [traces,dt,texthead,binaryhead,extendedhead]=readsegy('myfile.sgy')
% [dump,words,inotempty]=traceheaderdump(traces.traceheader);
% words(inotempty) %note round brackets
%
% to retrieve the kth nonempty header value use
% value=SEGY_getHeader(traceheaderobj,words{inotempty(k)});
% note carefully the round and curly brackets in this statement. k must be
% an integer between 1 and length(inotempty).
% 
    words={'tracl','tracr','fldr','tracf','ep','cdp','cdpt','trid','nvs','nhs',...
    'duse','offset','gelev','selev','sdepth','gdel','sdel','swdep',...
    'gwdep','scalel','scalco','sx','sy','gx','gy','counit','wevel',...
    'swevel','sut','gut','sstat','gstat','tstat','laga','lagb','delrt',...
    'muts','mute','ns','dt','gain','igc','igi','corr','sfs','sfe','slen',...
    'styp','stas','stae','tatype','afilf','afils','nofilf','nofils',...
    'lcf','hcf','lcs','hcs','year','day','hour','minute','sec','timbas',...
    'trwf','grnors','grnofr','grnlof','gaps','otrav','cdpx','cdpy',...
    'iline','xline','sp','scalsp','tval','tconstm','tconste','tunit',...
    'devtr','scalt','stypeo','sedm','sede','smmtm','smmte','smmtu',...
    'fbpicks','scalfb'};

ntraces=length(traceheaderobj.traceoffsets)-1;
nwords=length(words);

dump=zeros(nwords,ntraces);

for k=1:nwords
    
 dump(k,:)=SEGY_getHeader(traceheaderobj,words{k});
 
end

test=sum(abs(dump),2);

inotempty=find(test~=0);