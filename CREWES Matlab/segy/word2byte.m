function byteloc=word2byte(word)
% WORD2BYTE returns the byte location of a header word
% 
% byteloc=word2byte(word)
%
% word ... a sting containing one of the defined words
%     SEG-Y revision 1 standard words are:
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
% byteloc ... The byte location of the input word in the header. If the
% word is not found in the standard dictionary, then [] is returned
%
% Example, retrieve the byte location of SEGY header word 'cdp'
% byteloc=word2byte('cdp');
% byteloc will be 21 for this example
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

bytelocations=[1,5,9,13,17,21,25,29,31,33,35,37,41,45,49,53,57,61,65,69,71,73,77.81,85,89,...
    91,93,95,97,99,101,103,105,107,109,111,113,115,117,119,121,123,125,127,129,...
    131,133,135,137,139,141,143,145,147,149,151,153,155,157,159,161,163,165,...
    167,169,171,173,175,177,179,181,185,189,193,197,201,203,205,209,211,213,...
    215,217,219,223,225,229,231,233,237];

byteloc=[];

for k=1:length(words);
    if(strcmp(word,words{k}))
        byteloc=bytelocations(k);
    end
end