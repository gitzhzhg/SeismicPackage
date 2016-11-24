function [trc, t] = segd_read(filein,varargin)
% [trc t] = segd_read(filein,varargin)
%
% segd_read reads a SEG-D disk file. It has been tested on exactly one file
% from INOVA. Descale multipliers are ignored.
%
% Inputs:
%   filein   = the name of the SEG-D file that is to be read in.
%   varargin:
%       'file','filename.sgd'
%       'verbose','true'
%       'traces',[traceMin traceMax]
%
% Defaults:
%   - Do not print trace header contents ('verbose',false)
%   - Read and return all traces in the file
%
% Examples:
%   >> trc = segd_read;
%   >> [trc t] = segd_read;
%   >> trc = segd_read('00045.segd');
%   >> trc = segd_read('00045.segd','verbose','true');
%   >> trc = segd_read('00045.segd','verbose','false','traces',[1 4]);
%
% Outputs:
%   trc= array of trace data
%
% Author:
%   Kevin Hall, 2011
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

%% Parse input arguments

%set defaults
traces=[];  %get all traces in file
printhdr=0; %don't print header values

%if no input arguments, assume defaults and get a filename
if nargin<1 
    [filename, pathname] = ...
        uigetfile( {'*.sgd;*.segd;*.SGD;*.SEGD',...
        'All SEG-D Files (*.sgd,*.segd,*.SGD,*.SEGD)'; ...
        '*.*','All Files (*.*)'},'Select a SEG-D File');
    if  filename~=0
        filein=[pathname,filename];
    else
        me=MException('SEGD_READ:InvalidFilename',...
            'Please select a valid filename');
        throw(me)
    end
else
    % parse varargin for options
    for i = 1:2:length(varargin)
        name = varargin{i};
        value = varargin{i+1};
        if(~ischar(name))
            error('varargin must contain character strings paired with values');
        end
        switch name
            case 'verbose'
                if(~ischar(value))
                   error('''verbose'' must be ''true'' or ''false'''); 
                end
                printhdr = strcmpi(value,'true');
            case 'traces'
                if(~isnumeric(value) || length(value) ~= 2 || value(1)>value(2))
                  error(['''traces'' must be a two element array: '...
                         '[trcmin trcmax]']);  
                end
                traces = value;
            otherwise
        end
    end
end

%% Open file for reading:
%open with read-only permissions and big-endian byte order
SEGD = File(filein,'permission','r','machineformat','ieee-be');

%% General Header block 1
ghb1 = Headers(SEGD.fid,'generalHeader1');
dt   = getHeaderValue(ghb1,'baseScanInterval').toInterpretedString();

if getHeaderValue(ghb1,'generalHeaderBlocks').isEqual('0')
    me=MException('SEGD_READ:InvalidFormat',...
     'File is SEG-D revision 0. This script can read revision numbers >1');
    throw(me)    
end

if ~getHeaderValue(ghb1,'formatCode').isEqual('8058')      
    me=MException('SEGD_READ:InvalidFormat',...
        ['Data is stored as ''' ...
          getHeaderValue(ghb1,'formatCode').toInterpretedString()  ...
          '''. This script can only read ''32 bit IEEE demultiplexed '...
          'data, giving up...']);
    throw(me)
end

if(printhdr) 
    printHeader(ghb1);
end


%% General Header block 2
ghb2 = Headers(SEGD.fid,'generalHeader2');
disp (['** SEGD revision number: '...
       getHeaderValue(ghb2,'majorSEGDrevisionNumber').toString() ...
       '.' ...
       getHeaderValue(ghb2,'minorSEGDrevisionNumber').toString() ...
      ]);
disp (['** Sample rate is: ' dt ' ms']);

if(printhdr)
    printHeader(ghb2);
end

%% General Header blocks N
for i=1:getHeaderValue(ghb1,'generalHeaderBlocks').toNumber()-1;
   ghbN = Headers(SEGD.fid,'generalHeaderN');
   if (printhdr) 
       printHeader(ghbN);
   end
end

%% Scan Type Headers and Skew blocks
nTraces = 0;
for i = 1:getHeaderValue(ghb1,'scanTypesPerRecord').toNumber()
    %scan type headers
    for j = 1:getHeaderValue(ghb1,'channelSetsPerScanType').toNumber()

        sthN = Headers(SEGD.fid,'scanTypeHeader');       

        if(j==1)
            %This assumes all traces in file are the same length!!!
            %Needs more coding to handle variable trace lengths
            tEnd = getHeaderValue(sthN,'channelSetEndTime').toNumber();            
        end

        if(printhdr && getHeaderValue(sthN,'scanTypeNumber').toNumber()) 
            printHeader(sthN);
        end
        
        nTraces = nTraces ...
            +getHeaderValue(sthN,'numberOfChannelsThisSet').toNumber();


    end
    
    %Skew blocks (skip them; not certain I fully understand where these go 
    %exactly, but this is working for the test file, with zero skew blocks)
    for k=1:getHeaderValue(ghb1,'skewBlocks').toNumber();
        fseek(SEGD.fid,32,'cof');
    end
end

disp(['** Total number of traces expected in this file'...
      ' (all channel sets): ' num2str(nTraces)]);

%% Extended header blocks (skip them)
for i = 1:getHeaderValue(ghb1,'extendedHeaderBlocks').toNumber();
    fseek(SEGD.fid,32,'cof');
end

%% External header blocks (skip them)
for i = 1:getHeaderValue(ghb1,'externalHeaderBlocks').toNumber();
    fseek(SEGD.fid,32,'cof');
end

%% Do some thinking about output trace range
trace1=1;
trace2=nTraces;

%check user inputs
if(~isempty(traces))
    if(traces(1) > 0 && traces(1) <= nTraces)
        trace1 = traces(1);
    end
    
    if(traces(2) > 0 && traces(2) <= nTraces)
        trace2 = traces(2);
    end
end

disp(['** Reading sequential traces: ' num2str(trace1) ' to ' num2str(trace2)]);

%% Trace headers and trace data
traceInFile=1; traceInArray=1;
while (traceInFile<=nTraces)  
    %Is this a trace header??? It had better be...
    thN = Headers(SEGD.fid,'demuxTraceHeader');
    if(printhdr)
        printHeader(thN);
    end
    
    %Check if any trace header extensions exist
    nTrExt = getHeaderValue(thN,'traceHeaderExtension').toNumber();
    traceNo = getHeaderValue(thN,'traceNumber').toString();
    
    %Read at least one trace header extension
    ethN = Headers(SEGD.fid,'traceHeaderExtension');
    if(printhdr)
        printHeader(ethN);
    end    

    %Skip subsequent trace header extensions
    for i=1:nTrExt-1
        fseek(SEGD.fid,32,'cof');
    end

    nSamp = getHeaderValue(ethN,'numberOfSamplesPerTrace').toNumber();

    if(traceInFile==1)%pre-allocate memory, if first time through, and numberOfSamplesPreTrace
        trc = zeros(nSamp,length(trace1:trace2));
    end
    
    if (traceInFile >= trace1 && traceInFile <= trace2)
        %Read trace data
        if(printhdr)
            disp(['** Reading data for trace number ' traceNo '; trace '...
               num2str(traceInFile) ' in the file']);
        end
        trc(:,traceInArray) = fread(SEGD.fid,nSamp,'float');
        traceInArray = traceInArray+1;
    else
        %Skip this trace
        fseek(SEGD.fid,nSamp*4,'cof');
    end
    
    traceInFile=traceInFile+1;
    
    if (traceInFile > trace2)
        break;
    end
end

%% General Trailer blocks
for i = 1:getHeaderValue(ghb2,'generalTrailerBlocks').toNumber();
    gtN = Headers(SEGD.fid,'generalTrailer');
    if(printhdr)
        printHeader(gtN);
    end
end


%% Make a time vector
dt = str2double(dt)/1000;
t = 0.0:dt:tEnd/500;

%% Clean up
SEGD.closeFile();

end