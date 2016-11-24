function altappendsegy(sgyfile, datain, sampint, numsamps, numtraces,...
    segfmt, byteorder, trchdrs, renumber)
% altappendsegy - a platform independent SEG-Y file writer
%
% This is a special version of altappendsegy created for a sponsor in 2015.
%
% segfmt should generally be 5, for IEEE floating point data generation
%
% function altappendsegy(sgyfile, datain, sampint, numsamps, numtraces, ...
%    segfmt, byteorder, trchdrs, renumber)
%
%   sgyfile   - filename (should end in .sgy)
%   datain    - 2-D matrix of samples indexed by (sample, trace)
%   sampint   - sample interval in seconds.
%   numsamps  - number of samples per trace (optional) ok to supply []
%   numtraces - number of traces (optional) ok to supply []
%   segfmt    - output segfmt (optional, default=5)
%               1=IBM floating point, 2=4-byte integer, 3=2-byte integer, 5=IEEE floating point
%   byteorder - output byte order (optional, default=be)
%               'be' = big-endian (standard), 'le' = little-endian (not conformant to standard,
%                                                    used by bad PC implementations)
%   trchdrs   - trace headers (optional)
%   renumber  - renumber Trace Sequential Number in trace headers
%               1 = yes (default), 0 = no. ok to supply []
%
% Examples:
%  altappendsegy('file.sgy', datain, 0.002);
%  altappendsegy('output.sgy',datain,sampint,[],[],[],[],th,1);
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

if nargin < 3 
   error('crewes:altappendsegy',...
       'Invalid number of arguments: at least 3 required (segyfile, datain, sampint)');
end
if nargin < 4 || isempty(numsamps)
   numsamps = size(datain,1);
end
if nargin < 5 || isempty(numtraces)
   numtraces = size(datain,2);
end
if nargin < 6 || isempty(segfmt)
   segfmt = 5;
end
if nargin < 7 || isempty(byteorder)
   byteorder = 'be';
end
if nargin < 8 || isempty(trchdrs)
    trchdrs = [];
end
if nargin < 9 || isempty(renumber)
    renumber = 1;
end

% Check if file already exists
if ~isequal(exist(sgyfile,'file'),2)
    error('crewes:altappendsegy',['File ' sgyfile ' does not exist']);
end

% Open or create the file for writing.
switch lower(byteorder)
case {'be', 'bigendian', 'ieee-be'}
   platform='ieee-be';
case {'le', 'littleendian', 'ieee-le'}
   platform = 'ieee-le';
otherwise
   error(['Invalid byte order parameter: ' byteorder]);
end

% Read basic info about existing SEGY file
[trace1,outfileSampInt,txtH,binH,traceH]=altreadsegy(sgyfile,'traces',1:3,'textheader','yes','binaryheader' ,'yes','traceheaders','yes');
outfileNumSamp = length(trace1);
segyFmt = binH(10);

if ~isequal(outfileNumSamp-numsamps, 0.0)
    error('crewes:altappendsegy', ...
        ['Trace(s) to append have a different number of samples ('...
         num2str(numsamps)  ') than the existing output file ' ...
         '(' num2str(outfileNumSamp),')' ]);
end
if ~isequal(outfileSampInt-sampint, 0.0)
    error('crewes:altappendsegy', ...
        ['Desired output sample rate (' num2str(sampint) ...
         ') does not match the existing output file (' ...
         num2str(outfileSampInt) ')' ]);
end
if ~isequal(segyFmt-segfmt, 0.0)
    error('crewes:altappendsegy', ...        
        ['Desired output SEG-Y format (' num2str(segfmt) ...
         ') does not match the existing output file (' ...
         num2str(binH(10)) ')' ]);    
end

% Open output file for read/write
[fid, errmsg] = fopen(sgyfile, 'a+', platform);

if (fid == -1) 
    error(['Unable to open ' sgyfile 'for read/write: ' errmsg] );
end

switch segfmt
case 1
  dformat = 'float32'; % 4 bytes, should be IBM floating point
  bytesPerSamp = 4;
case 2
  dformat = 'int32'; % 4 bytes, signed.
  bytesPerSamp = 4;
case 3
  dformat = 'int16'; % 2 bytes, signed.
  bytesPerSamp = 2;
case 4
  fclose(fid);
  error('crewes:altappendsegy','Cannot write this format. (Fixed point with gain code.)');
case 5
  dformat = 'float32'; % 4 bytes presumably IEEE floating point
  bytesPerSamp = 4;
case 8
  fclose(fid);
  error('crewes:altappendsegy','Cannot write this format. (1-byte, twos''s complement integer)');
 % dformat = 'uchar8'; % 1 byte twos's compliment
 % bytesPerSamp = ?
otherwise
  error(['invalid format specified: ', num2str(segfmt)]);
end

%Rewind one trace, advance 4 bytes
bytesPerTraceHdr = 240;
bytesPerTrace = numsamps*bytesPerSamp;
startByte = 4; %bytes 5-8 (starting at 1) = trace sequence number in SEG-Y file, Matlab starts counting at 0
fseek(fid,-bytesPerTraceHdr-bytesPerTrace+startByte,'cof');

%Read trace header for trace sequence number in file
lastTraceInFile=fread(fid,1,'uint32');

%Find end of file again
fseek(fid,0,'eof');

if ~isempty(trchdrs) && renumber
    %renumber trace sequential number in file in the trace headers
    for ii = 1:numtraces %stored in four bytes rather than two    
        trchdrs(3:4,ii)   = four2two(lastTraceInFile+ii,byteorder);
    end
end

if isempty(trchdrs)
    trchdrs = zeros(120,numtraces);
    for ii = 1:numtraces %stored in four bytes rather than two    
        trchdrs(3:4,ii)   = four2two(lastTraceInFile+ii,byteorder);
    end
    trchdrs(floor((29+1)/2),:)  = ones(1,numtraces);   % seismic data
    trchdrs(floor((35+1)/2),:)  = ones(1,numtraces);   % production data
    trchdrs(floor((115+1)/2),:) = ones(1,numtraces) * numsamps;  % number of samples for this trace
end

%Last bit of error checking
%Is the first trace number in the input trace headers sequential from the
%last trace number in the file
firstTraceInOutput=two2four(trchdrs(3:4,1),byteorder);
lastTraceInOutput=two2four(trchdrs(3:4,numtraces),byteorder);

if ~isequal((firstTraceInOutput-lastTraceInFile),1)
    fclose(fid);
    error('crewes:altappendsegy', ...        
        ['Last trace number in file trace headers (' num2str(lastTraceInFile) ...
         ') is not sequential with the first trace of the output trace headers (' ...
         num2str(firstTraceInOutput) '). Consider using the renumber option' ]); 
end    
%overflow
if lastTraceInOutput > intmax('uint32')
    fclose(fid);
    error('crewes:altappendsegy', ...        
        ['Append operation would result in more traces ('...
         num2str(lastTraceInOutput) ') than can be stored in SEG-Y format ('...
         num2str(intmax('uint32')) ')']);     
end

%All right, carry on!
for ii = 1:numtraces
    count = fwrite(fid, trchdrs(:,ii), 'ushort'); % 'ushort'
    if count ~= 120
        error(['Problem writing trace ' , num2str(ii), '.']);
    end
    count = fwrite(fid, datain(:, ii), dformat);
    if count ~= numsamps
       error(['Problem writing trace ', num2str(ii), '.'])
    end
end

fclose(fid);

end

function ohdr = four2two (ihdr,byteorder)
    %ihdr is a double
    switch byteorder
        case 'le'
            ohdr = typecast(uint32(ihdr),'uint16')';
        case 'be'
            ohdr = flipud(typecast(uint32(ihdr),'uint16')');
        otherwise
    end
end

function ohdr = two2four (ihdr,byteorder)
    %ihdr is a uint16 stored as a double
    switch byteorder
        case 'le'
            ohdr = typecast(uint16(ihdr),'uint32');
        case 'be'
            ohdr = typecast(uint16(flipud(ihdr)),'uint32');
        otherwise
    end
end