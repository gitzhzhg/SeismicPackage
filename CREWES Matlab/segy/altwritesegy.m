function altwritesegy(sgyfile, datain, sampint, numsamps, numtraces, segfmt, byteorder, chrhdr, binhdr, trchdr)
% altwritesegy - a platform independent SEG-Y file writer
%
% This is a special version of writesegy meant for use with synth and logsec.
%
% segfmt should generally be 5, for IEEE floating point data generation
%
% function altwritesegy(sgyfile, datain, sampint, numsamps, numtraces, segfmt, byteorder)
%
%   sgyfile   - filename (should end in .sgy and this will be afixed if not)
%   datain    - 2-D matrix of samples indexed by (sample, trace)
%   sampint   - sample interval in seconds.
%   numsamps  - number of samples per trace (optional) ok to supply []
%   numtraces - number of traces (optional) ok to supply []
%   segfmt    - output segfmt (optional, default=5)
%               1=IBM floating point, 2=4-byte integer, 3=2-byte integer, 5=IEEE floating point
%   byteorder - output byte order (optional, default=be)
%               'be' = big-endian (standard), 'le' = little-endian (not conformant to standard,
%                                                    used by bad PC implementations)
%   chrhdr    - character (EBCDIC) header.  Gets converted from ASCII to EBCDIC (optional). 
%   binhdr    - binary header (optional)
%   trchdrs   - trace headers (optional)
%
% Example:
%  datain = ones(1024,10)   % 10 traces of 1024 samples
%  altwritesegy('file.sgy', datain, 0.002);
% Example read a SEG-Y file and retain headers.  Just modify data:
%   [td,si,ch,bh,th] = altreadsegy('input.sgy','textheader','yes','binaryheader','yes','traceheaders','yes');
%   altwritesegy('output.sgy',td,si,[],[],[],[],ch,bh,th);
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
   error('Invalid number of arguments: at least 3 required (segyfile, datain, sampint)');
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

if nargin < 8 || isempty(chrhdr)
    chrhdr = [];
end

if nargin < 9 || isempty(binhdr)
    binhdr = [];
end

if nargin < 10 || isempty(trchdr)
    trchdr = [];
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

%check file name for trailing .sgy and affix if needed
i1=strfind(sgyfile,'.sgy');
if(isempty(i1))
    %check for .segy
    i1=strfind(sgyfile,'.segy');
    if(isempty(i1))
        sgyfile=[sgyfile '.sgy'];
    end
end


[fid, errmsg] = fopen(sgyfile, 'w', platform);

if (fid == -1) 
    error(['Unable to create ' sgyfile ': ' errmsg] );
end

switch segfmt
case 1
  dformat = 'float32'; % 4 bytes, should be IBM floating point
case 2
  dformat = 'int32'; % 4 bytes, signed.
case 3
  dformat = 'int16'; % 2 bytes, signed.
case 4
  error('Can not write this format. (Fixed point with gain code.)');
case 5
  dformat = 'float32'; % 4 bytes presumably IEEE floating point
case 8
  dformat = 'uchar8'; % 4 bytes presumably IEEE floating point
otherwise
  error(['invalid format specified: ', num2str(segfmt)]);
end

if isempty(chrhdr)
    % Build a text header
    chrhdr{1} = char(['File generated ' datestr(now)]);

    switch segfmt
    case 1
      chrhdr{2} = 'IBM Floating point fmt (not implemented)';
    case 2
      chrhdr{2} = '4-byte integer format';
    case 3
      chrhdr{2} = '2-byte integer format';
    case 4
      chrhdr{2} = 'Fixed point w/gain code format';
    case 5
      chrhdr{2} = 'IEEE Floating point format';
    case 8
      chrhdr{2} = '1-byte integer format';
    otherwise
      error(['invalid format specified: ', num2str(segfmt)]);
    end

    chrhdr{39} = 'SEG Y rev1';    % ooh, bleeding edge
    chrhdr{40} = 'END EBCDIC';    %
    % Next line should contain 80 spaces
    for i=1:40
        if isempty(chrhdr{i})
            chrhdr{i} = ' ';
        end
        chrhdr{i} = sprintf('C%2d %s',i, chrhdr{i}); 
    end    
end
if iscell(chrhdr) 
    chrhdr = char(chrhdr);
    chrhdr(41,81) = 'X';
    chrhdr(41:end,:) = [];
    chrhdr(:,81:end) = [];    
end
chrhdr = ascii2ebcdic(chrhdr);
% Write the text header.
if size(chrhdr,1) < size(chrhdr,2)
    chrhdr = chrhdr';  % rotate it so it's 40 rows of 80 columns
end
count = fwrite(fid, chrhdr, 'uchar');

% Check if writing went successfully.
if count ~= 3200
    error(['EBCDIC header is too short. Size is only ', num2str(count), '.']);
end

% Now fill in the binary header, use any supplied defaults.
if isempty(binhdr)
    binhdr = zeros(27,1);
end

if(length(sampint)>1)
    if(length(sampint)==size(datain,1))
        sampint=sampint(2)-sampint(1);%in case a t vector was input
    else
        error('Sample interval should be a scalar value in seconds');
    end
end

if sampint <= 0
    error('Sample interval must be greater than zero');
end

if isnan(sampint) || isinf(sampint)
    error('Sample interval cannot be NaN or Inf');
end

binhdr(6) = sampint*1000000;

% Gracefully handle cases where the sample interval will overflow the 16-bit storage limit
% of SEG-Y.  Knock long sample intervals down by a factor of 1000.

while binhdr(6) > 65535
   binhdr(6) = binhdr(6) / 1000;
end

binhdr(8) = numsamps;
binhdr(10) = segfmt;

if (fwrite(fid, binhdr(1:3), 'ulong') ~= 3) | ...
    (fwrite(fid, binhdr(4:27), 'ushort') ~= 24) | ...
    (fwrite(fid, zeros(170,1), 'ushort') ~= 170) ;

    error('The binary header has been truncated.')
end


% Write out the data traces and trace headers.
% Other initializations needed for the while loop.

if isempty(trchdr)
    trchdr = zeros(120,numtraces);
end

trchdr(floor((29+1)/2),:) = ones(1,numtraces);   % seismic data 
trchdr(floor((35+1)/2),:) = ones(1,numtraces);   % production data
trchdr(floor((115+1)/2),:) = ones(1,numtraces) * numsamps;  % number of samples for this trace

for i = 1:numtraces
    count = fwrite(fid, trchdr(:,i), 'ushort'); % 'ushort'
    if count ~= 120
        error(['Problem writing trace ' , num2str(i), '.']);
    end
    count = fwrite(fid, datain(:, i), dformat);
    if count ~= numsamps
       error(['Problem writing trace ', num2str(i), '.'])
    end
end

fclose(fid);