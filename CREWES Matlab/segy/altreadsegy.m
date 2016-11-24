function [dataout, sampint, varargout] = altreadsegy(sgyfile, varargin)

% function [dataout, sampint, ...] = altreadsegy(sgyfile, 'property_name', property_value, ...)
%
% Reads segy data into a structured array format in Matlab workspace.
%
% dataout   = a 2-D array containing the data of interest
% sampint   = sample interval in seconds (optional)
%
% Optional properties
% 
% textheader
%   yes    - sends the text header to one of the output arguments
% 
% textformat
%   ascii  - force text header to be interpretted as ascii
%   ebcdic - force text header to be interpretted as ebcdic
%
% fpformat
%   ibm    - force floating point numbers to be interpreted as IBM360 floating point format (SEG-Y prior to 2002)
%   ieee   - force floating point numbers to be interpreted as IEEE floating point format (SEG-Y revision 2 (after 2002))
%            and some non-standard SEG-Y generating software -- particularly on PC systems
% traces
%   specify a vector of desired traces within the file.  The first trace is 1.  For example, to read every third trace:
%   traces = 1:3:ntraces
%
% nt
%  <number> - for non-compliant SEG-Y files which contain a missing or incorrect
%             number of samples per trace in the binary header, one can supply this
%             value.  Not recommended for typical use.
% traceheaders   - specify a headers to retrieve (possible header names: yes).  
%            A vector of header values is generated for each header word (indexed by trace).  
%            One output argument must be supplied for each desired header.  To obtain more than one header,
%            separate the header names with a comma. 
%            For example: [data,dt,th] = altreadsegy('foo.sgy','traceheaders','raw')
% binaryheader 
%   yes    - sends the binary header to one of the output arguments
%
% The following properties DO NOT WORK YET but are planned:
% times    - specify the desired time window: [starttime endtime]  (both in fractional seconds)
% depths   - specify the desired depth window: [startdepth enddepth]  (both in fractional seconds)
% timevector - return a vector of times corresponding to samples in dataout.
%
% Quick-start:  If no arguments are supplied, or the filename is empty ('')
% you will be prompted for a SEG-Y file to read.  Want to view a SEG-Y file
% in a hurry?  Just type: plotseis(altreadsegy)
%
%
% Examples:
%
% Read and display all the seismic data:
%  dataout = altreadsegy('foo.sgy');
%  plotseis(dataout);  
%
% Read the data, display it with the correct time scale, and view the text header:
%  [dataout, sampint, textheader] = altreadsegy('foo.sgy','textheader','yes');
%  t = 0:sampint:(size(dataout,1)-1)*sampint;
%  plotseis(dataout,t); disp(textheader);
%
% Troubleshooting advise:  add 'verbose','yes' to your argument list like
% this:
%  dataout = altreadsegy('foo.sgy','verbose','yes');
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

property_strings = ...
    {'textformat','fpformat','segfmt','traces','times','depths','textheader','traceheaders','binaryheader','verbose','nt'};

fixedargs = 2;
nvarargout = nargout - fixedargs;
argout=0;

verbose = FindValue('verbose',property_strings,varargin{:});

if (nargin == 0 || (nargin>=1 && strcmp(sgyfile,'')~=0))
        [name,path]=uigetfile('*.SGY');
        sgyfile = fullfile(path,name);
end


if isempty(sgyfile)
    [fn,pth] = uigetfile('*.sgy');
    sgyfile = fullfile(pth,fn);
    
end

if (~ischar(sgyfile))
	error('First argument must be a file name');
end

fileinfo = dir(sgyfile); % Fileinfo is a structured array of file information.

if isempty(fileinfo)
    error(['The file ', sgyfile, ' does not exist.'])
end

fileinfo.bytes; % Pull the size of the file out of fileinfo.

gotDataFormat = 0;
byteOrder = 'be';   % Big-endian byte order

while ~gotDataFormat
	fid = fopen(sgyfile, 'r', ['ieee-' byteOrder]);% Open the segy file for reading.
	
	if fid == -1
        error('Unable to open file.')
	end
	
	% Read the segy file text header.  Even if the user doesn't want
    % the text header returned, we still load it and analyse it. 
    % It will give us a clue about floating point format later on.
	textheader = char(reshape(fread(fid, 3200, 'uchar'), 80, 40))';
    isEbcdic = FindValue('textformat', property_strings, varargin{:});
    if isempty(isEbcdic)
          % Convert from EBCDIC to ASCII if appropriate.
          % EBCDIC headers have byte values greater than 127 within them.
        isEbcdic = length(find(textheader > 127)) > 0;
        if verbose > 1
            disp('guessing the text header is ebcdic');
        end
    else
        switch isEbcdic
        case 'ebcdic'
               isEbcdic = 1;
        case 'ascii'
               isEbcdic = 0;
        otherwise
             error('Invalid text format specified.  Allowed values: ascii, ebcdic');
        end
	end
		
	if isEbcdic
        textheader = ebcdic2ascii(textheader);
    end

  	wantTextHeader = BooleanValue(FindValue('textheader',property_strings,varargin{:}));
    if wantTextHeader
        argout = argout + 1;
        if argout > nvarargout
            error('Not enough output arguments to store text header');
        end; 
        varargout(argout) = {textheader};
    end

	
	% Read out the information from the binary header that is typically available.
	% Header descriptions are in header.m.
	binpart1 = fread(fid, 3, 'int32'); % First section of the binary header.
	binpart2 = fread(fid, 24, 'int16'); % Second section of the binary header.
	
	binaryheader = [ binpart1; binpart2];

  	wantBinaryHeader = BooleanValue(FindValue('binaryheader',property_strings,varargin{:}));
    if wantBinaryHeader
        argout = argout + 1;
        if argout > nvarargout
            error('Not enough output arguments to store binary header');
        end; 
        varargout(argout) = { binaryheader };
    end
    
    
    segfmt = binaryheader(10);
	
	fpformat = FindValue('fpformat', property_strings, varargin{:});
	
	switch segfmt
	case 1
      % If the text is ascii, there's a good chance the data is IEEE and not IBM floating point
      % (If you're going to ignore the standard in one place, chances are you'll ignore it in other places).
      if isempty(fpformat)
         if isEbcdic
            fpformat='ibm';
         else
            fpformat='ieee';
         end
      else
         if ~strcmp(fpformat,'ieee') & ~strcmp(fpformat,'ibm')
            error('Floating point format must be "ieee" or "ibm"');
         end
      end
      dformat = 'float32'; % 4 bytes, should be IBM floating point
      bytesPerSample=4;
      gotDataFormat=1;
	case 2
      dformat = 'int32'; % 4 bytes, signed.
      bytesPerSample=4;
      gotDataFormat=1;
	case 3
      dformat = 'int16'; % 2 bytes, signed.
      bytesPerSample=2;
      gotDataFormat=1;
	case 4
      error('Can not read this format. (Fixed point with gain code.)');
	case {5, 6}
      if isempty(fpformat) 
          fpformat='ieee';
      end
      dformat = 'float32'; % 4 bytes presumably IEEE floating point
      bytesPerSample=4;
      gotDataFormat=1;
	case 8
      dformat = 'uchar8'; % 4 bytes presumably IEEE floating point
      bytesPerSample=1;
      gotDataFormat=1;
	otherwise
      if strcmp(byteOrder,'be') 
          firstTimeSegFmt = segfmt;
          byteOrder = 'le';
          % We'll loop due to gotDataFormat being false
      else
          % Tried big-endian and little-endian byte orders, and still the format looks bogus.
          error(['Invalid data format contained within SEG-Y file.  Allowable values: 1,2,3,4,5,8.  Got: ', num2str(firstTimeSegFmt), ' as big-endian, and ' num2str(segfmt), ' as little-endian']);
      end 
	end

end
   
sampint = binaryheader(6) / 1e6; % Sample interval in seconds (was microseconds)

numsamps = FindValue('nt', property_strings, varargin{:});
if isempty(numsamps) 
  numsamps = binaryheader(8); % Number of samples.
  if verbose
      disp(['number of samples per trace (from bytes 3221-3222) is ' num2str(numsamps)]);
  end 
  if numsamps < 1
    %peek ahead in the first trace header to find numsamps
    [th ,st] = fread(fid, 120, 'uint16');
    if st ~= 120
        error('File is too short to be a valid SEG-Y file - it must have got truncated');
    end
    fseek(fid, -240, 'cof');   % zip back to where we were before the previous fread
    numsamps = th(floor(115/2) + 1);
    if verbose
        disp('Hmm.  That can not be right, lets look in bytes 115-116 of the first trace.');
        disp(['Well, that got us ' num2str(numsamps) ' samples per trace']);
    end
  end
end

if numsamps < 1
  error('Unable to determine the number of samples per trace - file is corrupted or non-compliant with the SEG-Y standard. Try overriding with <numsamps> parameter.');
end


if verbose > 1
    disp(['using seg-y format number ' num2str(segfmt) ' - reading as ' dformat]);
    disp(['fpformat is ' fpformat]);
end

% Check to see if the entire data set is to be read in or not.
traces = FindValue('traces',property_strings,varargin{:});
if isempty(traces)
    numtraces = floor((fileinfo.bytes-3600)/(240+(bytesPerSample*numsamps)));
    traces = 1:numtraces;
else
    numtraces = length(traces);
end 

numheaders=120;
dataout=zeros(numsamps,numtraces);
traceheaders = zeros(numheaders,numtraces);


for n=1:numtraces
    pos = 3600+((traces(n)-1)*(240+(bytesPerSample*numsamps)));
    st = fseek(fid, pos,-1);
    if st ~= 0 
        break
    end
    [th ,st] = fread(fid, 120, 'uint16');
    if st ~= 120
        if verbose > 1 
            disp(['File appears to be truncated after ' num2str(n) ' traces']);
        end
        break;
    end
    traceheaders(:, n) = th;
    
    if strcmp(fpformat,'ibm')
	    [dataout(:,n) , st] = fread(fid, numsamps, 'uint32');
        if st ~= numsamps
            break;
        end 
        dataout(:,n) = ibm2ieee(dataout(:,n));
  	else
	    [tmp, st] = fread(fid, numsamps, [dformat '=>double']);
        if st ~= numsamps
            break
        end
        dataout(:,n) = tmp;
    end
end

fclose(fid);

wantTraceHeaders = BooleanValue(FindValue('traceheaders',property_strings,varargin{:}));
if wantTraceHeaders
    argout= argout + 1;
    if argout > nvarargout
        error('not enough output arguments to store traceheaders');
    end
    varargout(argout) = {traceheaders};
end



% -----------------------
function value = FindValue( property_name, property_strings, varargin )

value = [];
for i = 1:((nargin-2)/2)
    current_name = varargin{2*i-1};
    if ischar(current_name)
        imatch = strmatch(lower(current_name),property_strings);
        nmatch = length(imatch);
        if nmatch > 1
                error(['Ambiguous property name ' current_name '.']);
        end
        if nmatch == 1
            canonical_name = property_strings{imatch};
            if strcmp(canonical_name, property_name)
                if isempty(value)
                    if isempty(varargin{2*i})
                        error(['Empty value for ' property_name '.']);
                    end
                    value = varargin{2*i};
                else
                    error(['Property ' property_name ' is specified more than once.']);
                end
            end
        end
    end
end
%---------------------

function value = BooleanValue(x)

if ischar(x) 
    x = lower(x);
    value = strcmp(x,'yes') | strcmp(x, 'on') | strcmp(x, 'true');
elseif ~isempty(x) & isnumeric(x) 
    value = (x ~= 0);
else
    value = 0;
end