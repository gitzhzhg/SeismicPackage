function [seismic,text_header,binary_header]=read_segy_file(filename,varargin)
% Function reads a SEG-Y file and outputs a seismic structure; the function
% assumes that all traces of the SEG-Y file have the same length (this is 
% usually the case for data that come from a seismic processing system but may 
% not be true for data from a field-recording system).
%
% Written by: E. Rietsch: November 25, 2006
% Last updated: January 7, 2007: Fix bug in IBM ==> IEEE conversion
%
%             [seismic,ebcdic_header,binary_header]=read_segy_file(filename,varargin)
%
% INPUT       (all input arguments are optional)
% filename    name of the  file to read
%             if the name is omitted, empty, or a file with this name is not found
%             a file selector box will pop up to allow interactive file selection
% varargin    Variable number of arguments. Each argument is a cell array whose first
%             element is a keyword and whose other elements can be strings, numeric
%             values, or other cell arrays
%             Possible keywords are:
%     format  floating point format to use; this cell array has the form
%             {'format',fp-format} where fp_format is one of the three strings:
%             'ibm'      IBM floating point format (standard and default)
%             'ieee'     IEEE format, big endian  (Sun, SGI, etc.)
%             'header'   The binary header bytes 25-26 are used to determine the format
%             Default: {'format','header'}; 
%     headers  header values to be read from binary trace header; this cell array has
%             the form {'headers',{mnem1,first,bytes,units,description}, ...
%             {mnem2,first,bytes,units,description},...} where
%             "mnem1", "mnem2", ... denote header mnemonics (such as CDP, OFFSET), 
%             "first" denotes the first byte in the binary header, 
%             "bytes" denotes the number of bytes occupied by the mnemonic (2 or 4), 
%             "units" denotes the units of measurement for the header value, and 
%             "description" is a description of the header value. 
%             Example:
%             {'headers',{'ILINE_NO',181,4,'n/a','CDP number'}, ...
%                        {'OFFSET',37,4,'m','Source-receiver distance'}}
%             See below for a list of headers retrieved by default.
%             Default: {'headers',{}}
%     times   times to output; this cell array has the form 
%             {'times',first,last}, {'times',[first,last]}, or {'times',[]} 
%             In the first two forma all samples with times between (and 
%             including) first and last (in ms) are output. In the last case
%             all samples are output.
%             Default: {'times',[]}
%     traces  select traces to output; this cell array has the form
%             {'traces',expression}; the variable "expression" is used to 
%             determine which traces to output. "expression" can be an index
%             vector specifying the traces to output
%             Examples: {'traces',1:2:100}
%                       {'traces',[2,5,7:10,22]}
%             Alternatively, it can be a string with a logical expression involving 
%             trace headers such as '10 <= cdp && 100 >= cdp'
%             Examples: {'traces','cdp == 100 && offset > 100'}
%                       {'traces','14000 < cdp  && (14660 >= cdp || 14680 <= cdp)'};
%             The variables in the logical relationships must be headers of the 
%             data set; Use of functions "fix", "mod", and "round" are permitted; all
%             other function names will be interpreted as headers and likely cause an
%             error; the case of the headers in an expression does not matter.
%             Default: {'traces',[]}
%    ignoreshift  By default this function reads byte locations 109-110 (see 
%             header "lag" below) and applies the shift to the seismic data;
%             This behavior can be overwritten by setting this parameter to true;
%             Default: {'ignoreshift',false}
%    max_mem   maximum amount of contiguous memory in megabytes (MB) bytes available 
%             to store seismic traces;
%             Default: {'max_mem',[]}
%             This means the maximum size is determined internally. 
%
% Headers retrieved by default are (any one of these headers is removed if it turns out to 
% be identically zero):
%        ds_seqno      Trace sequence number within line (1-4)
%        ffid          Original Field record number (9-12)
%        o_trace_no    Trace sequence number within original field record (13-16)
%        source        Energy source point number (17-20)
%        cdp           CDP ensemble number (21-24)
%        seq_cdp       Trace sequence number within CDP ensemble (25-28)
%        trc_type      Trace ID (1=live,2=dead,3=dummy,4=time break,...) (29-30)
%        offset        Distance from source point to receiver group (37-40)
%        rec_elev      Receiver elevation (41-44);
%        sou_elev      Surface elevation at source (45-48)
%        depth         Source depth below surface (49-52)
%        sou_h2od      Water depth at source (61-64)
%        rec_h2od      Water depth at receiver group (65-68)
%        sou_x         X coordinate of source (73-76)
%        sou_y         Y coordinate of source (77-80)
%        rec_x         X coordinate of receiver (81-84)
%        rec_y         Y coordinate of receiver (85-88)
%        lag           Lag time between shot and recording start in ms (109-110)
%                      (the value of lag is added to the start time of the 
%                      seismic; hence it can be used to simulate non-zero start
%                      time of the data)
%                      see also parameter "ignoreshift", above.
%        cdp_x         X coordinate of CDP (181-184)
%        cdp_y         Y coordinate of CDP (185-188)
%        iline_no      In-line number (189-192)
%        xline_no      Cross-line number (193-196)
%               The numbers in parentheses at the end of the line denote the location 
%               of the corresponding bytes in the SEG-Y trace header
%    
% OUTPUT
% seismic       Seismic structure
%       seismic.type               'seismic' (type of structure)
%       seismic.name               file name without extension
%       seismic.from               Full name of the SEG-Y file
%       seismic.traces             Array of seismic traces
%       seismic.first              Start time of seismic (in ms)
%       seismic.last               End time of seismic (in ms)
%       seismic.step               Sample interval of seismic (in ms)
%       seismic.units              Time units used (ms)
%       seismic headers            Matrix with header mnemonics (one row 
%                                  per header)
%       seismic.header_info        Three-column cell array with header info 
%                                  (one row per header)
%       seismic.null               []
%       seismic.line_number        Line number (5-8)
%       seismic.reel_number        Reel number (9-12)
%       seismic.cdp_fold           CDP fold
%       seismic.traces_per_record  Data traces per record (13-14)
%       seismic.aux_per_record     Auxiliary traces per record (15-16)
%       seismic.history            A four element cell array. The first element
%                                  is the start date/time of the program that 
%                                  invoked this function; the second element is
%                                  the start date/time this function was executed;
%                                  and the last cell contains the name if the file 
%                                  that was read
%            Example:
%               seismic.offset     contains the offset for each trace
%               seismic.header_info.cdp two-element cell array {'m','Offset'}
%                                  the first element represents the units of measurement, the 
%                                  second is a description of the header
%                 
% ebcdic_header         EBCDIC reel header converted to ASCII
% binary_header         Binary reel header

global PARAMETERS4FUNCTION

%	Set default output arguments
run_presets_if_needed

%	Set default for input parameters
param.format='header';
param.headers={};
param.debug=false;
param.ignoreshift=false;
param.times=[];
param.traces=[];
param.header_precision='single';
param.max_mem=[];   

if nargin == 0
   filename='';

elseif nargin > 1

%	Replace defaults by actual input arguments
   PARAMETERS4FUNCTION.open_segy_file4reading.default=assign_input(param,varargin);

end

%	Open SEG-Y file and get info from textual and binary headers
[fid,seismic,param,parameters,text_header,binary_header] = ...
                               open_segy_file4reading(filename);

if fid < 0
   alert('No file selected.')
   return
end

%	Read traces
[seismic,headers]=read_traces_of_segy_file(fid,seismic,param,parameters,param.ntraces);

%	Close file
fclose(fid);

if isempty(seismic.traces);
   disp(['No seismic traces read from file "',seismic.from,'".'])
   seismic=[];
   return
end

if any(isnan(seismic.traces(:)))
   seismic.null=NaN;
else
   seismic.null=[];
end

%	Check headers, remove unnecessary ones, etc.
seismic=finalize_seismic_dataset(seismic,headers,param,parameters);
