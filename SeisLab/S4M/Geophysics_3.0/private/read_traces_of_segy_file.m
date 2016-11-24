function [seismic,headers]=read_traces_of_segy_file(fid,seismic,param,parameters,max_no_of_traces)
% Read traces of the open SEG-Y file with file ID "fid". The file must have
% been opened with function "open_segy_file4reading". This function also 
% provides the input arguments for this function. 
% For internal use in "read_segy_file" and "read_segy_file_traces".
%
% Written by: E. Rietsch: January 7, 2007
% Last updated: April 5, 2009: Account for single-precision memory requirements
%
%        [seismic,headers]=read_traces_of_segy_file(fid,seismic,param,parameters,max_no_of_traces)
% INPUT
% fid    file identifier of the SEG-file
% seismic
% param
% parameters
% max_no_of_traces  maximum number of traces to read
% OUTPUT
% seismic  input seismic dataset with headers and traces read from file

% UPDATE HISTORY
%         November 7, 2008: Fix typo in function name
%         November 19, 2008: Fix typo on line 37 (Ravi)


param.ntraces=max_no_of_traces;

%	Reserve space for traces and headers
[ntraces,isall,param]=calculate_space_requirements_no8(param);

%	Read headers and trace values
if isall	% All requested traces fit into memory
   [seismic.traces,headers]=read_headers_and_traces_no9(fid, ...
           parameters.idx4times,ntraces, ...
           parameters.indices,parameters.true4four,param,parameters.constraint_info);

elseif ischar(param.traces)  % Possibly not enough memory for all traces; but
                             % logical constraints on traces to read may allow
			                    % one to read all requested traces
   [seismic.traces,headers]=read_headers_and_traces_no9(fid,parameters.idx4times, ...  % Ravi
               ntraces,indices,true4four,param,constraint_info);
   if size(seismic.traces,2) == ntraces
      mywarning('It is possible that not all requested seismic traces have been read.')
   end
%   fclose(fid);

else
   disp(' Insufficient memory to read all requested traces.')
   disp([' At most ',num2str(ntraces),' can be read.'])
   if ~ispc
      disp('Check if the memory for traces (keyword "max_mem") can be increased.')
   end
   fclose(fid);
  
   error('Abnormal termination.')
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [ntraces,isall,param]=calculate_space_requirements_no8(param)
% Compute the amount of space required to store headers and traces

global S4M

if ~isempty(param.max_mem)
   param.max_mem=fix(param.max_mem*131072);
else
   if ispc 
      switch S4M.precision
         case 'double'
            param.max_mem=fix(memblock/8); % Greatest memory block available           
         case 'single'
            param.max_mem=fix(memblock/4); % Greatest memory block available
         otherwise
            error(['Unknown precision: ',S4M.precision])
      end
      
   else
      param.max_mem=4.0e9;
   end
end

if isempty(param.traces)  || ischar(param.traces)
   if param.ntraces*param.nsamp <= param.max_mem
      ntraces=param.ntraces;
      isall=true;

   else
      ntraces=fix(param.max_mem/param.nsamp);
      if ntraces == 0
         error(' Not enough memeory for even a single trace.')
      end
      isall=false;
   end

elseif isnumeric(param.traces)
   param.traces=param.traces(param.traces > 0 & param.traces <= param.ntraces);
   ntraces=length(param.traces);
   if ntraces*param.nsamp <= param.max_mem
      isall=true;
   else
      ntraces=fix(param.max_mem/param.nsamp);
      isall=false;
   end

else
   error('Illegal parameter for keyword "traces".')

end   
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [traces,headers]=read_headers_and_traces_no9(fid,idx4times,ntraces, ...
                 indices,true4four,param,constraint_info)
% Read headers and traces
% INPUT
% fid       file identifier
% idx4times index of the time samples to keep
% ntraces   maximum number of traces to read
% param     input parameters for read_segy_file 
% OUTPUT
% traces    matrix with traces read
% headers   integer matrix with headers saved


headers=zeros(param.nheaders,ntraces,'int32');

precision=param.precision;

iloc=ftell(fid);

if strcmpi(param.format,'ibm')
   traces=zeros(param.nsamp,ntraces,'uint32');
else
   traces=zeros(param.nsamp,ntraces,'single');
end

%	No trace constaints; read all traces
if isempty(param.traces) 
   for ii=1:ntraces
      hh4=fread(fid,60,'int32');
      fseek(fid,iloc,'bof');
      hh2=fread(fid,120,'int16');
      if isempty(hh2)  % End of file reached
         traces(:,ii:end)=[];
	 headers(:,ii:end)=[];
	 break
      else
         headers(true4four,ii)=hh4(indices(true4four));
      end
      headers(~true4four,ii)=hh2(indices(~true4four));
      temp=fread(fid,param.no_samples,precision);  
      traces(:,ii)=temp(idx4times);
      iloc=ftell(fid);
   end

%	Traces to read are specified via trace numbers
elseif isnumeric(param.traces)
   inc=240+4*param.no_samples;
   iloc0=iloc;
   for ii=1:ntraces
      iloc=iloc0+(param.traces(ii)-1)*inc;
      fseek(fid,iloc,'bof');
      hh4=fread(fid,60,'int32');
      fseek(fid,iloc,'bof');
      hh2=fread(fid,120,'int16');
      headers(true4four,ii)=hh4(indices(true4four));
      headers(~true4four,ii)=hh2(indices(~true4four));
      temp=fread(fid,param.no_samples,precision);  
      traces(:,ii)=temp(idx4times);
   end

%	Traces to read are defined via a logical expression
else
   ik=0;
   temp_headers=zeros(param.nheaders,1,'int32');
   for ii=1:param.ntraces
      hh4=fread(fid,60,'int32');
      fseek(fid,iloc,'bof');
      hh2=fread(fid,120,'int16');
      temp_headers(true4four)=hh4(indices(true4four));
      temp_headers(~true4four)=hh2(indices(~true4four));
      temp=fread(fid,param.no_samples,precision);  
      if requested_header_no11(temp_headers,constraint_info,ii,param.traces)
         ik=ik+1;
	 if ik > ntraces
	    disp(' At least one more trace meets trace constraints but cannot be stored because')
	    disp(' of memory limitation.')
	    mywarning('Trace reading terminated prematurely.')
	    break
	 end
         traces(:,ik)=temp(idx4times);
	 headers(:,ik)=temp_headers;
      end
      iloc=ftell(fid);
   end
   headers(:,ik+1:end)=[];
   traces(:,ik+1:end)=[];
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function bool=requested_header_no11(headers,constraint_info,trace_no, ...
                                    expression) %#ok input arguments "headers" and
				        % "trace_no" may be needed in "expression"
% Output logical variable. "bool" is true if the header value satisfied the 
% constraint specified in param.traces

nheaders=size(constraint_info,1);
if constraint_info{nheaders,2} == 0
   nheaders=nheaders-1;
end
for ii=1:nheaders
   eval([constraint_info{ii,1},'=headers(',constraint_info{ii,2},');']);
end
try
   bool=eval(expression);
catch
  disp(' Logical expresession for trace selection:')
  disp([' ',expression])
  disp(' has errors. Misspelled headers, unknown functions and/or a bad syntax may be at fault.')
  ple   % Print last error
  myerror(' Abnormal terminantion')
end
