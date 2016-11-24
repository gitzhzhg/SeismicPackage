function seismic=finalize_seismic_dataset(seismic,headers,param,parameters)
% Called from "read_segy_file"

global S4M

if S4M.history
   seismic=s_history(seismic,'add',seismic.from);
end

%	Convert to IEEE format if necesssary
if strcmpi(param.format,'ibm')
   if strcmpi(S4M.precision,'single')
      seismic.traces=ibm2single(seismic.traces);
   else
      seismic.traces=ibm2double(seismic.traces);
   end
end
if isempty(seismic.traces)
   msgdlg([' Alert: No seismic traces read from file ',seismic.from])
end

if seismic.aux_per_record == 0
   seismic=rmfield(seismic,'aux_per_record');
end

seismic.null=[];

%	Use requested precision
if strcmpi(S4M.precision,'single')
   seismic=single(seismic);  % Convert other numeric fields to single-precision
else
   seismic=double(seismic);
end

%       Remove default headers that are all zeros; convert headers from
%       integers to single-precision floats (if possible)
[seismic.header_info,seismic.headers,lagindex]= ...
               fix_headers_no10(headers,parameters.header_info);

%       Remove fields "headers" and "header_info" if they are empty
if isempty(seismic.header_info) || isempty(seismic.headers)
   seismic=rmfield(seismic,{'header_info','headers'});
end

%	Apply lag if it is not zero and should not be ignored
if ~isempty(lagindex) &&  ~param.ignoreshift
   disp('Seismic data shifted since header "lag" is not identically zero.')
   disp(['Lag varies from ',num2str(min(seismic.headers(lagindex,:))),' to ', ...
        num2str(max(seismic.headers(lagindex,:)))])
   seismic=s_shift(seismic,{'shifts',seismic.headers(lagindex,:)});
end

seismic.fp_format_of_segy_file=param.format;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [header_info,headers,lagindex]=fix_headers_no10(headers,header_info)
% Remove headers that are read by default if they are all zero; user-specified
% headers are kept

global S4M

%       Remove header mnemonics that contain only zeros
nh=size(headers,1);
index=find(~cell2num(header_info(:,6)));
bool=false(nh,1);
for ii=length(index):-1:1
   bool(ii)=all(headers(ii,:) == 0);
end

header_info(bool,:)=[];
header_info(:,4:end)=[];
headers(bool,:)=[];

%	Check if header "lag" still exists
lagindex=find(ismember(header_info(:,1),'lag'));

%	Convert from integer to float
if strcmpi(S4M.precision,'single')
%	Convert header from int32 to single (if possible without loss)
   bool=any(headers ~= int32(single(headers)),2);
   if any(bool)
%      mnems=header_info(bool,1);
      disp([' Headers: ',cell2str(header_info(bool,1),', ')])     
      disp(' cannot be converted to single precision without loss of accuracy.') 
      alert('All headers have been saved in double precision.')
      headers=double(headers);
   else
      headers=single(headers);
   end
else
   headers=double(headers);
end
