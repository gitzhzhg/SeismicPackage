function wlog=fix_las_file_log(wlog)
% Perform error checking and other tasks for well log read fronm LAS file
% User in LAS-file reader
%
% Written by: E. Rietsch: December 19, 2006
% Last updated:
%
%        wlog=fix_las_file_log(wlog)
% INPUT
% wlog   raw well log
% OUTPUT
% wlog   corrected well log

global S4M

%	Convert all well log data to single precision if requested
if strcmpi(S4M.precision,'single')
   wlog=single(wlog);
end

%	Replace mnemonic of first column if it is "dept"
if strcmpi(wlog.curve_info{1,1},'dept')
   wlog.curve_info{1,1}='DEPTH';
end

%	Make sure that log depths are ascending
if wlog.first > wlog.last
   wlog.curves=flipud(wlog.curves);
   temp=wlog.first;
   wlog.first=wlog.last;
   wlog.last=temp;
   wlog.step=-wlog.step;
end

%       Check log start and end time and step size
if isempty(wlog.curves)
   disp(' No log curve values read; there may be non-numeric characters in the data block')
   error(' Check ~ASCII-data section of LAS file')
end

if wlog.first ~= wlog.curves(1,1)
   fprintf(['Log start depth (',num2str(wlog.curves(1,1)), ...
      ') differs from header information (',num2str(wlog.first), ...
      '); now corrected\n'])
   wlog.first=wlog.curves(1,1);
end

if wlog.last ~= wlog.curves(end,1)
  fprintf(['Log end depth (',num2str(wlog.curves(end,1)), ...
    ') differs from header information (',num2str(wlog.last), ...
    '); now corrected\n']);
  wlog.last=wlog.curves(end,1);
end

if wlog.step ~= 0
   if ~isconstant(diff(wlog.curves(:,1)),S4M.log_step_error)
      fprintf('Log step size not uniform enough; step size changed to 0\n')
   end
end

%  Replace null values by NaNs
idx=find(wlog.curves == wlog.null);
if ~isempty(idx)
   wlog.curves(idx)=NaN;
   wlog.null=NaN;
else
   wlog.null=[];
end

%	Replade 'UNDEFINED' in curve description when it is obvious from the mnemonic
wlog.curve_info=description_substitution(wlog.curve_info);

%       Replace unadmissible curve mnemonics
wlog.curve_info(:,1)=fix_mnemonics(wlog.curve_info(:,1));
