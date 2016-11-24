function units=getParameterUnits(dataset,mnem)
% Get units of a parameter from a  data set (seismic, log, etc.)

if ~isfield(dataset,'parameter_info')
   error('Data set has no field "parameter_info".')
end

idx=find(ismember(dataset.parameter_info(:,1),mnem));

if length(idx) == 1
   units=dataset.parameter_info{idx,2};
else
   if isempty(idx)
      disp(['Mnemonic "',mnem,'" not found.'])
      disp('Existing mnemonics are:')
      disp(cell2str(dataset.parameter_info(:,1),', '))
      error('Abnormal termination')
   
   else
      disp(['More than one nemonic "',mnem,'" found.'])
      disp('Existing mnemonics are:')
      disp(cell2str(dataset.parameter_info(:,1),', '))
      error('Abnormal termination')
   end
end