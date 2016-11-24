function wlog=l_reflections(wlog,varargin)
% Function computes reflection coefficients; 
% Written by: E. Rietsch, November 5, 2000
% Last update: March 18, 2001; Complete rewrite with new curve-name conventions
%
%          logout=l_reflections(wlog,varargin)
% INPUT
% wlog     log structure with curves two-way time and acoustic impedance; 
% varargin one or more cell arrays; the first element of each cell array is a keyword,
%          the other elements are parameters. Presently, keywords are:
%          'action'       defines action to take. Possible values are:
%              'add'      Add curve. Gives error message if curve 'Arefl' already exists
%              'add_ne'   Add curve. Replaces it if a curve with mnemonic "mnem" already 
%                         exists.
%              'replace'  Replaces curve with mnemonic "mnem"; error if no curve with this
%                         mnemonic exists 
%               Default: {'action','add_ne'}
%          'step'    sample interval in time.
%               Default: sample interval of curve 'TWT' (if uniform)
% OUTPUT
% logout   log structure with curves TWT, depth (if in input log), Refl, gradient (if shear velocity is available)
 
if ~isstruct(wlog)
  error(' Input data set must be a log structure')
end

param.action='add_ne';
param.step=[];

%       Decode input arguments
[param,cm]=l_assign_input(param,varargin);

wlog=l_switch_depth(wlog,cm.twt);
if isempty(param.step)
  if wlog.step == 0
    disp(' Log structure not uniformly sampled in time and no sample interval specified')
    error(' Abnormal termination')
  end
else
  wlog=l_resample(wlog,param.step); 
end

aimp=l_gc(wlog,cm.aimp);

%       Compute impedance and reflection coefficients
refl=diff(aimp)./(aimp(1:end-1)+aimp(2:end));

wlog=l_curve(wlog,param.action,'Arefl',[0;refl],'n/a','Acoustic reflectivity');
