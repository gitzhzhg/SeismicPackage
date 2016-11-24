function wlog=l_convert(curves,info,aux_wlog)
% Function converts a matrix of curve values, curve names, curve units of measurement, 
% curve description, etc. into a well log structure.
%
% Written by: E. Rietsch: February 12, 2000;  
% Last updated: October 12, 2007: do not add text fields with unknown info 
%
%          wlog=l_convert(curves,info,aux_wlog)  
% INPUT
% curves   Matrix of log curves; the first column represents depth 
%          or equivalent (e. g. travel time)
% info     cell array of the form {mnemonic,units,description}; 
%          one row for each column of matrix "curves".
%          Example: {'rho','g/cm3','Density'; 'DTp','us/ft','Sonic'}
% aux_wlog   optional log structure from which some of the fields not
%          specified with the data above can be copied
% OUTPUT
% wlog     Log structure; if the input curves are single-precision then all
%          numeric fields of the structure will single-precision as well.
%          "wlog" has the following fields:
%          wlog.type    'well_log'
%          wlog.name    'Synthetic log'
%          wlog.tag     'unspecified'
%          wlog.curve_info    Cell array with curve mnemonics, units of
%                        measurement, and curve descriptions for each curve
%	   wlog.curves	Matrix of curve values
%          wlog.first	Start of log (first depth in file)
%          wlog.last	End of log (last depth in file)
%	   wlog.step	Depth increment (0 if unequal)
%          wlog.units   Units of measurement for depth (info{1,2})
%	   wlog.null	Null value (empty, if there are no NaNs in 
%                       matrix "curves".
%          wlog.date	Date (current date)
%
% The above fields do not represent all the fields required to create a 
% valid LAS file. In particular, the LAS standard requires the following
% pieces of information:
%        wlog.company     Company
%        wlog.well        Name of well
%        wlog.field       Field name
%        wlog.location    Location of well
% Sometimes there already exists a log structure which has this information.
% In this case the optional input parameter aux_wlog can be used to copy 
% this information to the new log structure.      
%
% EXAMPLE
%      depth=(5000:0.5:10000)';    % Create depth curve      
%      dt=150+25*randn(length(depth),1);   % Create sonic curve
%
%      wlog=l_convert(single([depth,dt]),{'depth','ft','Depth';'DTp','us/ft','P-sonic'});
%      wlog.name='Synthetic log'



run_presets_if_needed

% 	Check for input compatibility
[n,m]=size(curves);
if n == 0
   error('Curve array empty')
end

ninfo=size(info,1);

if ninfo ~= m
   error(['Number of curve mnemonics (',num2str(ninfo),...
    ') different from number of curves (',num2str(m),')'])
end

% 	Store input in structure
wlog.type='well_log';
wlog.name='Log created from matrix';
wlog.tag='unspecified';
wlog.curve_info=info;

if curves(1,1) > curves(end,1)
   wlog.curves=flipud(curves);
else
   wlog.curves=curves;
end

wlog.first=wlog.curves(1,1);
wlog.last=wlog.curves(end,1);
wlog.step=depths2step_with_checking(wlog.curves(:,1));
wlog.units=info{1,2};

if any(any(isnan(curves(:,2:end))))
   wlog.null=NaN;
else
   wlog.null=[];
end

%       Convert null field to the precision of the curve matrix
if isa(curves,'single')
   wlog.null=single(wlog.null);
end

%       Add a creation date field
wlog.date=date;
     
%       Check if input arguments include a log structure; if they do
%       add fields relevant for LAS files to the output dataset.
if nargin == 5   	
   if isstruct(aux_wlog)
      if isfield(aux_wlog,'company'), wlog.company=aux_wlog.company;   end
      if isfield(aux_wlog,'wellname'),wlog.wellname=aux_wlog.wellname; end 
      if isfield(aux_wlog,'field'),   wlog.field=aux_wlog.field;       end
      if isfield(aux_wlog,'location'),wlog.location=aux_wlog.location; end  
      if isfield(aux_wlog,'api'),     wlog.api=aux_wlog.api;           end
      if isfield(aux_wlog,'province'),wlog.province=aux_wlog.province; end
      if isfield(aux_wlog,'state'),   wlog.state=aux_wlog.state;       end
      if isfield(aux_wlog,'county'),  wlog.county=aux_wlog.county;     end
      if isfield(aux_wlog,'country'), wlog.country=aux_wlog.country;   end
      if isfield(aux_wlog,'service'), wlog.service=aux_wlog.service;   end
      if isfield(aux_wlog,'uwi'),     wlog.uwi=aux_wlog.uwi;           end
   else
      disp('Input parameter "aux_wlog" is not a structure.')
   end
end
