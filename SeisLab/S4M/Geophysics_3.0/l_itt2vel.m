function wlog=l_itt2vel(wlog,typ,varargin)
% Compute P-velocity from the sonic curve; if a V-velocity curve already exists
% a message is printed to the command window and "wlog" is returned unchanged.
% To add an additional P-velocity give it a different name (see example below).
%
% Written by: E. Rietsch: April 16, 2006
% Last updated: February 20, 2010; Remove one of the "verbose" options
%
%        wlog=l_itt2vel(wlog,typ,varargin)
% INPUT
% wlog   log structure with a sonic curve
% typ    string with type of interval transit time
%        possible values are:
%        'sonic': sonic interval transit time to compressional velocity
%        'shear': shear interval transit time to shear velocity
% varargin  new definitions of curve mnemonics of the form {'dtp','DTCO')
%        By default, the function uses the mnemonics defined in global structure
%        CURVES created in function "systemDefaults".
%   'verbose'  specifies if an alert is to be issued if the log already has a 
%        velocity curve (i.e. if no conversion is performed). 
%        Possible values are: true ('yes') and false ('no').
%        Default: {'verbose',true}
%	 The other possible "varargin" input can be re-definitions of
%	      curve mnemonics  (see example below).       
% OUTPUT
% wlog   log structure with all input curves and Vp
%
% EXAMPLE
%        wlog=l_data;
%        wlog=l_itt2vel(wlog,'sonic',{'vp','new_vp'});
%        l_curve(wlog);

% UPDATE HISTORY
%        June 6, 2008: Add "verbose" option
         
         
%     Set default of input argument
param.verbose=true;

%     Replace default by actual input arguments
[param,cm]=l_assign_input(param,varargin);


switch lower(typ)
case 'sonic'
   alert1=' Well log already has a P-velocity curve.';
   alert2=' Log must have a P-sonic (DTp) curve.';
   alert3=' Unknown units of sonic log: ';
   alert4='Compressional velocity';
   mnem4itt=cm.dtp;
   mnem4vel=cm.vp;
   vel='Vp';
   veltype='compressional velocity';

case 'shear'
   alert1=' Well log already has an S-velocity curve.';
   alert2=' Log must have a shear-sonic (DTs) curve.';
   alert3=' Unknown units of shear sonic log: ';
   alert4='Shear velocity';
   mnem4itt=cm.dts;
   mnem4vel=cm.vs;
   vel='Vs';
   veltype='shear velocity';

otherwise
   error(['Unknown type of interval transit time: "',typ,'"'])
end

%	Check if log already has a P-velocity curve
[dummy,ier]=curve_index1(wlog,mnem4vel);
if ier == 0 
    if isyes(param.verbose)
       alert(alert1)
    end
   return
end


[idtp,dummy]=curve_index1(wlog,mnem4itt);   %#ok Second output argument influences internal processing
if isempty(idtp)
   disp(alert2)
   disp([' Curve mnemonics of log: ',cell2str(wlog.curve_info(:,1),', ')])
   error(' Abnormal termination')
   
else
   wlog.curves=[wlog.curves,1.0e6./wlog.curves(:,idtp)];
   if strcmpi(wlog.curve_info{idtp,2},'us/m')
      punits='m/s';
   elseif strcmpi(wlog.curve_info{idtp,2},'us/ft')
      punits='ft/s';
   else
      error([alert3,wlog.curve_info{idtp,2}]) 
   end 
   wlog.curve_info=[wlog.curve_info;{mnem4vel,punits,alert4}]; 
   wlog=add_curve_type(wlog,{mnem4vel,vel,veltype});
end
