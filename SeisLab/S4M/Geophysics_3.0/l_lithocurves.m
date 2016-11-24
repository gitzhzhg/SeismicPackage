function wlog=l_lithocurves(wlog,varargin)
% Function computes "logical" curves representing lithology. The curve values are
% either 1 (true), if a particular lithology is present, or 0 (false), if it is absent.
%
% It assumes that the input log has at least the curve
%      Vclay
% and computes the following additional curves if they do not exist
%      sand
%      sh_sand
%      shale
% based on the (default) condition 
%      sand=vclay < 0.25
%      sh_sand=vclay >= 0.25 && vclay  <= 0.35;
%      shale=vclay > 0.35;
%      The cut-offs can be changed via keyword 'clay_cutoffs'
%
%             OR
% It assumes that the input log has at least the curves
%      Vclay
%      Sbrine
% and computes the following additional curves if they do not exist
%      sand
%      hc_sand
%      w_sand
%      sh_sand
%      shale
% based on the (default) condition 
%      sand=vclay < 0.25
%      hc_sand=vclay < 0.25  && sbrine <= 0.60;
%      w_sand =vclay < 0.25  && sbrine >  0.60;
%      sh_sand=vclay >= 0.25 && vclay  <= 0.35;
%      shale=vclay > 0.35;
%      The water saturation cut-off can be changed via keyword 'sw_cutoff'
%      The clay volume cutoffs can be changed via keyword 'clay_cutoffs'
% The above conditions assume that the units of Vclay and Sbrine are fractions; they are 
% appropriately modified if this is not the case.
%
% Written by: E. Rietsch: December 19, 2000
% Updated: March 9, 2001: Cut-offs can be a vector or individual values 
%
%               wlog=l_lithocurves(wlog,varargin)
% INPUT
% wlog    log structure with the at least the curves listed above
% varargin  new definitions of curve mnemonics of the form {'vclay','Vcl'), i.e. the
%        default mnemonic followed by the actual mnemonic
%        'action'     Possible values are: 
%                     'add' (add curve; abort if it already exists),
%                     'add_ne' (add curve; replace if it already exists), 
%                     'replace' (replace curve'; abort if it already exists) 
%                     Default: {'action','add_ne'}
%        'sw_cutoff'  Water saturation cut-off as a fraction.
%                     Default: {'sw_cutoff',0.6}
%        'clay_cutoffs'   sand, sandy shale, and shale cut_offs as fractions.
%                     Default: {'clay_cutoffs',0.25,0.35}    
%          
% OUTPUT
% wlog log structure with all input curves and those additional curves listed 
%        above if they do not among the curves of the input log.
%        By default, the function uses the definitions of the global structure
%        CURVES as defined in function "presets0"

%       Set defaults for input parameters
param.action='add_ne';
param.sw_cutoff=0.60;
param.clay_cutoffs=[0.25,0.35];

%       Read input parameters
[param,cm]=l_assign_input(param,varargin);

if iscell(param.clay_cutoffs)
   param.clay_cutoffs=cat(2,param.clay_cutoffs{:});
end

%       Check for existence of clay volume and water saturation
[ivclay,ier]=curve_index1(wlog,cm.vclay);
if ier > 0
   error(['No clay volume curve (mnemonic "',cm.vclay,'") found.'])
end

%       Compute lithology curves
%       Create logicals for Water sand, HC sand, Shaly sand, and Shale
vclay=wlog.curves(:,ivclay);
units_vclay=wlog.curve_info{ivclay,2};

if strcmpi(units_vclay,'%')
   l1_vclay=param.clay_cutoffs(1)*100;
   l2_vclay=param.clay_cutoffs(2)*100;
elseif strcmpi(units_vclay,'fraction')
   l1_vclay=param.clay_cutoffs(1);
   l2_vclay=param.clay_cutoffs(2);
else
   disp(' ')
   disp([' Unknown units for clay volume: ',units_vclay])
   error(' Abnormal termination')
end

[isbrine,ier]=curve_index1(wlog,cm.sbrine);


           if ier == 0             % There is a brine saturation curve
sbrine=wlog.curves(:,isbrine);
units_sbrine=wlog.curve_info{isbrine,2};
if strcmpi(units_sbrine,'%')
   l1_sbrine=param.sw_cutoff*100;
elseif strcmpi(units_sbrine,'fraction')
   l1_sbrine=param.sw_cutoff;
else
   disp([' Unknown units for water saturation: ',units_sbrine])
end

hc_sand=vclay < l1_vclay  & sbrine <= l1_sbrine;
w_sand=vclay  < l1_vclay  & sbrine > l1_sbrine;
sh_sand=vclay >= l1_vclay & vclay  <= l2_vclay;
shale=vclay   > l2_vclay;
sand=hc_sand+w_sand;
wlog=l_curve(wlog,param.action,cm.sand,sand,'logical','Sand');
wlog=l_curve(wlog,param.action,cm.hc_sand,hc_sand,'logical','Hydrocarbon sand');
wlog=l_curve(wlog,param.action,cm.wet_sand,w_sand,'logical','Wet sand');
wlog=l_curve(wlog,param.action,cm.sh_sand,sh_sand,'logical','Shaly sand');
wlog=l_curve(wlog,param.action,cm.shale,shale,'logical','Shale');


           else                           % There is no brine saturation curve 
alert('No brine saturation found.')

sand=vclay < l1_vclay;
sh_sand=vclay >= l1_vclay  &  vclay  <= l2_vclay;
shale=vclay   > l2_vclay;
wlog=l_curve(wlog,param.action,cm.sand,sand,'logical','Sand');
wlog=l_curve(wlog,param.action,cm.sh_sand,sh_sand,'logical','Shaly sand');
wlog=l_curve(wlog,param.action,cm.shale,shale,'logical','Shale');
           end
