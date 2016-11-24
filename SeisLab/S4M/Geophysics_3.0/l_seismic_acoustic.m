function wlog=l_seismic_acoustic(wlog,varargin)
% Function computes seismic-relevant curves such as P-velocity and acoustic
% impedance, Poison's ratio provided they do not yet exist
% Function assumes that the input log has at least the following curves
%      DTp or Vp
%      rho
% and computes the following additional curves if they do not exist
%      Vp  or DTp
%      aImp
%
% Written by: E. Rietsch: December 22, 2000
% Last updated: April 17, 2006: Bug fix with impedance mnemonics
%
%               wlog=l_seismic_acoustic(wlog,varargin)
% INPUT
% wlog    log structure with, as a minimum, the curves listed above
% varargin  new definitions of curve mnemonics of the form {'rho','RHOB')
%        By default, the function uses the definitions of global structure
%        CURVES as defined in function "systemDefaults"         
% OUTPUT
% wlog log structure with all input curves and those additional curves listed 
%        above if they do not among the curves of the input log.
%
% EXAMPLE
%        wlog=l_data;
%        l_curve(wlog)
%        wlog1=l_seismic_acoustic(wlog);
%        l_curve(wlog1)


%	The only possible "varargin" input can be re-definitions of curve mnemonics

dummy=[];
[dummy,cm]=l_assign_input(dummy,varargin); %#ok  First output argument not required

%	Check for existence of density
irho=curve_index1(wlog,cm.rho,0);
if isempty(irho)
   disp(' Log must have curve for density to compute the impedance.')
   disp([' Curve mnemonics of log: ',cell2str(wlog.curve_info(:,1),', ')])
   disp(' The impedance will not be computed.')
   compute_imp=false;
else
   compute_imp=true;
end

%    Check for existence of P-velocity curve and compute it from sonic if absent
ivp=curve_index1(wlog,cm.vp,0);
if isempty(ivp)
   idtp=curve_index1(wlog,cm.dtp,0);
   if isempty(idtp)
      disp(' Log must have either a P-sonic (DTP) or a P-velocity (Vp) curve')
      disp([' Curve mnemonics of log: ',cell2str(wlog.curve_info(:,1),', ')])
      error(' Abnormal termination')
   else
      wlog.curves=[wlog.curves,1.0e6./wlog.curves(:,idtp)];
      if strcmpi(wlog.curve_info{idtp,2},'us/m')
         punits='m/s';
      elseif strcmpi(wlog.curve_info{idtp,2},'us/ft')
         punits='ft/s';
      else
         error([' Unknown units of sonic log: ',wlog.curve_info{idtp,2}]) 
      end 
      wlog.curve_info=[wlog.curve_info;{cm.vp,punits,'P-velocity'}]; 
      ivp=curve_index1(wlog,cm.vp);
      wlog=add_curve_type(wlog,{cm.vp,'Vp','acoustic velocity'});
   end
else
   punits=wlog.curve_info{ivp,2};
end

%    Check for existence of sonic curve and compute it from P-velocity if absent
idtp=curve_index1(wlog,cm.dtp,0);
if isempty(idtp)
   wlog.curves=[wlog.curves,1.0e6./wlog.curves(:,ivp)];
   if strcmpi(wlog.curve_info{ivp,2},'m/s')
      punits='m/s';
      piunits='us/m';
   elseif strcmpi(wlog.curve_info{ivp,2},'ft/s')
      punits='ft/s';
      piunits='us/ft';
   else
      error([' Unknown units of velocity log: ',wlog.curve_info{ivp,2}]) 
   end 
   wlog.curve_info=[wlog.curve_info;{cm.dtp,piunits,'Sonic log (Pressure)'}];
   wlog=add_curve_type(wlog,{cm.dtp,'DTp','sonic'});
end

%       Compute acoustic impedance if not present in input log
if compute_imp
   iaimp=curve_index1(wlog,cm.aimp,0);
   if isempty(iaimp)
      runits=wlog.curve_info{irho,2};
      wlog.curves=[wlog.curves,wlog.curves(:,ivp).*wlog.curves(:,irho)];
      wlog.curve_info=[wlog.curve_info;{cm.aimp,[punits,' x ',runits], ...
            'Acoustic impedance'}];  
      wlog=add_curve_type(wlog,{cm.aimp,'Imp','impedance'});
   end
end
