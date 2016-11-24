function wlog=l_elastic_impedance(wlog,varargin)
% Compute elastic impedance curves for given angles
% The function performs the following steps:
% 1. Compute angle-dependent reflection coefficients for the requested angles
% 2. Compute angle-dependent impedances
% The function assumes that P-sonic (P-velocity), S-sonic (or S-velocity) and
% density are represented by the standard curve mnemonics. If this is not the
% case they can be defined via {standard_mnemonic,actual_mnemonic} cell vectors
% in the argument list (e.g. {'Vp','p_velocity'}).
% If Rueger's method is used, curves for epsilon and delta must also be in the 
% well log.
%
% Rewritten by: E. Rietsch: July 16, 2003
% Last updated: February 18, 2010: Use function "zoeppritz_reflections"
%
%         wlog=l_elastic_impedance(wlog,varargin)
% INPUT
% wlog    log structure with at least sonic, shear, and density curves
%         if the method is 'Rueger' it also needs curves for epsilon and delta
% varargin  one or more cell arrays; the first element of each cell array is a 
%         keyword string, the following arguments contains a parameter(s). 
%         Accepted keywords are:
%    'angles'  vector of angles (in degrees). 
%         Default: {'angles',[0:10:50]}
%    'method'  Method used; possible values are 'Aki','Bortfeld', 
%         'Hilterman','two-term','Rueger','Shuey','Zoeppritz'
%         The case does not matter ('Aki' is equivalent to 'aki');
%         also, 'Rueger' is equivalent to 'Ruger'.
%         Default: {'method','Bortfeld'}
% OUTPUT
% wlog    input log structure with the elastic impedance curves appended
%         the curve mnemonics are eimp with the angle and the first letter
%         of the method appended (e.g. eimp30s for the elastic impedance 
%         for 30 degrees computed with Shuey's method)
%
% EXAMPLE
%         wlog=l_data;
%         step=wlog.step;
%         wlog.step=0;     % Pretend the log is not uniformly sampled
%         wlog=l_elastic_impedance(wlog,{'angles',[0,5,10,20,30]},{'method','Aki'});
%         wlog=l_elastic_impedance(wlog,{'angles',[0,5,10,20,30]},{'method','Zoeppritz'});
%         wlog.step=step;  % Restore the original sample interval
%         l_curve(wlog)
%         l_plot1(wlog,{'curves','eimp30a','eimp30z'},{'depths',8000,9000})
%
%         %    Compare reflection coefficients from the two impedance curves
%         reflecta=s_log2reflectivity(wlog,4,{'depth_time',8000,1000}, ...
%              {'aImp','eimp30a'});
%         reflecta.name='Reflectivity-Aki';
%         reflectz=s_log2reflectivity(wlog,4,{'depth_time',8000,1000}, ...
%              {'aImp','eimp30z'});
%         reflectz.name='Reflectivity-Zoeppritz';
%         s_compare(reflecta,reflectz)


% UPDATE HISTORY
%         August 23, 2007: Use "l_itt2vel" for velocity computations.
%         November 19, 2007: Include Zoeppritz equations
%         April 8, 2008: Handle logs in depth and time
%         June 4, 2008: silence alert in "l_itt2vel"


%       Set defaults for input parameters
param.angles=(0:10:50);
param.method='Bortfeld';

%       Replace defaults by actual input arguments (if there are any)
[param,cm]=l_assign_input(param,varargin,'l_elastic_impedance');

if iscell(param.angles)
   param.angles=cell2mat(param.angles);
end

%%    Create Vp and Vs curves
wlog=l_itt2vel(wlog,'sonic',{'dtp',cm.dtp},{'vp',cm.vp},{'verbose',false});
wlog=l_itt2vel(wlog,'shear',{'dts',cm.dts},{'vs',cm.vs},{'verbose',false});

%       Compute the elastic impedance(s) according to the requested method
switch lower(param.method)
case {'rueger','ruger'}
   tlog=l_rm_nulls(wlog,'any',{cm.vp,cm.vs,cm.rho,cm.epsilon,cm.delta});
   tlog=l_fill_gaps(tlog);
   tlog=l_curve(tlog,'keep',{cm.vp,cm.vs,cm.rho,cm.epsilon,cm.delta});
   refl=ava_approximation_rueger(l_gc(tlog,cm.vp),l_gc(tlog,cm.vs), ...
           l_gc(tlog,cm.rho),l_gc(tlog,cm.epsilon),l_gc(tlog,cm.delta), ...
           param.angles);

case 'zoeppritz'
   tlog=l_rm_nulls(wlog,'any',{cm.vp,cm.vs,cm.rho});
   tlog=l_fill_gaps(tlog);
   tlog=l_curve(tlog,'keep',{cm.vp,cm.vs,cm.rho});
   refl=zoeppritz_reflections(l_gc(tlog,cm.vp),l_gc(tlog,cm.vs), ...
           l_gc(tlog,cm.rho),param.angles);
          
case {'aki','bortfeld','hilterman','two-term','shuey'}
   tlog=l_rm_nulls(wlog,'any',{cm.vp,cm.vs,cm.rho});
   tlog=l_fill_gaps(tlog);
   tlog=l_curve(tlog,'keep',{cm.vp,cm.vs,cm.rho});
   refl=ava_approximation(l_gc(tlog,cm.vp),l_gc(tlog,cm.vs), ...
           l_gc(tlog,cm.rho),param.angles,param.method);

otherwise
   error(['Unknown method: "',param.method,'"'])

end

%       Compute angle-dependent impedance
nangles=length(param.angles);
% [vp,info]=l_gc(tlog,cm.vp);
vp=l_gc(tlog,cm.vp);
den=l_gc(tlog,cm.rho);
imp0=vp(1)*den(1);
imp=imp0(1)*[ones(1,nangles);exp(2*cumsum(refl))];


%       Add curves to existing log curves
tlog.curves=[tlog.curves,imp];
info=cell(length(param.angles),3);

for ii=1:nangles
   mnem=mnem4eimp(param.method,round(param.angles(ii)));
   wlog=l_curve(wlog,'delete_ne',mnem);
   info(ii,:)={mnem,[l_gu(tlog,cm.vp),' x ',l_gu(tlog,cm.rho)], ...
      ['Elastic impedance for ',num2str(param.angles(ii)),' deg.(',param.method,')']};
   wlog=add_curve_type(wlog,{mnem,'Imp','impedance'});
end

bool=(ismember(wlog.curves(:,1),tlog.curves(:,1)));
ncurves=size(wlog.curves,2);
wlog.curves(:,ncurves+(1:nangles))=NaN;
wlog.curve_info=[wlog.curve_info;info];
wlog.curves(bool,ncurves+(1:nangles))=imp;

tlog.curve_info=[tlog.curve_info;info];


%     Account for null values.
if any(isnan(wlog.curves(:)))
   wlog.null=NaN;
else
   wlog.null=[];
end

%     Select the appropriate precision
if strcmp(class(wlog.curves),'single')
   wlog=single(wlog);
else
   wlog=double(wlog);
end
