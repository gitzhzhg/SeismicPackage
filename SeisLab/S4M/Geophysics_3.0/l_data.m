function wlog=l_data
% Create a synthetic well log for test and demo purposes; the log curves are
% not based on rock physics.
%
% Written by: E. Rietsch: October 1, 2005;
% Last updated: February 21, 2010: Read log from mat-file (if it exists)
%
%      wlog=l_data

% UPDATE HISTORY
%      January 22, 2007: Use requested precision
%      May 12, 2008: Add curves with lithology markers "sand and "shale"


run_presets_if_needed

global S4M

%   If the file exists, red data from file
filename=fullfile(S4M.testdata,'wlog.mat');
if exist(filename,'file')
   load(filename)  % Dataset loaded has name "seismic"
   return
end

%   Since the file has not been found, create data
param.seed=1111;
param.stdev=2;
param.first=5000;
param.last=10000;
param.step=0.5;
depth=(param.first:param.step:param.last)';
nsamp=length(depth);

%	Set seed for random number generator to assure that the same log 
%       is created each time the function is run
randn('state',param.seed);

temp=create_powerlaw_samples(nsamp,1,-0.5,param.seed);
dt=3000*depth.^(-0.33)+(500./sqrt(depth)).*temp;

vp=1.0e6./dt;

vclay=max((min((3.5+temp),6)/6),0).^0.6;
vclay(vclay < 0.4)=vclay(vclay < 0.4).^2;

%	Create log curves
curves=zeros(nsamp,6);
curves(:,1)=depth;
curves(:,2)=dt;
curves(:,3)=vp;
curves(:,4)=0.23*curves(:,3).^0.25 + 0.02*randn(nsamp,1);
curves(:,5)=0.8042*vp-450/0.3 + 100*randn(nsamp,1);
curves(:,6)=vclay;
curves(:,7)=vclay < 0.5;
curves(:,8)=~curves(:,7);

wlog=l_convert(curves,{'depth','ft','Depth';
                       'DTp','us/ft','P-sonic';
		               'Vp','ft/s','P-velocity';
		               'rho','g/cm3','Bulk density';
		               'Vs','ft/s','S-velocity';
		               'Vclay','fraction','Clay volume fraction';
                       'sand','logical','Sand index';
                       'shale','logical','Shale index'});

wlog.name='Synthetic log';

wlog.parameter_info={'ekb',wlog.units,'Kelly Bushing Elevation'};
wlog.ekb=84;

%	Add curve-type info to well log
wlog=add_curve_type(wlog,{'depth','depth','Depth'});
wlog=add_curve_type(wlog,{'DTp','dtp','Depth'});
wlog=add_curve_type(wlog,{'Vp','vp','P-velocity'});
wlog=add_curve_type(wlog,{'rho','rho','Density'});
wlog=add_curve_type(wlog,{'Vs','vs','S-velocity'});
wlog=add_curve_type(wlog,{'Vclay','vclay','Clay volume'});

wlog.null=[];

if strcmp(S4M.precision,'single')
   wlog=single(wlog);
end
