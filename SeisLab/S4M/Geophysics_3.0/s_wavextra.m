function [wavelet,aux]=s_wavextra(seismic,refl,varargin)
% Function extracts wavelet from seismic data using a log-derived 
% reflectivity sequence.
%
% Written by E. Rietsch, May 6, 2000
% Last update: January 16, 2007: Discontinued use of "spdiags"
%
%           [wavelet,aux]=s_wavextra(seismic,refl,varargin)
% INPUT
% seismic   seismic traces in the vicinity of the well
% refl      seismic structure representing the reflection coefficient sequence
%           seismic and refl must have the same sample interval
% varargin  one or more cell arrays; the first element of each cell array is a keyword,
%           the other elements are parameters. Presently, keywords are:
%           'downstep' step size (in seismic time units - usually ms) by which log and 
%                      seismic move down to compute a new wavelet.
%                      Default: {'downstep',20}
%           'headers'  Header to copy from seismic to wavelets. 
%                      Default: {'headers','*'} meaning all headers
%           'header_null' No-data value to use for missing header values.
%                      Default: {'header_null',NaN} 
%           'wlength'   wavelet length. Default 60 ms
%           'logshifts' vector of global shifts of the reflection coefficient
%                      sequence vs the seismic that are to be used (to account
%                      for a possible error in the time to the top of the log,
%                      bad check shot data). It can be one value or a vector. 
%                      A wavelet is computed for each shift.
%                      Default: {'logshifts',0}
%           'logwindow' length of log segment matched to seismic. Recommended: 
%                      logwindow >= 5*wlength
%                      Default: {'logwindow',refl.last-refl.first}  i.e. the 
%                               whole reflection coefficient series
%           'null'     Null value to use; Default: {'null',NaN}
%           'print'    Controls printed output showing progress of wavelet estimation; 
%                      no output if set to 0. Default: {'print',1}
%           'scale'    scale wavelets so that synthetic has about the same 
%                      amplitude level as the seismic (counteract noise in data)
%                      Default: {'scale','yes'}
%           'sp_constraint'  Controls use of spectrum constraints, representing 
%                      the ratio of Frobenius norm of spectrum constraint to 
%                      Frobenius norm of convolution matrix. If set to zero 
%                      spectrum constraint is not used. 
%                      Default: {'sp_constraint',0)
%           'dc_constraint' Constraint on the DC component of the wavelet
%                      Default: {'dc_constraint',0}
%           'wavshifts' vector of shifts of seismic data with respect to the
%                      reflection coefficients.  It can be one value or a vector. 
%                      It is intended to allow small changes in 
%                      shifts for any value of 'logshifts'. Only the best wavelet 
%                      found for any of the shifts is output
%                      Default: {'wavshifts',0}
%           'wnoise'   White noise (ratio of to maximum reflection coefficient)
%                      Default: {'wnoise',0.1}
%        
% OUTPUT
% wavelet   wavelet(s) extracted from the seismic data. A number of headers are added to this
%           data set to capture the following information about the wavelet.
%           wstart  start time of window over which wavelet is estimated
%           cc_wavelet  correlation coefficient for this wavelet and for this particular
%                       trace, window, and value of "logshifts"
%           cc_max      maximum correlation coefficient for this particular trace and 
%                       value of "logshift" (common for all wavelets derived from the same 
%                       seismic trace and the same value of "logshift")
%           cc_median   median correlation coefficient for this  particular trace and
%                       value of "logshift" (common for all wavelets derived from the same 
%                       seismic trace and the same value of "logshift")
%           swstart     start of seismic window used
%           rwstart     start of log window used
% aux       structure with additional information
%               aux.logsegments   number of log segments used (==> number of
%                       wavelets per trace
%               aux.logshifts     number of bulk logshifts used (see keyword 'logshifts')
%
% EXAMPLES    wavelets=s_wavextra(seismic,refl,{'logshifts',-40,4,32})

global ABORTED

ABORTED=true;  %#ok  Used in in compiled, GUI-based environment

%       Set defaults for input parameters
param.downstep=20;
param.headers='*';
param.header_null=NaN;
param.wlength=60;
param.logshifts=0;
param.logwindow=refl.last-refl.first;
param.null=NaN;
param.print=1;
param.sp_constraint=0.0;
param.dc_constraint=0.0;
param.scale='yes';
param.wavshifts=0;
param.wnoise=0.1;

%       Decode input arguments
param=assign_input(param,varargin);   % Read input parameters

%	Check input parameters
if param.logwindow > refl.last-refl.first
   param.logwindow=refl.last-refl.first;
end
if strcmp(param.scale,'yes')
   scaling=true;
else
   scaling=false;
end

if iscell(param.wavshifts)	% Legacy code
   param.wavshifts=cell2mat(param.wavshifts);
end
temp(1)=round(param.wavshifts(1)/seismic.step);
temp(3)=round(param.wavshifts(end)/seismic.step);
increment=max(fix(temp(3)-temp(1))/length(param.wavshifts),1);
temp(2)=increment;
param.wavshifts=temp*seismic.step;


%       Convert times to samples
nsampw=round(param.wlength/seismic.step)+1;        % Number of samples of wavelet
nlogwindow=round(param.logwindow/seismic.step)+1;  % Number of samples of log window
ndownstep=round(param.downstep/seismic.step);      % Number of samples to step down for next window
nsampr=length(refl.traces);                        % Number of samples of reflection coefficient
[nsamp,ntr]=size(seismic.traces);

%       Checking of input parameters
if abs(seismic.step - refl.step) > 1.06*eps*seismic.step
  error([' Seismic and reflection coefficients have different sample intervals: ', ...
        num2str([seismic.step,refl.step])])
end

temp=(seismic.first-refl.first)/seismic.step;
if abs(round(temp)-temp) > 1.0e6*eps
  disp(' Start time of seismic and reflection coefficients differ by a non-integer')
  disp(' multiple of the sample interval')
  error(' Abnormal termination')
end

%{
if iscell(param.logshifts)	% Legacy parameters
   logshifts=param.logshifts{1}:param.logshifts{2}:param.logshifts{3};
else
   logshifts=param.logshifts;
   if isempty(logshifts)
      error(' Empty array of log shifts supplied')
   end
end
%}

first=round(param.logshifts(1)/seismic.step);
last=round(param.logshifts(end)/seismic.step);
step=max(fix(temp(end)-temp(1))/length(param.logshifts),1);
logshifts=(first:step:last)*seismic.step;

%   	Prepare for newly created headers
header_info=[ ...
     {'cc_wavelet','n/a','Cross-correlation of synthetic and seismic'}; ...
     {'cc_max','n/a','Maximum correlation for this trace and shift'}; ...
     {'cc_median','n/a','Median correlation for this trace and shift'}; ...
     {'swstart','ms','Start of seismic window used'}; ...
     {'rwstart','ms','Start of reflection coefficient window used'}];

%       Select seismic headers to copy to wavelet and append header_info
if isfield(seismic,'headers')
  if ~iscell(param.headers)
    param.headers={param.headers};
  end
  if length(param.headers) == 1 && strcmp(param.headers{1},'*')
    param.headers=seismic.header_info(:,1);
  end
  [index,ier]=mnemonics_match(seismic.header_info(:,1),param.headers);
  if ier
    error(' Abnormal termination')
  end
  seismic_headers=seismic.headers(index,:);
  header_info=[header_info;seismic.header_info(index,:)];
end
nheaders=size(header_info,1); 

%       Reserve room for arrays
nlogsegments=fix((nsampr-nlogwindow)/ndownstep)+1; % Number of log windows
nwavelets=ntr*nlogsegments*length(logshifts);
shifts=NaN*zeros(nwavelets,1);
% cc_wavelets=param.header_null*zeros(nwavelets,1);
wavelets=zeros(nsampw,nwavelets);
headers=param.header_null*zeros(nheaders,nwavelets);

iawav=1; 
iewav=ntr;
nseiswindow=round((param.logwindow-param.wlength+ ...
        param.wavshifts(end)-param.wavshifts(1))/seismic.step);  % Correction

if param.print
  disp(['S_WAVEXTRA uses ',num2str(nlogsegments),' log segment(s) and ', ...
       num2str(length(logshifts)),' log shift(s)'])
end
aux.logsegments=nlogsegments;
aux.logshifts=logshifts;

nulls=0;

%       Create constraint matrix
if param.sp_constraint ~= 0          % Create spectral constraint matrix
   temp=s_select(seismic,{'times',refl.first,refl.last});
   spc=spectral_constraints(sum(correlate(temp.traces,temp.traces),2),nsampw);
   clear temp
   spc=spc/norm(spc,'fro'); 
   if param.dc_constraint ~= 0        % Add DC constraint matrix
      spc(1,:)=spc(1,:)*(1+abs(param.dc_constraint/param.sp_constraint));
   end  
   constraint=param.sp_constraint;
elseif param.dc_constraint ~= 0      % Create DC constraint matrix
   spc=ones(1,nsampw)/sqrt(nsampw);
   constraint=param.dc_constraint;
else
   constraint=0;
   spc=[];     
end

if iscell(param.wavshifts)
    increment=round(param.wavshifts{2}/seismic.step);
else
   increment=round(param.wavshifts(2)/seismic.step);
end

ik=0; 
% keyboard
for lshift=logshifts
  ik=ik+1;
  disp([' Shift: ',num2str(lshift),' (',num2str(ik),' of ',num2str(length(logshifts)),')'])
  seismic_ta=refl.first+lshift+param.wavshifts(1)+param.wlength*0.5;
  ia0=round((seismic_ta-seismic.first)/seismic.step);
  ia=ia0+1;
  iawav0=iawav;
  ie=ia+nseiswindow;
  ia=max(1,ia);
  ia_refl=1;
  ie_refl=nlogwindow;
 %     	 
 
  for ii=1:nlogsegments
    if ia > 0 && ie <= nsamp
      s1=refl.traces(ia_refl:ie_refl,:);
      s2=seismic.traces(ia:ie,:);
      [filters,cc,shift,scale]=mfilter_t2d(s1,s2, ...
          nsampw,param.wnoise,increment,constraint,spc);
      if scaling
         for jj=iawav:iewav
            wavelets(:,jj)=filters(:,jj-iawav+1)*scale(jj-iawav+1);
         end
%         wavelets(:,iawav:iewav)=filters*spdiags(scale,0,length(scale),length(scale));
      else
         wavelets(:,iawav:iewav)=filters;
      end
      headers(1,iawav:iewav)=cc(:)';
      headers(4,iawav:iewav)=seismic.first+((ia-1)+shift(:)')*seismic.step;
      headers(5,iawav:iewav)=refl.first+ia_refl+refl.step;
      shifts(iawav:iewav)=shift+ia0;

    else                      % Not enough seismic data for a requested shift
       nulls=1;
    end   
    if exist('seismic_headers','var')
       headers(6:end,iawav:iewav)=seismic_headers;  % Store seimic headers
    end
    ia_refl=ia_refl+ndownstep;
    ie_refl=ie_refl+ndownstep;
    ia=ia+ndownstep;
    ie=ie+ndownstep;
    iawav=iawav+ntr;
    iewav=iewav+ntr;
  end

%       Compute maximum and median correlation coefficient for each trace
  temp=reshape(headers(1,iawav0:iewav-ntr),ntr,nlogsegments);
  temp_max=max(temp,[],2);
  if nulls
    temp_median=NaN*zeros(size(temp,1),1);
    for ll=1:size(temp,1)
      idx=find(~isnan(temp(ll,:)));
      if ~isempty(idx)
        temp_median(ll)=median(temp(ll,idx));
      end
    end
  else
    temp_median=median(temp,2);
  end

  headers(2,iawav0:iewav-ntr)= ...
             reshape(temp_max(:,ones(nlogsegments,1)),1,nlogsegments*ntr);
  headers(3,iawav0:iewav-ntr)= ...
             reshape(temp_median(:,ones(nlogsegments,1)),1,nlogsegments*ntr);
end

idx=find(~isnan(shifts))';
min_shift=min(shifts(idx));
shifts(idx)=shifts(idx)-min_shift;
max_shift=max(shifts(idx));

   try
wavelet.type='seismic';
wavelet.tag='wavelet';
wavelet.name='';
wavelet.traces=param.null*zeros(nsampw+max_shift,nwavelets);
   catch
disp(' Not enough space in MATLAB to store all requires wavelets')
 %keyboard
   end

for ii=idx
  wavelet.traces(shifts(ii)+1:shifts(ii)+nsampw,ii)=wavelets(:,ii);
end
wavelet.first=seismic.first-refl.first+(min_shift-nsampw)*seismic.step;
wavelet.last=wavelet.first+(nsampw+max_shift-1)*seismic.step;
wavelet.step=seismic.step;
wavelet.units=seismic.units;

if (nulls || max_shift > 0) && isnan(param.null)
  wavelet.null=NaN;
end

if isfield(wavelet,'headers')
  wavelet.headers=[wavelet.headers;headers];
  wavelet.header_info=[wavelet.header_info;header_info];
else
  wavelet.headers=headers;
  wavelet.header_info=header_info;
end

if isfield(seismic,'history') && isfield(refl,'history')
  wavelet.history=seismic.history;
  wavelet=s_history(wavelet,'append',' ');
  wavelet=s_history(wavelet,'merge',refl.history);
end

ABORTED=false;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function matrix=spectral_constraints(ac,ncol)
% Function creates a matrix which imposes a spectral constraint on on the solution 
% of a linear system of equations
% INPUT
% ac      autocorrelation of a function with the desired spectral shape
%         ac must be symmetric and have at least ncol samples; 
%         the number of samples is odd
% ncol    number of columns/rows of the spectral constraint matrix
% 
% OUTPUT
% matrix  constraint matrix
%            matrix=spectral_constraints(ac,ncol)

nac=length(ac);
nc=(nac+1)/2;
if mod(ncol,2)
  ncolh=(ncol-1)/2;
  fac=fft(ac(nc-ncolh:nc+ncolh));
  fac=sqrt(sqrt(abs(fac)));
  fac=1./(fac+0.01*max(fac));
  tempm=fac(1:ncolh+1,ones(1,ncol)).*ftmatrix(ncolh+1,ncol);
  matrix=[real(tempm);imag(tempm(2:end,:))];
else
  ncolh=ncol/2-1;  
  fac=fft(ac(nc-ncolh:nc+ncolh),ncol);
  fac=sqrt(sqrt(abs(fac)));
  fac=1./(fac+0.01*max(fac));
  tempm=fac(1:ncolh+2,ones(1,ncol)).*ftmatrix(ncolh+2,ncol);
  matrix=[real(tempm);imag(tempm(2:end-1,:))];
end
% matrix=matrix/norm(matrix,'fro');
