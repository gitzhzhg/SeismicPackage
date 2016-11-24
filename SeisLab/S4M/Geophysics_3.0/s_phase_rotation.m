function seismic=s_phase_rotation(seismic,phase,varargin)
% Function rotates the phase of each trace in the input data set.
% If more than one phase angle is given (if "phase" is an array), the output
% can be a seismic structure array with the same dimension as "phase" or a
% regular seismic structure where the number of traces of the output data  
% set equals the number of traces of the input data set multiplied by the
% number of elements of "phase". This latter option is the default.
%
% Written by: E. Rietsch: January 28, 2001.
% Last updated: September 21, 2009: bug fix
%
%       seismic=s_phase_rotation(seismic,phase,varargin)
% INPUT
% seismic   seismic data set
% phase     phase angle or array of phase angles (in degree)
% varargin  one or more cell arrays; the first element of each cell array is a
%           keyword, the other elements are parameters. 
%           Presently, keywords are:
%    'output'   Type of output. The options are 'standard' and 'array'. 
%           The latter creates a structure array; one structure element for 
%           each phase if "phase" is an array. This is only relevant if 
%           "phase" has at least two elements.
%           Default: {'output','standard'}
%    'header'  header mnemonic to use for phase. No header added if empty string
%           Default: {'header','phase'}
%    'add_option'  Possible values are 'add' (add header, abort with error 
%           if header already exists) and 'add_ne' (add header, overwrite 
%           header if it exists; no error)
%           Default:{'add_option','add_ne'}
% OUTPUT
% seismic   seismic dataset or seismic dataset array with phase rotated as
%           requested
%
% EXAMPLE
%           wavelet=s_create_wavelet;
%           wavelet.name='zero-phase';
%           wavelet35=s_phase_rotation(wavelet,35);
%           wavelet35.name='35-degree phase';
%           s_compare(wavelet,wavelet35)

% UPDATE HISTORY
%           May 15, 2007: Clean-up code


global S4M

%       Set defaults of input parameters
param.output='standard';
param.header='phase';
param.add_option='add_ne';

%       Replace defaults by input arguments
param=assign_input(param ,varargin);

[nsamp,ntr]=size(seismic.traces);
nphase=length(phase);

if nsamp < 3
   disp([' Alert from "s_phase_rotation": traces have only ',num2str(nsamp),' samples'])
end

if isnull(seismic)
   bool=isnan(seismic.traces);
   if any(bool)
      seismic.traces(bool)=0;
      seismic.null=[];
      disp(' Alert from "s_phase_rotation": NaNs replaced by zeros')
   end
end

%       Compute the Hilbert transform of the seismic traces
hseis=myhilbert(seismic.traces);

cphase=phase*pi/180;
sph=sin(cphase);
cph=cos(cphase);

if nphase == 1
   seismic.traces=cph*real(hseis)-sph*imag(hseis);

   if ~isempty(param.header)
      history=S4M.history;
      S4M.history=false;      % Do not make an entry into the history field of the dataset
      seismic=ds_header(seismic,param.add_option,param.header,phase,'degree','Phase angle');
      S4M.history=history;
   end

%	Append history field
   if isfield(seismic,'history')
      htext=['Phase rotation: ',num2str(phase),' degrees'];
      seismic=s_history(seismic,'append',htext);
   end 
 
else
   switch param.output
   case 'standard'
      ntr_new=ntr*nphase;
      seismic.traces=zeros(nsamp,ntr_new);
      if isfield(seismic,'headers')
         orig_headers=seismic.headers;
         nh=size(seismic.headers,1);
         seismic.headers=zeros(nh,ntr_new);
%         seismic.header_info=[seismic.header_info;{param.header,'degree','Phase angle'}];
      else
         nh=0;
         seismic.headers=zeros(1,ntr_new);
%         seismic.header_info={param.header,'degree','Phase angle'};
      end
      seismic=add_header(seismic,0,{param.header,'degree','Phase angle'});
      
      ia=1;
      ie=ntr;
 
      for ii=1:nphase
         seismic.traces(:,ia:ie)=cph(ii)*real(hseis)-sph(ii)*imag(hseis); 
         if nh > 0
            seismic.headers(1:nh,ia:ie)=orig_headers;
         end
         seismic.headers(nh+1,ia:ie)=phase(ii);
         ia=ie+1;
         ie=ie+ntr;  
      end

%    Append history field
      if isfield(seismic,'history')
         htext=['Phase rotations: ',num2str(phase),' degrees'];
         seismic=s_history(seismic,'append',htext);
      end 
 

    case 'array'
       if isfield(seismic,'headers')
          nh=size(seismic.headers,1);
          seismic.header_info=[seismic.header_info;{param.header,'degrees','Phase angle'}];
          seismic.headers=[seismic.headers;zeros(1,ntr)];
       else
          nh=0;
          seismic.header_info={param.header,'degrees','Phase angle'};
          seismic.headers=zeros(1,ntr);
       end
       temp=seismic;
       
       for ii=nphase:-1:1
          seismic(ii)=temp;
          seismic(ii).traces=cph(ii)*real(hseis)-sph(ii)*imag(hseis);
          seismic(ii).headers(nh+1,:)=phase(ii);

%         Append history field
          if isfield(seismic,'history')
             htext=['Phase rotation: ',num2str(phase(ii)),' degrees'];
             seismic(ii)=s_history(seismic(ii),'append',htext);
          end 
       end


   otherwise
      error([' Unknown output type "',param.output,'".'])

   end

end
