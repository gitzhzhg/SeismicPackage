function stackf=filter_stack(stack,t,flow,fhigh,varargin)
% FILTER_STACK: applies bandpass filter to a stacked section in 2D or 3D
%
% stackf=filter_stack(stack,t,flow,fhigh,varargin)
%
%
% stack ... stacked section as a 2D or 3D matrix of traces. 
% t ... time coordinate for stack
% flow ... lowest frequency to pass (set to 0 for low pass)
% fhigh ... highest frequency to pass (set to 0 for high pass)
% Possible name-argument pairs
% 'method' ... either 'filtf' or 'butterband'
% ************ default 'butterband' ************
% 'dflow' ... filter taper width on low end (only used for method='filtf').
%             Must be greater than 0 and less than flow.
% ************ default = flow/2 *********
% 'dfhigh' ... filter taper width on high end (only used for method='filtf')
% ************ default = 20 Hz *********
% 'phase' ... either 0 or 1 indicating zero phase or minimum phase
% ************ default = 0 ***********
% 'norder' ... order of Butterworth filter (only used for method='butterband')
% ********** default = 4 for phase =0, 8 for phase=1 *********
% 
% stackf ... filtered stack
%
% G.F. Margrave, Devon Canada, May 2016
%
% NOTE: This SOFTWARE may be used by any individual or corporation for any purpose
% with the exception of re-selling or re-distributing the SOFTWARE.
% By using this software, you are agreeing to the terms detailed in this software's
% Matlab source file.

% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by
% its 'AUTHOR' (identified above) and the CREWES Project.  The CREWES
% project may be contacted via email at:  crewesinfo@crewes.org
%
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) This SOFTWARE may be used by any individual or corporation for any purpose
%    with the exception of re-selling or re-distributing the SOFTWARE.
%
% 2) The AUTHOR and CREWES must be acknowledged in any resulting publications or
%    presentations
%
% 3) This SOFTWARE is provided "as is" with no warranty of any kind
%    either expressed or implied. CREWES makes no warranties or representation
%    as to its accuracy, completeness, or fitness for any purpose. CREWES
%    is under no obligation to provide support of any kind for this SOFTWARE.
%
% 4) CREWES periodically adds, changes, improves or updates this SOFTWARE without
%    notice. New versions will be made available at www.crewes.org .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE

%check for 3D
sz=size(stack);
threeD=0;
if(length(sz)==3)
    threeD=1;
end

nt=sz(1);
if(length(t)~=nt)
    error('t and stack have incompatible sizes');
end

%parse varargin
nargs=length(varargin);
if(~iseven(nargs))
    error('extra arguments must be name-value pairs');
end
method='butterband';
dflow=flow/2;
dfhigh=20;
phase=[];
norder=[];
fnyq=.5/(t(2)-t(1));
for k=1:2:nargs
    name=varargin{k};
    if(~ischar(name))
        error(['expecting the ' int2str(k) 'th extra argument to be a string']);
    end
    switch name
        case 'method'
            val=varargin{k+1};
            if(~ischar(val))
                error('method must be a string')
            end
            if(~strcmp(val,'butterband')&&~strcmp(val,'filtf'))
                error('method must be either ''butterband'' or ''filtf''')
            end
            method=val;
        case 'dflow'
            val=varargin{k+1};
            if(~isnumeric(val))
                error(' dflow must be a number');
            end
            if(val<0 || val>flow)
                error('dflow must be greater than 0 and less than flow')
            end
            dflow=val;
        case 'dfhigh'
            val=varargin{k+1};
            if(~isnumeric(val))
                error(' dhigh must be a number');
            end
            if(val<0 || val>fnyq)
                error('dflow must be greater than 0 and less than flow')
            end
            dflow=val;
        case 'phase'
            val=varargin{k+1};
            if(~isnumeric(val))
                error(' phase must be a number');
            end
            if(val~=0 && val~=1)
                error('phase must be greater 0 or 1')
            end
            phase=val;
        case 'norder'
            val=varargin{k+1};
            if(~isnumeric(val))
                error(' norder must be a number');
            end
            if(val<1 || val>20)
                error('norder must an integer between 1 and 20')
            end
            norder=val;
        otherwise
            error(['extra input argument ' int2str(k) ' unrecognized'])
    end
end
    if(isempty(phase))
        phase=0;
    end
    if(isempty(norder))
        if(phase==0)
            norder=4;
        else
            norder=1;
        end
    end
    
    if(strcmp(method,'filtf'))
        if(flow==0)
            fmin=0;
        else
            fmin=[flow dflow];
        end
        if(fhigh==0)
            fmax=0;
        else
            fmax=[fhigh dfhigh];
        end
    end


if(threeD)
    
    disp('filter in 3D')
    
    nxlines=sz(2);
    nilines=sz(3);
    nt=sz(1);
    
    
    stackf=zeros(nt,nxlines,nilines);
    
    small=100*eps;
    
    t0=clock;
    ievery=10;
    for kx=1:nxlines
        for ki=1:nilines
            tmp=stack(:,kx,ki);
            if(sum(abs(tmp))>small)%avoid filtering a zero trace
                switch method
                    case 'filtf'
                       stackf(:,kx,ki)=filtf(tmp,t,fmin,fmax,phase);
                    case 'butterband'
                       stackf(:,kx,ki)=butterband(tmp,t,flow,fhigh,norder,phase);
                end
            end
        end
        if(rem(kx,ievery)==0)
            tnow=clock;
            time_used=etime(tnow,t0);
            time_per_xline=time_used/kx;
            time_remaining=(nxlines-kx)*time_per_xline;
            disp(['filter_stack: finished xline ' int2str(kx) ' of ' int2str(nxlines)])
            disp(['estimated time remaining ' int2str(time_remaining) ' sec'])
        end
    end
else
    ntr=size(stack,2);
    
    stackf=zeros(size(stack));
    
    small=100*eps;
    
    t0=clock;
    ievery=1000;
    for k=1:ntr
        tmp=stack(:,k);
        
        if(sum(abs(tmp))>small)%avoid filtering a zero trace
                switch method
                    case 'filtf'
                       stackf(:,k)=filtf(tmp,t,fmin,fmax,phase);
                    case 'butterband'
                       stackf(:,k)=butterband(tmp,t,flow,fhigh,norder,phase);
                end
        end
        if(rem(k,ievery)==0)
            tnow=clock;
            time_used=etime(tnow,t0);
            time_per_trace=time_used/k;
            time_remaining=(ntr-k)*time_per_trace;
            disp(['filter_stack: finished trace ' int2str(k) ' of ' int2str(ntr)])
            disp(['estimated time remaining ' int2str(time_remaining) ' sec'])
        end
    end

end
   
