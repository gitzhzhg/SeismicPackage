function [trout,envsm,ma]=aec(trin,sampint,op_length,trip,profileflag)
% AEC: automatic envelope correction, a better AGC.
%
% Syntax
%  trout=aec(trin);
%  trout=aec(trin,sampint,op_length,trip)
%  trout=aec(trin,sampint,op_length,trip,profileflag)
%
% Description 
%   AEC performs an automatic amplitude adjustment.
%
% Method
%   1) Compute Hilbert envelope of the input trace TRIN
%   2) Convolve envelope with triangular smoother of half-length
%      OP_LENGTH
%   3) Divide input trace by smoothed envelope
%   4) Balance the output to a maximum of 1.0
%
% Inputs
%   trin      = input trace or gather of traces.
%   t or sampint   = sample interval for trin 
%               For backwards compatibility, if sampint is supplied as a time coordinate 
%               vector, the difference of the first two elements is used as
%               the sample interval.
%   **********  Default is 0.001 seconds.  (1 millisecond) ********
%   op_length = half-length of triangular smoother in seconds
%   **********  default is 1/8th of the trace length *******
%               ***** must be less than half the trace length *****
%   trip      = front end time before which the smoothed envelope is
%               set to a constant
%   **********  default is op_length/10 *******
%  profileflag= 0 or 1, this applies only if the input is a profile of
%               traces. In that case, if profileflag is 0, then each trace
%               is divided by its own envelope, and this is called single
%               trace more. If profileflag is 1, then the hilbert
%               envelopes of all the traces are stacked and the result is
%               smoothed to get a single smooth envelope. Each trace is
%               then divided by this envelope and normalized.
%   ***********  default is 0 ***********
% Outputs
%   trout     = output trace or gather of traces
%   envsm     = smoothed hilbert envelope of trace or traces
%   ma        = maximum absolut value of the corretect trace befor
%               normalization to 1.
% NOTES:
%   1) To remove trace normalization to 1, trout_unnorm=trout*ma; (single
%           trace) or trout_unnorm(:,k)=trout(:,k)*ma(a); (multitrace)
%   2) To recover the input trace: trin_recovered=trout*ma.*envsm; (single
%           trace) or trin_recovered(:,k)=trout(:,k)*ma.*envsm(:,k) (multitrace)
%
% by G.F. Margrave, May 1991-2016 (Henry Bland and Kevin Hall too)
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
   
% set defaults
if nargin < 2 || isempty(sampint)
    sampint = 0.001;   % sample interval in seconds
else
    % Backwards compatibility with time coordinate vector
    if length(sampint) > 1
        sampint = sampint(2) - sampint(1);
    end
end
if nargin < 3 || isempty(op_length)
    op_length = sampint*length(trin)/8;
end
if nargin < 4 || isempty(trip);
    trip=op_length/10.;
end
if(nargin<5)
    profileflag=0;
end

% Number of traces
[nt,ntr] = size(trin);

% if (2*op_length/sampint) >= (nt-2)
%     error('Operator length too long, must be less than 1/2 trace length.');
% end


% Turn scalar op_length and trip into vectors if necessary
% if length(op_length) ~= ntr    
%     op_length=ones(1,ntr) * op_length;
% end
% if length(trip) ~= ntr
%     trip = ones(1,ntr) * trip;
% end


% Handle 2-D or 1-D invocation differently
if numel(trin) ~= length(trin)
    % 2-D invocation
    if(profileflag)
        trout=zeros(size(trin));
        ma=zeros(1,ntr);
        [envsm,envstack]=average_envelope(trin,sampint,op_length,trip);
        for k=1:ntr
            tmp=trin(:,k)./envsm;
            ma(k)=max(abs(tmp));
            trout(:,k)=tmp/ma(k);
        end
    else
        trout=zeros(size(trin));
        envsm=trout;
        ma=zeros(1,ntr);
        for k=1:ntr
            [trout(:,k),envsm(:,k),ma(k)] = aec_vector(trin(:,k)',sampint,op_length,trip);
        end
    end
    
else
    % 1-D invocation
    pivotted=(ntr==1);
    if pivotted
        trin=trin.';
    end
    [trout,envsm,ma]= aec_vector(trin,sampint,op_length,trip);
    if pivotted
        trout = trout';
        envsm=envsm';
    end
end

end


function [trout_,envsm,ma] = aec_vector(trin_,sampint_,op_length_,trip_)
% the original trace is trin_=trout*ma.*envsm

% double the operator length
op2=op_length_*2;
% form new trace padded to a power of 2
trinew=padpow2(trin_,0);
% compute the envelope
env=abs(hilbm(trinew));
env=env(1:length(trin_));
% compute the smoothed envelope
nop=round(op2/sampint_)+1;
envsm=conv(env,triang(nop));
% grab the central length(trin) samples
envsm=envsm(round(nop/2):length(trin_)+round(nop/2)-1);
% stabilize the envelope at the ends
ntrip=round(trip_/sampint_)+1;

%for some reason, envsm(ntrip+1:length(envsm) is sometimes a column vector
%and sometimes a row vector. KWH, 2014
envsm_part = envsm(ntrip+1:length(envsm));
m = size(envsm(ntrip+1:length(envsm)),1);
if m > 1
    envsm_part = envsm_part';
end

envsm=[envsm(ntrip)*ones(1,ntrip) envsm_part];
envsm=[envsm(1:length(envsm)-ntrip) envsm(length(envsm)-ntrip)*ones(1,ntrip)];
% correct the trace
if(sum(abs(envsm))==0)
    trout_=zeros(size(trin_));
    ma=0;
else
    trout_=trin_./envsm;
    % balance the output to have a maximum of 1
    ma=max(abs(trout_));
    %ma=1;
    trout_=trout_/ma;
end


% balance the output to have the same mean power as input
%trout=balans(trout,trin);
end

function [envsm,envstack]=average_envelope(trin_,sampint_,op_length_,trip_)
%get the average envelope and smooth it
[nt_,ntr_]=size(trin_);
envstack=zeros(nt,1);
for k=1:ntr_
    tr=padpow2(trin_(:,k),0);%make sure its a power of 2 for hilbert
    envstack=envstack+abs(hilbm(tr));
end
envstack=envstack(1:nt_);
% double the operator length
op2=op_length_*2;
% compute the smoothed envelope
nop=round(op2/sampint_)+1;
envsm=conv(envstack,triang(nop));
% grab the central length(trin) samples
envsm=envsm(round(nop/2):length(trin_)+round(nop/2)-1);
% stabilize the envelope at the ends
ntrip=round(trip_/sampint_)+1;

%for some reason, envsm(ntrip+1:length(envsm) is sometimes a column vector
%and sometimes a row vector. KWH, 2014
envsm_part = envsm(ntrip+1:length(envsm));
m = size(envsm(ntrip+1:length(envsm)),1);
if m > 1
    envsm_part = envsm_part';
end

envsm=[envsm(ntrip)*ones(1,ntrip) envsm_part];
envsm=[envsm(1:length(envsm)-ntrip) envsm(length(envsm)-ntrip)*ones(1,ntrip)];
end

function w=triang(n)

if(iseven(n))
    n2=n/2;
    d=1/n2;
    val=d*(1:n2)-d/2;
    %val=linspace(0,1,n2+2);
    w=[val val(n2:-1:1)];
    w=w';    
else
    n2=ceil(n/2);
    d=1/n2;
    val=d*(1:n2-1);
    w=[val 1 val(n2-1:-1:1)];
    w=w';
end
end
