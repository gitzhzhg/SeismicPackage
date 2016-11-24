function num=evaluatetraces(trace,t)
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

num.nan=false;
num.inf=false;
num.stdhist=[];
num.std=[];
num.kurt=[];
num.skew=[];
num.smooth=[];
num.total=0;
f=figure;
%calculate parameters of trace
mint=min(trace);
maxt=max(trace);

%calculate histogram of trace
numofbins=1000;
bins=(mint:(maxt-mint)/numofbins:maxt);
histo=histc(trace,bins);

%standard deviation test on histogram
num.stdhist=std(histo);
num.std=std(trace);

% gaussian tests
num.skew=skewness(histo);

num.kurt=kurtosis(histo);

% smooth test
t2=[trace,0];t2=t2(2:end);
smoo=((t2-trace)/maxt);
num.smooth=max(smoo);%histc(smoo,[(max(smoo)-.1*max(smoo)),max(smoo)]);

% spectrum
[spec2, f2]=fftrl(trace,t);
spec2=abs(spec2);
binspec=(min(spec2):((max(spec2)-min(spec2))/numofbins*5):max(spec2));
histspec=histc(spec2,binspec);
plot(spec2)
num.spck=kurtosis(spec2);
num.spcs=skewness(spec2);

if num.skew<.5
    num.total=num.total+4;
elseif num.skew<1
    num.total=num.total+3;
elseif num.skew<3
    num.total=num.total+2;
elseif num.skew<5
    num.total=num.total+1;
elseif num.skew<7
    num.total=num.total+0;
end

if num.kurt<3
    num.total=num.total+4;
elseif num.kurt<5
    num.total=num.total+3;
elseif num.kurt<7
    num.total=num.total+2;
elseif num.kurt<9
    num.total=num.total+1;
elseif num.kurt<12
    num.total=num.total+0;
end

if sum(num.smooth)<.05
    num.total=num.total+5;
elseif sum(num.smooth)<.1
    num.total=num.total+4;
elseif sum(num.smooth)<.2
    num.total=num.total+3;
elseif sum(num.smooth)<.3
    num.total=num.total+2;
elseif sum(num.smooth)<.5
    num.total=num.total+1;
end



if isnan(num.std)
    num.nan=true;
    num.total=0;
end
if isinf(num.std)
    num.inf=true;
    num.total=0;
end

if max(trace)>(10^100) || max(trace)<(10^-100)
    num.inf=true;
end
delete(f);




