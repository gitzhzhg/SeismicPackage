function wavelet=s_add_header4phase(wavelet,varargin)
% Function computes the amount of constant-phase shift and time shift
% required to match a zero-phase wavelet with the same amplitude spectrum
% to the the signal on each trace. Thus applying the negative phase shift
% and time shift will convert the signal approximately into zero-phase
% signals centered at time zero. This function makes only sense for 
% wavelets. The tme shifts and phase shifts computed are stored in trace
% headers 
%
% Written by: E. Rietsch: September 21, 2009
% Last updated:
%
%
% INPUT
% wavelet   seismic dataset with wavelets
% varargin    one or more cell arrays; the first element of each cell array 
%           is a keyword, the other elements are parameters. 
%           Presently, keywords are:
%     'phaseheader'  string with the header mnemonic for the phase shift 
%           (in degrees). The phase is rounded to the nearest degree.
%           No header is created if this string is empty.
%           Default: {'phaseheader','phase_shift'}
%     'traceheader'  string with the header mnemonic for the time shift 
%           (in ms).  The time is rounded to the nearest millisecond.
%           No header is created if this string is empty.
%           Default: {timeheader','time_shift'}
% OUTPUT
% wavelet   input dataset with headers for the phase shift and time shift
%           added

%       Set defaults od input parameters
param.phaseheader='phase_shift';
param.timeheader='time_shift';

%       Replace defaults by actual input arguments
param=assign_input(param,varargin);

%       Compute phase shift and time shift
[phase,t0]=det_phase(wavelet.traces,true);

if ~isempty(param.phaseheader)
    wavelet=add_header(wavelet,round(phase),{param.phaseheader,'degrees','Phase'});
end

zerotime=round(wavelet.first+t0*wavelet.step);
if ~isempty(param.timeheader)
    wavelet=add_header(wavelet,zerotime,{param.timeheader,'ms','Zero time'});
end

%         Append history field
if isfield(wavelet,'history')
   htext='Compute phase and zero time';
   wavelet=s_history(wavelet,'append',htext);
end 


