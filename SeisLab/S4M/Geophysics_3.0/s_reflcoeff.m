function refl=s_reflcoeff(impedance)
% Function computes reflection coefficients from impedance
% Start time and sample interval of the output reflection coefficients are those
% of the impedance
%
% Written by: E. Rietsch: April 9, 2000
% Last update: March 28, 2006: add input checking
%
%             refl=s_reflcoeff(impedance)
% INPUT 
% impedance   Impedance in form of a seismic dataset
% OUTPUT
% refl        Refledction coefficient sequence

if ~istype(impedance,'seismic')
   error('Input must be a seismic dataset.')
end

refl=impedance;
refl.tag='reflectivity';
refl.name=['Reflectivity (',impedance.name,')'];
refl.last=impedance.last-impedance.step;
refl.traces(refl.traces <= 0) = NaN;
refl.traces=diff(refl.traces)./(refl.traces(1:end-1,:)+refl.traces(2:end,:));

%	Check for NaNs
%index=find(isnan(refl.traces));
%if ~isempty(index)
if any(isnan(refl.traces))
   refl.null=NaN;
end

%    Append history field
if isfield(impedance,'history')
   refl=s_history(refl,'append','');
end
