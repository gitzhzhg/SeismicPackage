function info=description_substitution(info)
% Function substitutes standardized set of descriptions for input mnemonics
% e.g. CALI ==> Caliper
% INPUT
% info    cell array with curve information {mnemonuc,units,description}
% OUTPUT
% info    cell array with curve information {mnemonuc,units,description}
%             info=description_substitution(info)

io={'BADHOLE','Bad hole'
    'BS',   'Bit size'
    'BVW',  'Bulk volume of water'
    'CALI', 'Caliper'
    'DEPTH','Depth'
    'DEPTHBML','Depth below mud line'
    'DRHO', 'Bulk density correction'
    'DT',   'Compressional sonic'
    'DTC',  'Compressional sonic'
    'DTCO', 'Compressional sonic'
    'DTP',  'Compressional sonic'
    'DTS',  'Shear sonic'
    'DTSM', 'Shear sonic'
    'GR',   'Gamma ray'
    'LLD',  'Deep resistivity'
    'NPHI', 'Neutron porosity'
    'NPHISS','Neutron porosity (sandstone base)'
    'PEF',  'Photoelectric factor'
    'PHIE', 'Effective porosity'
    'Phit', 'Total porosity'
    'RHOB', 'Bulk density'
    'RHO',  'Density'
    'Sbrine','Water saturation'
    'SW',   'Water saturation'
    'T',    'Temperature'
    'VCL'   'Clay volume'
    'Vclay', 'Clay volume'
    'Vp',   'Compressional velocity'
    'Vs',   'Shear velocity'
};

index=find(ismember(lower(info(:,3)'),'undefined'));
for ii=index
  idx=find(ismember(lower(io(:,1)),lower(info{ii,1})));
  if ~isempty(idx)
    if length(idx) > 1
      error(['More than one match for mnemonic "',info{ii,1},'"'])
    else
      info(ii,3)=io(idx,2);
    end
  else
    info(ii,3)=info(ii,1);
  end
end

          
