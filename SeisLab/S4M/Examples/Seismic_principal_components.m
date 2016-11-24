% Seismic_principal_components
%       Example of the usage of "s_principal_components" 

keep WF     % Keep global variable WF (work flow) if it exists
presets
global S4M   %#ok


%%       Create seismic test data
seismic=s_data;
seismic.name='original seismic data';

%%       Compute and display the first principal component of each trace
%%       (each trace is a scaled version of the first principal component)
pc=s_principal_components(seismic); 
s_wplot(pc)
mytitle('First principal component of each trace (correct relative scale and polarity)')


%%       Use the first two principal components to represent each trace
%%       and compare it with the original trace (using the first four
%%       principsl components is not visibly different from the original
%%       data)
[seis1to3,aux]=s_principal_components(seismic,{'index',1:3}); 
seis1to3.name='first 3 principal components';
discrepancy=seismic-seis1to3.traces;
discrepancy.name='last 9 principal components';

lfigure
subplot(1,3,1)
   auxp=s_wplot(seismic,{'figure','old'},{'quality','high'});
subplot(1,3,2)
   s_wplot(seis1to3,{'figure','old'},{'quality','high'},{'scale',auxp.scale});
subplot(1,3,3)
   s_wplot(discrepancy,{'figure','old'},{'quality','high'},{'scale',auxp.scale})  

disp(['Fraction of the energy of each trace represented by the first ', ...
      'two principal components:'])
disp(aux.energy)

disp(['Fraction of the energy of the dataset represented by an ', ...
      'increasing number of principal components:'])
disp(aux.d)

%%       Coefficients of the principal components for each trace; 
%%       inspection shows that the first four principal components should be 
%%       enough to represent all the traces with sufficient accuracy
coeff=s_principal_components(seismic,{'output','coefficients'});
s_wplot(coeff,{'quality','spikes'})


%%       Compute and dispay the principal components
[pc,aux]=s_principal_components(seismic,{'output','pc'});
s_wplot(pc)


%%      Multiplying the matrix of principal components and the matrix of
%%      coefficients gives the matix of the seimic traces
traces=pc.traces*coeff.traces;
reconstructed=s_convert(traces,seismic.first,seismic.step);
reconstructed.name='reconstructed data';
s_compare(seismic,reconstructed)
 