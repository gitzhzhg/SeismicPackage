function s_synthetic4landmark(synthetic,filename)
% Function writes one-trace synthetic to ASCII file in Landmark format.
% Written by: E. Rietsch
% Last updated

%           s_synthetic4landmark(synthetic,filename)
% INPUT
% synthetic synthetic in form of a seismic structure
% filename  filename (optional)
%           Default: PROGRAM_lmg_synthetic.txt where PROGRAM is the 
%           name of the script which created it

if nargin == 1
   filename='';
end

[nsamp,ntr]=size(synthetic.traces);
if ntr > 1
   error(' Synthetic has more than one trace')
end
 
text={'ASCII synthetic';
     ['Number of samples: ',num2str(nsamp)]; 
     ['First sample: ',num2str(synthetic.first),' ms'];
     ['Last sample: ',num2str(synthetic.last),' ms'];
     ['Sample interval: ',num2str(synthetic.step),' ms']};

wr_columns(filename,synthetic.traces,text)
