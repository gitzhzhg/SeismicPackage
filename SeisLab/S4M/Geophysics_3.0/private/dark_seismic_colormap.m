function colormatrix=dark_seismic_colormap
% Create the color matrix for the default seismic color display
%
% Written by: E. Rietsch: February 23, 2004
% Last updated:
%
%              colormatrix=dark_seismic_colormap
% OUTPUT
% colormatrix  three-column color matrix

nc=16;
up=(0:nc-1)'/nc;
down=up(end:-1:1);
eins=ones(nc,1);
nulls=zeros(nc,1);

colormatrix=[nulls,nulls,up; 
             up,up,eins; 
             1, 1, 1;
             eins,down,down;
             down,nulls,nulls];
colormatrix=colormatrix(9:end-8,:);
