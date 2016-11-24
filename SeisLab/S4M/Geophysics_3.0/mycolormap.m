function [colormatrix,ncolors]=mycolormap(mapname)
% Create specific color maps in addition to the default ones
%
% Written by: E. Rietsch: October 3, 2003
% Last updated: April 20, 2007: allow input of numeric color matrix
%
%             map=mycolormap(mapname)
% INPUT
% mapname     name of colormap;
%             possible additional names are:
%             'pdf'           for probability distributions
%             'flipped_jet'
%             'seismic'       for seismic data
%             'dark_seismic"  for seismic data
%             'litho'         for pseudo-wells
% OUTPUT
% colormatrix  color matrix (three-column matrix)
% ncolors      number of different colors (number of rows of color matrix)


%       If "mapname" is numeric it is assumed to be a color matrix and returned unchanged
if isnumeric(mapname)
   if size(mapname,2) == 3
      colormatrix=mapname;
      ncolors=size(colormatrix,1);

      %       Refresh the colorbar (if it exists)
      phch = get(findall(gcf,'type','image','tag','TMW_COLORBAR'),{'parent'});
      if ~isempty(phch)
         delete(phch{1})
         colorbar
      end
      return
   else
      error('Input argument "mapname" must be the name of a color map or a numeric n x 3 matrix.')
   end
end

switch mapname

case 'flipped_jet'
%   figure
   colormatrix=colormap('jet');
   delete(gcf)
   colormatrix=[1 1 1; 
                colormatrix(27:-1:1,:); 
                0.8       0.8       0.8
                0.6       0.6       0.6
                0.4       0.4       0.4
                0.2       0.2       0.2
               colormatrix(38:end,:)];

case 'pdf'
   colormatrix=[ ...
                1         1         1
                0.8       0.8       0.8
                0.6       0.6       0.6
                0.4       0.4       0.4
                0.2       0.2       0.2
                  0         0    0.5625
                  0         0    0.6250
                  0         0    0.6875
                  0         0    0.7500
                  0         0    0.8125
                  0         0    0.8750
                  0         0    0.9375
                  0         0    1.0000
                  0    0.0625    1.0000
                  0    0.1250    1.0000
                  0    0.1875    1.0000
                  0    0.2500    1.0000
                  0    0.3125    1.0000
                  0    0.3750    1.0000
                  0    0.4375    1.0000
                  0    0.5000    1.0000
                  0    0.5625    1.0000
                  0    0.6250    1.0000
                  0    0.6875    1.0000
                  0    0.7500    1.0000
                  0    0.8125    1.0000
                  0    0.8750    1.0000
                  0    0.9375    1.0000
                  0    1.0000    1.0000
                  0.0625    1.0000    0.9375
                  0.1250    1.0000    0.8750
                  0.1875    1.0000    0.8125
                  0.2500    1.0000    0.7500
                  0.3125    1.0000    0.6875
                  0.3750    1.0000    0.6250
                  0.4375    1.0000    0.5625
                  0.5000    1.0000    0.5000
                  0.5625    1.0000    0.4375
                  0.6250    1.0000    0.3750
                  0.6875    1.0000    0.3125
                  0.7500    1.0000    0.2500
                  0.8125    1.0000    0.1875
                  0.8750    1.0000    0.1250
                  0.9375    1.0000    0.0625
                  1.0000    1.0000         0
                  1.0000    0.9375         0
                  1.0000    0.8750         0
                  1.0000    0.8125         0
                  1.0000    0.7500         0
                  1.0000    0.6875         0
                  1.0000    0.6250         0
                  1.0000    0.5625         0
                  1.0000    0.5000         0
                  1.0000    0.4375         0
                  1.0000    0.3750         0
                  1.0000    0.3125         0
                  1.0000    0.2500         0
                  1.0000    0.1875         0
                  1.0000    0.1250         0
                  1.0000    0.0625         0
                  1.0000         0         0
                  0.9375         0         0
                  0.8750         0         0
                  0.8125         0         0
                  0.7500         0         0
                  0.6875         0         0
                  0.6250         0         0
                  0.5625         0         0
                  0.5000         0         0 
                 ];

case 'seismic'
   colormatrix=default_seismic_colormap;

case 'dark_seismic'
   colormatrix=dark_seismic_colormap;

case 'litho'
   colormatrix=[0.6 0.6 0.6;
                1   1   0  ];

otherwise
   colormatrix=colormap(mapname);

end

colormap(colormatrix);


%       Refresh the colorbar (if it exists)
phch = get(findall(gcf,'type','image','tag','TMW_COLORBAR'),{'parent'});
if ~isempty(phch)
   delete(phch{1})
   colorbar
end


if nargout == 0
   clear colormatrix
elseif nargout > 1
   ncolors=size(colormatrix,1);
end
