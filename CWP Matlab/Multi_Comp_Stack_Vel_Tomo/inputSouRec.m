%==============================================================================
% Input of sources and receivers
%==============================================================================

function [] = inputSouRecInt;

%==============================================================================

global Source Receiv Npair 

%==============================================================================

% Input x-, y-, and z-coordinates of the sources
xx = [-0.2: 0.4: 0.2];
yy = [-0.2: 0.4: 0.2];
%xx = [0, 0];
%yy = [0];

Npair = length(xx)*length(yy);
[XX, YY] = meshgrid(xx, yy); 

xSource = reshape(XX, Npair, 1);  
ySource = reshape(YY, Npair, 1); 
zSource = zeros(Npair,1);

% Input x-, y-, and z-coordinates of the receivers
xReceiv = xSource;   
yReceiv = ySource; 
zReceiv = zSource; 

% Construct the source and receiver matrices
Source(:,:) = [xSource(:), ySource(:), zSource(:)]';
Receiv(:,:) = [xReceiv(:), yReceiv(:), zReceiv(:)]'; 

%==============================================================================

