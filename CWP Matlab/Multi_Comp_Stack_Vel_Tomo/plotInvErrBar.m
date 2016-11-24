%==============================================================================
% Image results of inversion in MNC layer -- Ilya's ORT notation
%==============================================================================

clear all;   close all;

  load Result.PSVinv01;
% title('InvMNCtsv.outM1');

100*std(Result)./mean(Result)

Ndim = 6;   nn = [1:Ndim]; 
AnisCoef(:,1) = Result(:,3);   AnisCoef(:,2) = Result(:,4);
AnisCoef(:,3) = Result(:,7);   AnisCoef(:,4) = Result(:,8);
AnisCoef(:,5) = Result(:,11);  AnisCoef(:,6) = Result(:,12);

AnisCoefCor = [0.15,  0.05,  0.20,  0.10,  0.20,  0.05];
AnisCoefMean = mean(AnisCoef)  
AnisCoefStd  = std(AnisCoef)  

figure;
subplot(2,1,1);
set(gca,'Fontsize', 16);
hold on;   grid on;   
pl = plot(nn, AnisCoefCor, 'k*');
set(pl, 'LineWidth', 3);   set(pl, 'MarkerSize', 3);
pl = plot(nn, AnisCoefCor, 'ko');
set(pl, 'LineWidth', 3);   set(pl, 'MarkerSize', 3);

for ij=1:Ndim
   y1 = AnisCoefCor(ij) - AnisCoefStd(ij);
   y2 = AnisCoefCor(ij) + AnisCoefStd(ij);
%  y1 = AnisCoefMean(ij) - AnisCoefStd(ij);
%  y2 = AnisCoefMean(ij) + AnisCoefStd(ij);
   x = [ij, ij];    y = [y1,  y2]; 
   pp = plot(x,y,'k-');    set(pp, 'LineWidth', 2);  
   x = [ij-0.05, ij+0.05];   y = [y1, y1];
   pp = plot(x,y,'k-');    set(pp, 'LineWidth', 2);  
   y = [y2, y2];
   pp = plot(x,y,'k-');    set(pp, 'LineWidth', 2);
end;
axis([0.8  6.2  -0.05  0.301]);
set(gca, 'XTick', [1:9]);
  set(gca, 'XTickLabel', ...
      ['    ';'    ';'    ';'    ';'    ';'    ';'    ';'    ';'    ']);
% set(gca, 'XTickLabel', ...
%     ['EPS1';'EPS2';'DEL1';'DEL2';'GAM1';'GAM2';'ZET1';'ZET2';'ZET3']);
% ylabel('Values  of  anisotropic  parameters');

