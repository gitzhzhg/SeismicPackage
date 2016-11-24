%==============================================================================
% Plot inversion results of P- and SV-moveouts for parameters of VTI media
%==============================================================================

close all;   clear all;

% Load stuff
load Result.PSVtti1;    
Vp0 = Result(:,1);      Vs0 = Result(:,2);     
epsilon = Result(:,3);  delta = Result(:,4);   
nu = Result(:,5);       beta = Result(:,6);   

% Determine the scales
VP0B = Vp0(1) - 0.10;            VP0E = Vp0(1) + 0.10;
VS0B = Vs0(1) - 0.04;            VS0E = Vs0(1) + 0.04;
epsilonB = epsilon(1) - 0.10;    epsilonE = epsilon(1) + 0.10;   
deltaB   = delta(1) - 0.10;      deltaE   = delta(1) + 0.10;
eta = (epsilon(1) - delta(1))/(1 + 2*delta(1));   dd = [deltaB: 0.01: deltaE];
betaB = beta(1) - 4.0;           betaE = beta(1) + 4.0;   
nuB   = nu(1) - 4.0;             nuE   = nu(1) + 4.0;
%==============================================================================

figure;
subplot(2,2,2);   hold on;   grid on;   axis('square');
set(gca, 'FontSize', 12);
set(gca, 'Xtick', [VP0B-1:0.05:VP0E+1]);
set(gca, 'Ytick', [VS0B-1:0.02:VS0E+1]);
axis([VP0B-0.0001, VP0E+0.0001, VS0B-0.0001, VS0E+0.0001]);
plot(Vp0, Vs0, 'ro', 'LineWidth', 2, 'MarkerSize', 2);
plot(Vp0(1), Vs0(1), 'b+', 'LineWidth', 4, 'MarkerSize', 60);
xlabel('V_{P0}', 'FontName', 'Times', 'FontAngle', 'Italic', 'FontSize', 18);
ylabel('V_{S0}', 'FontName', 'Times', 'FontAngle', 'Italic', 'FontSize', 18);

%==============================================================================

subplot(2,2,1);   hold on;   grid on;   axis('square');
set(gca, 'FontSize', 12);
set(gca, 'Xtick', [-1:0.05:1]);
set(gca, 'Ytick', [-1:0.05:1]); 
axis([deltaB-0.0001,    deltaE+0.0001, epsilonB-0.0001,  epsilonE+0.0001]);   
plot(delta, epsilon, 'ro', 'LineWidth', 2, 'MarkerSize', 2);
%plot(dd, eta*(1+2*dd)+dd, 'k--', 'LineWidth', 2);
plot(delta(1), epsilon(1), 'b+', 'LineWidth', 4, 'MarkerSize', 60); 
xlabel('d', 'FontName', 'Symbol', 'FontSize', 20);
ylabel('e', 'FontName', 'Symbol', 'FontSize', 20);
%==============================================================================

length(Vp0)
[mean(Vp0), mean(Vs0), mean(epsilon), mean(delta), mean(nu), mean(beta)]
[std(Vp0), std(Vs0), std(epsilon), std(delta), std(nu), std(beta)]

figure;
subplot(2,2,1);   hold on;   grid on;   axis('square');
set(gca, 'FontSize', 12);
set(gca, 'Xtick', [betaB-10:2:betaE+10]);
set(gca, 'Ytick', [nuB-10:2:nuE+10]);
axis([betaB-0.0001,    betaE+0.0001, nuB-0.0001,  nuE+0.0001]);
plot(beta, nu, 'ro', 'LineWidth', 2, 'MarkerSize', 2);
plot(beta(1), nu(1), 'b+', 'LineWidth', 4, 'MarkerSize', 60);
xlabel('b', 'FontName', 'Symbol', 'FontSize', 20);
ylabel('n', 'FontName', 'Symbol', 'FontSize', 20);
%==============================================================================

