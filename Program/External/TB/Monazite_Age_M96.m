function [OutputData,OutputVariables] = Monazite_Age_M96(InputData,InputVariables,AddParameters,ElOxDataDef)
% -
% XMapTools External Function: quartz thermobarometry (all calibrations)
%
% List of output variables:
%       - Age_M96    Montel (1996)
%
% Input variables: 
%       - ThO2, UO2 and PbO in wt% (use the converter if your data
%         were calibrated in a different format).
%  
% P. Lanari & J. Laughton - Last update 15.11.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Age_M96'};
OutputData = zeros(size(InputData,1),length(OutputVariables));

%% Constants and Conversion factors
l_238 = 1.55125*10^-10*1e6;
l_235 = 9.8485*10^-10*1e6;
l_232 = 4.9475*10^-11*1e6;

f_UO2 = 0.881498;
f_ThO2 = 0.878809;
f_PbO = 0.928318;

f = @OptiAge;

Idx = find(sum(InputData,2) > 0.1);

Th = InputData(Idx,1)*1e4*f_ThO2;     % Conversion from Wt% oxide to ppm element
U = InputData(Idx,2)*1e4*f_UO2;
Pb = InputData(Idx,3)*1e4*f_PbO;

Age  = zeros(size(Idx));

for i = 1:length(Idx)
    if Th(i) > 0 && U(i) > 0 && Pb(i) > 0
        [Res1,t1] = OptiAge(1,Th(i),U(i),Pb(i),l_238,l_235,l_232);
        [Res2,t2] = OptiAge(4500,Th(i),U(i),Pb(i),l_238,l_235,l_232);
        
        Calc = 1;
        if Res1 < 0 && Res2 < 0
            Calc = 0;
        elseif Res1 > 0 && Res2 > 0
            Calc = 0;
        end
        
        if Calc
            [Age_t,Res,EXITFLAG,OUTPUT] = fzero(f,[1,4500],optimset('Display','none'),Th(i),U(i),Pb(i),l_238,l_235,l_232);
            
            if Res < 1e-9 && isequal(EXITFLAG,1)
                Age(i) = Age_t;
            else
                %keyboard   % we should never get there
            end
        end
    end
end

OutputData(Idx,:) = Age;





function [Res,t] = OptiAge(t,Th,U,Pb,l_238,l_235,l_232)
%

Pbm = Th/232*(exp(l_232*t)-1)*208 + U/238.04*0.9928*(exp(l_238*t)-1)*206 + U/238.04*0.0072*(exp(l_235*t)-1)*207;
Res = Pbm-Pb;

end


end