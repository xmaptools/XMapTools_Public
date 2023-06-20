function [ProData] = UpdateProData(ProData,iPos,Evaluation,Emin,WorkVariMod,WorkVariXMap)
%


ProData.Qfactors.Q1(1,iPos) = Evaluation.assemblage;
ProData.Qfactors.Q2(1,iPos) = Evaluation.Volume;
ProData.Qfactors.Q3(1,iPos) = Evaluation.Compositions;
ProData.Qfactors.Qt(1,iPos) = Emin;


MinStable = WorkVariMod.Names;
[Is,Ind] = ismember(MinStable,ProData.MinProp.ListMin);

for i = 1:length(Is)
    
    if Is(i)
       Where = Ind(i); 
    else
        % Create a new dimensions
        Where = length(ProData.MinProp.ListMin)+1;
        ProData.MinProp.ListMin{Where} = char(MinStable{i});
    end
    
    ProData.MinProp.ProMatrix(Where,iPos) = WorkVariMod.VolFrac(i);
    % WorkVariMod.Dens(i);
    
end

MinStable1 = WorkVariXMap.Names;
[Is1,Ind1] = ismember(MinStable1,ProData.MinPropXray.ListMin);

for i = 1:length(Is1)
    
    if Is1(i)
       Where1 = Ind1(i); 
    else
        % Create a new dimensions
        Where1 = length(ProData.MinPropXray.ListMin)+1;
        ProData.MinPropXray.ListMin{Where1} = char(MinStable1{i});
    end
    
    ProData.MinPropXray.ProMatrix(Where1,iPos) = WorkVariXMap.VolFrac(i);
    % WorkVariMod.Dens(i);
    
end

for i = 1:length(Evaluation.MinIsCompared_TEMP)
    if Evaluation.MinIsCompared_TEMP(i)
        Ind2 = find(ismember(ProData.MinComp.ListMin,Evaluation.MinNames_TEMP{i}));
        
        if Ind2 
            Where2 = Ind2;
        else
            Where2 = length(ProData.MinComp.ListMin)+1;
            ProData.MinComp.ListMin{Where2} = char(Evaluation.MinNames_TEMP{i});
        end
        
        ProData.MinComp.MinQ(Where2,iPos) = Evaluation.MinQ_TEMP(i);
        ProData.MinComp.MinQw(Where2,iPos) = Evaluation.MinQw_TEMP(i);
        
    end
end


for i = 1:length(WorkVariMod.ChemComp)
    Ind3 = find(ismember(ProData.ChemPot.ListChemPot,WorkVariMod.ChemComp{i}));
    
    if Ind3
        Where3 = Ind3;
    else
        Where3 = length(ProData.ChemPot.ListChemPot)+1;
        ProData.ChemPot.ListChemPot{Where3} = char(WorkVariMod.ChemComp{i});
    end
    
    ProData.ChemPot.ChemPotMatrix(Where3,iPos) = WorkVariMod.ChemPot(i);
    
end

return





