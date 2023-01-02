function [WorkVariXMap] = GenerateResXMap(BinPhaseDef)

%BinPhaseDef = handles.BinPhaseDef;

NbPhases = 0;
for i=1:length(BinPhaseDef)
    if BinPhaseDef(i).SelForBA
        if BinPhaseDef(i).PhaseVolProp > 0.0005  % Pierre L. 14.02.18   % changed from 0.005 to 0.0005 by PL (25.07.19) to avoid filtering phases
            % Here I want to ignore the phases that are defined but not
            % present in this domain
            % Original code by Erik: if BinPhaseDef(i).PhaseVolProp/100~=0;  % Erik D., 05.10.16
            NbPhases = NbPhases+1;

            COMP(NbPhases,:) = [BinPhaseDef(i).OxBasis,BinPhaseDef(i).Grps(BinPhaseDef(i).SelGrps).SFCompositions];
            UNC(NbPhases,:) = [0,BinPhaseDef(i).Grps(BinPhaseDef(i).SelGrps).SigmaSF];
            Names{NbPhases} = BinPhaseDef(i).DBMinName;
            VolFrac(NbPhases) = BinPhaseDef(i).PhaseVolProp/100;

            NbEl1 = length(BinPhaseDef(i).Grps(BinPhaseDef(i).SelGrps).SFLabels);
            Els1 = BinPhaseDef(i).Grps(BinPhaseDef(i).SelGrps).SFLabels;
        end
    end
end


Els{1} = 'O';
Els(2:NbEl1+1) = Els1;

NbEl = length(Els);

WorkVariXMap.NbPhases = NbPhases;
WorkVariXMap.NbEl = NbEl;
WorkVariXMap.Els = Els;
WorkVariXMap.COMP = COMP;
WorkVariXMap.UNC = UNC;

WorkVariXMap.Names = Names;
WorkVariXMap.VolFrac = VolFrac;

return