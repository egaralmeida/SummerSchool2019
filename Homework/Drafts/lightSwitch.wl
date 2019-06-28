(* ::Package:: *)

lightSwitch[]:=Module[{},
history={};
startTime=Total[DateList[][[-3;;]]*{3600,60,1}];
currTime :=Total[DateList[][[-3;;]]*{3600,60,1}];

switchHistory[time_]:=1/;EvenQ[Length[Select[history,#<time&]]];
switchHistory[time_]:=0/;OddQ[Length[Select[history,#<time&]]];

Button["Switch",history=Append[history,currTime-startTime]]

Dynamic[Refresh[Plot[switchHistory[time],{time,Max[0,currTime-startTime-10],currTime-startTime},PlotRange->{{Max[0,currTime-startTime-10],currTime-startTime+1},{0,1}}]],UpdateInterval->0.1]
]
