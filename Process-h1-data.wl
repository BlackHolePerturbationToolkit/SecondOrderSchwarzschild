(* ::Package:: *)

(* ::Section::Closed:: *)
(*Load precalculated data*)


M=1;
(*r0=710/100M;
r0S=ToString[If[IntegerQ[r0],r0,N[r0]]];*)
(*lmaxret=50;*)(* Number of l modes for retarded fields *)


Print["Generating h1 files:
r0="<>ToString[r0]<>"
lmaxret="<>ToString[lmaxret]<>"
h1dir="<>h1dir];


r0=Rationalize[r0];


grid=Import[FileNameJoin[{h1dir,"no-ddh/h1ret/h1-l0m0.h5"}],{"Datasets","grid"}]/.{Sequence[N@r0,N@r0]->Sequence[N@r0,N@r0], N@r0->Sequence[N@r0,N@r0]};


\[CapitalDelta]rgrid=Rationalize[grid-r0,10^-10]/.{{a___,0,0,b___}:>{a,0,0,b}, 0->Sequence[0,0]};


\[CapitalDelta]rderivgrid=\[CapitalDelta]rgrid/.{a___,0,0,b___}:>{a,Left,Right,b};


r0i=Position[\[CapitalDelta]rgrid,0];


fields={1,2,3,4,5,6,7,8,9,10};


With[{l=2,m=2},{sizel,sizer}=(Dimensions/@Import[FileNameJoin[{h1dir, "no-ddh","h1ret","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{"Datasets",{"inhom_left","inhom_right"}}])[[All,1]]];


If[!(r0i[[1,1]]+1==r0i[[2,1]]&&Dimensions[r0i]=={2,1}&&sizel==r0i[[1,1]]&&sizel+sizer==Length[\[CapitalDelta]rgrid]),Abort[]];


ClearAll[data]


data[l_,m_]:=data[l,m]=Check[Join@@Import[FileNameJoin[{h1dir, "no-ddh","h1ret","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{"Datasets",{"inhom_left","inhom_right"}}],Print[{l,m}]];


hret[1,l_,m_]:=hret[1,l,m]=Complex@@@data[l,m][[All,{1,2}]];


dhret[1,l_,m_]:=dhret[1,l,m]=Complex@@@data[l,m][[All,{1,2}+2]];


hret[2,l_,m_]/;l>1:=hret[2,l,m]=Complex@@@data[l,m][[All,{21,22}]];


dhret[2,l_,m_]/;l>1:=dhret[2,l,m]=Complex@@@data[l,m][[All,{21,22}+2]];


hret[2,1,m_]:=hret[2,1,m]=Complex@@@data[1,m][[All,{17,18}]]


dhret[2,1,m_]:=dhret[2,1,m]=Complex@@@data[1,m][[All,{17,18}+2]]


hret[2,0,m_]:=hret[2,0,m]=Complex@@@data[0,m][[All,{13,14}]]


dhret[2,0,m_]:=dhret[2,0,m]=Complex@@@data[0,m][[All,{13,14}+2]]


hret[3,l_,m_]:=hret[3,l,m]=Complex@@@data[l,m][[All,{5,6}]];


dhret[3,l_,m_]:=dhret[3,l,m]=Complex@@@data[l,m][[All,{5,6}+2]];


hret[4,l_,m_]:=hret[4,l,m]=Complex@@@data[l,m][[All,{25,26}]];


dhret[4,l_,m_]:=dhret[4,l,m]=Complex@@@data[l,m][[All,{25,26}+2]];


hret[4,1,m_]:=hret[4,1,m]=Complex@@@data[1,m][[All,{21,22}]];


dhret[4,1,m_]:=dhret[4,1,m]=Complex@@@data[1,m][[All,{21,22}+2]];


hret[5,l_,m_]:=hret[5,l,m]=Complex@@@data[l,m][[All,{9,10}]];


dhret[5,l_,m_]:=dhret[5,l,m]=Complex@@@data[l,m][[All,{9,10}+2]];


hret[6,l_,m_]:=hret[6,l,m]=Complex@@@data[l,m][[All,{13,14}]];


dhret[6,l_,m_]:=dhret[6,l,m]=Complex@@@data[l,m][[All,{13,14}+2]];


hret[6,0,m_]:=hret[6,0,m]=Complex@@@data[0,m][[All,{9,10}]]


dhret[6,0,m_]:=dhret[6,0,m]=Complex@@@data[0,m][[All,{9,10}+2]]


hret[7,l_,m_]:=hret[7,l,m]=Complex@@@data[l,m][[All,{17,18}]];


dhret[7,l_,m_]:=dhret[7,l,m]=Complex@@@data[l,m][[All,{17,18}+2]];


hret[8,l_,m_]:=hret[8,l,m]=Complex@@@data[l,m][[All,{9,10}]];


dhret[8,l_,m_]:=dhret[8,l,m]=Complex@@@data[l,m][[All,{9,10}+2]];


hret[8,1,m_]:=hret[8,1,m]=Complex@@@data[1,m][[All,{5,6}]];


dhret[8,1,m_]:=dhret[8,1,m]=Complex@@@data[1,m][[All,{5,6}+2]];


hret[9,l_,m_]:=hret[9,l,m]=Complex@@@data[l,m][[All,{1,2}]];


dhret[9,l_,m_]:=dhret[9,l,m]=Complex@@@data[l,m][[All,{1,2}+2]];


hret[10,l_,m_]:=hret[10,l,m]=Complex@@@data[l,m][[All,{5,6}]];


dhret[10,l_,m_]:=dhret[10,l,m]=Complex@@@data[l,m][[All,{5,6}+2]];


Print["Loading retarded fields"];


Do[hret[i,l,m],{i,{1,2,3,6}},{l,(*1*)0,lmaxret},{m,l,1(*0*),-2}];
Do[hret[i,l,m],{i,{4,5}},{l,1,lmaxret},{m,l,1(*0*),-2}];
Do[hret[i,l,m],{i,{7}},{l,2,lmaxret},{m,l,1(*0*),-2}];
Do[hret[i,l,m],{i,{8,9}},{l,1,lmaxret},{m,l-1,0,-2}];
Do[hret[i,l,m],{i,{10}},{l,2,lmaxret},{m,l-1,0,-2}];


Do[dhret[i,l,m],{i,{1,2,3,6}},{l,0,(*1,*)lmaxret},{m,l,1(*0*),-2}];
Do[dhret[i,l,m],{i,{4,5}},{l,1,lmaxret},{m,l,1(*0*),-2}];
Do[dhret[i,l,m],{i,{7}},{l,2,lmaxret},{m,l,1(*0*),-2}];
Do[dhret[i,l,m],{i,{8,9}},{l,1,lmaxret},{m,l-1,0,-2}];
Do[dhret[i,l,m],{i,{10}},{l,2,lmaxret},{m,l-1,0,-2}];


Do[hret[i,l,m]=0.+0.I,{i,{1,2,3,6}},{l,(*1*)2,lmaxret,2},{m,{0}}];
Do[hret[i,l,m]=0.+0.I,{i,{4,5}},{l,2,lmaxret,2},{m,{0}}];
Do[hret[i,l,m]=0.+0.I,{i,{7}},{l,2,lmaxret,2},{m,{0}}];


Do[dhret[i,l,m]=0.+0.I,{i,{1,2,3,6}},{l,(*1*)2,lmaxret,2},{m,{0}}];
Do[dhret[i,l,m]=0.+0.I,{i,{4,5}},{l,2,lmaxret,2},{m,{0}}];
Do[dhret[i,l,m]=0.+0.I,{i,{7}},{l,2,lmaxret,2},{m,{0}}];


(* ::Section::Closed:: *)
(*Compute second derivatives of retarded field using field equations*)


Print["Computing second derivatives"];


Block[
{r=grid,f=1-(2M)/r,fp=(2M)/r^2,fpp=(4M)/r^3,
h1:=hret[1,l,m],h2:=hret[2,l,m],h3:=hret[3,l,m],h4:=hret[4,l,m],h5:=hret[5,l,m],h6:=hret[6,l,m],h7:=hret[7,l,m],h8:=hret[8,l,m],h9:=hret[9,l,m],h10:=hret[10,l,m],dt=(-I m Sqrt[M/r0^3]),
dth1:=dt hret[1,l,m],dth2:=dt hret[2,l,m],dth3:=dt hret[3,l,m],dth4:=dt hret[4,l,m],dth5:=dt hret[5,l,m],dth6:=dt hret[6,l,m],dth7:=dt hret[7,l,m],dth8:=dt hret[8,l,m],dth9:=dt hret[9,l,m],th10:=dt hret[10,l,m],
drh1:=dhret[1,l,m],drh2:=dhret[2,l,m],drh3:=dhret[3,l,m],drh4:=dhret[4,l,m],drh5:=dhret[5,l,m],drh6:=dhret[6,l,m],drh7:=dhret[7,l,m],drh8:=dhret[8,l,m],drh9:=dhret[9,l,m],drh10:=dhret[10,l,m]},
Table[ddhret[1,l,m]=Simplify[-(fp/f)drh1+1/f^2 dt^2 h1+1/f ((2M)/r^3+(l(l+1))/r^2)h1+4/f^2 (1/2 f fp drh1-1/2 fp dth2+f^2/(2r^2) (h1-f h3-If[l>=1,h5,0]-f h6))],{l,0,lmaxret},{m,l,0,-2}];
Table[ddhret[2,l,m]=Simplify[-(fp/f)drh2+1/f^2 dt^2 h2+1/f ((2M)/r^3+(l(l+1))/r^2)h2+4/f^2 (1/2 f fp drh2-1/2 fp dth1+f^2/(2r^2) (h2-If[l>=1,h4,0]))],{l,0,lmaxret},{m,l,0,-2}];
(*Table[ddhret[3,l,m]=Simplify[(-(fp/f)(f drh3+h3 fp)+1/f^2dt^2f h3+1/f((2M)/r^3+(l(l+1))/r^2)f h3+4/f^2(1/2f fp (f drh3+h3 fp)+1/(2r^2)(1-8M/r+10(M/r)^2)f h3 -f^2/(2r^2)(h1-h5-(1-4M/r)h6))-2fp drh3-h3 fpp)/f],{l,0,lmaxret},{m,l,0,-2}]; - Use Akcay, Warburton & Barack instead of Barack & Lousto *)
Table[ddhret[3,l,m]=Simplify[-(fp/f) drh3+1/f^2 dt^2 h3+1/f ((2M)/r^3+(l(l+1))/r^2)h3+4/f^2 (-f/(2r^2))(h1-If[l>=1,h5,0]-(1-4M/r)(h3+h6))],{l,0,lmaxret},{m,l,0,-2}];Table[ddhret[4,l,m]=Simplify[-(fp/f)drh4+1/f^2 dt^2 h4+1/f ((2M)/r^3+(l(l+1))/r^2)h4+4/f^2 (1/4 f fp drh4-1/4 fp dth5-3/4 fp f/r h4-1/2 l(l+1)f/r^2h2)],{l,1,lmaxret},{m,l,0,-2}];
Table[ddhret[5,l,m]=Simplify[-(fp/f)drh5+1/f^2 dt^2 h5+1/f ((2M)/r^3+(l(l+1))/r^2)h5+4/f^2 (1/4 f fp drh5-1/4 fp dth4+f/r^2 (1-7/2M/r)h5-f/(2r^2) l(l+1)(h1-f h3-f h6)-f^2/(2r^2) If[l>=2,h7,0])],{l,1,lmaxret},{m,l,0,-2}];
Table[ddhret[6,l,m]=Simplify[-(fp/f)drh6+1/f^2 dt^2 h6+1/f ((2M)/r^3+(l(l+1))/r^2)h6+4/f^2 (-f/(2r^2))(h1-If[l>=1,h5,0]-(1-4M/r)(h3+h6))],{l,0,lmaxret},{m,l,0,-2}];
Table[ddhret[7,l,m]=Simplify[-(fp/f)drh7+1/f^2 dt^2 h7+1/f ((2M)/r^3+(l(l+1))/r^2)h7+4/f^2 (-f/(2r^2))(h7+(l-1)(l+2)h5)],{l,2,lmaxret},{m,l,0,-2}];
Table[ddhret[8,l,m]=Simplify[-(fp/f)drh8+1/f^2 dt^2 h8+1/f ((2M)/r^3+(l(l+1))/r^2)h8+4/f^2 (1/4 f fp drh8-1/4 fp dth9-3/4 fp f/r h8)],{l,1,lmaxret},{m,l-1,0,-2}];
Table[ddhret[9,l,m]=Simplify[-(fp/f)drh9+1/f^2 dt^2 h9+1/f ((2M)/r^3+(l(l+1))/r^2)h9+4/f^2 (1/4 fp(f drh9-dth8)+f/r^2 ((1-7/2M/r)h9-f/2If[l>=2,h10,0]))],{l,1,lmaxret},{m,l-1,0,-2}];
Table[ddhret[10,l,m]=Simplify[-(fp/f)drh10+1/f^2 dt^2 h10+1/f ((2M)/r^3+(l(l+1))/r^2)h10+4/f^2 (-f/(2r^2))(h10+(l-1)(l+2)h9)],{l,2,lmaxret},{m,l-1,0,-2}];
];


(* ::Section::Closed:: *)
(*Read even static modes*)


Print["Reading even static modes"];


evenstaticdata[l_,m_]:=evenstaticdata[l,m]=Check[Join@@Import[FileNameJoin[{h1dir, "EvenStatic","h1ret","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{"Datasets",{"inhom_left","inhom_right"}}],Print[{l,m}]];


Do[
hret[1,l,0]=Complex@@@evenstaticdata[l,0][[All,{1,2}]];
dhret[1,l,0]=Complex@@@evenstaticdata[l,0][[All,{1,2}+2]];
ddhret[1,l,0]=Complex@@@evenstaticdata[l,0][[All,{1,2}+4]];
hret[3,l,0]=Complex@@@evenstaticdata[l,0][[All,{7,8}]];
dhret[3,l,0]=Complex@@@evenstaticdata[l,0][[All,{7,8}+2]];
ddhret[3,l,0]=Complex@@@evenstaticdata[l,0][[All,{7,8}+4]];
hret[5,l,0]=Complex@@@evenstaticdata[l,0][[All,{13,14}]];
dhret[5,l,0]=Complex@@@evenstaticdata[l,0][[All,{13,14}+2]];
ddhret[5,l,0]=Complex@@@evenstaticdata[l,0][[All,{13,14}+4]];
hret[6,l,0]=Complex@@@evenstaticdata[l,0][[All,{19,20}]];
dhret[6,l,0]=Complex@@@evenstaticdata[l,0][[All,{19,20}+2]];
ddhret[6,l,0]=Complex@@@evenstaticdata[l,0][[All,{19,20}+4]];
hret[7,l,0]=Complex@@@evenstaticdata[l,0][[All,{25,26}]];
dhret[7,l,0]=Complex@@@evenstaticdata[l,0][[All,{25,26}+2]];
ddhret[7,l,0]=Complex@@@evenstaticdata[l,0][[All,{25,26}+4]];
hret[2,l,0]=Complex@@@evenstaticdata[l,0][[All,{31,32}]];
dhret[2,l,0]=Complex@@@evenstaticdata[l,0][[All,{31,32}+2]];
ddhret[2,l,0]=Complex@@@evenstaticdata[l,0][[All,{31,32}+4]];
hret[4,l,0]=Complex@@@evenstaticdata[l,0][[All,{37,38}]];
dhret[4,l,0]=Complex@@@evenstaticdata[l,0][[All,{37,38}+2]];
ddhret[4,l,0]=Complex@@@evenstaticdata[l,0][[All,{37,38}+4]];,{l,2,lmaxret,2}]


(* ::Section::Closed:: *)
(*Export data*)


Print["Exporting data"];


gridout=Join[grid[[;;sizel-1]],grid[[sizel+1;;]]];


(* ::Subsection::Closed:: *)
(*h^(1ret)*)


CreateDirectory[FileNameJoin[{h1dir,"h1ret"}]];


hretLeft[i_,l_,m_]:=hret[i,l,m][[;;sizel]](*/._[___,Left,___]\[Rule]0*);
hretRight[i_,l_,m_]:=hret[i,l,m][[sizel+1;;]](*/._[___,Right,___]\[Rule]0*);
dhretLeft[i_,l_,m_]:=dhret[i,l,m][[;;sizel]](*/._[___,Left,___]\[Rule]0*);
dhretRight[i_,l_,m_]:=dhret[i,l,m][[sizel+1;;]](*/._[___,Right,___]\[Rule]0*);
ddhretLeft[i_,l_,m_]:=ddhret[i,l,m][[;;sizel]](*/._[___,Left,___]\[Rule]0*);
ddhretRight[i_,l_,m_]:=ddhret[i,l,m][[sizel+1;;]](*/._[___,Right,___]\[Rule]0*);


Table[
Export[FileNameJoin[{h1dir,"h1ret","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hretLeft[1,l,m]],Im[hretLeft[1,l,m]],Re[dhretLeft[1,l,m]],Im[dhretLeft[1,l,m]],Re[ddhretLeft[1,l,m]],Im[ddhretLeft[1,l,m]],
Re[hretLeft[3,l,m]],Im[hretLeft[3,l,m]],Re[dhretLeft[3,l,m]],Im[dhretLeft[3,l,m]],Re[ddhretLeft[3,l,m]],Im[ddhretLeft[3,l,m]],
Re[hretLeft[5,l,m]],Im[hretLeft[5,l,m]],Re[dhretLeft[5,l,m]],Im[dhretLeft[5,l,m]],Re[ddhretLeft[5,l,m]],Im[ddhretLeft[5,l,m]],
Re[hretLeft[6,l,m]],Im[hretLeft[6,l,m]],Re[dhretLeft[6,l,m]],Im[dhretLeft[6,l,m]],Re[ddhretLeft[6,l,m]],Im[ddhretLeft[6,l,m]],
Re[hretLeft[7,l,m]],Im[hretLeft[7,l,m]],Re[dhretLeft[7,l,m]],Im[dhretLeft[7,l,m]],Re[ddhretLeft[7,l,m]],Im[ddhretLeft[7,l,m]],
Re[hretLeft[2,l,m]],Im[hretLeft[2,l,m]],Re[dhretLeft[2,l,m]],Im[dhretLeft[2,l,m]],Re[ddhretLeft[2,l,m]],Im[ddhretLeft[2,l,m]],
Re[hretLeft[4,l,m]],Im[hretLeft[4,l,m]],Re[dhretLeft[4,l,m]],Im[dhretLeft[4,l,m]],Re[ddhretLeft[4,l,m]],Im[ddhretLeft[4,l,m]]}],Transpose[{
Re[hretRight[1,l,m]],Im[hretRight[1,l,m]],Re[dhretRight[1,l,m]],Im[dhretRight[1,l,m]],Re[ddhretRight[1,l,m]],Im[ddhretRight[1,l,m]],
Re[hretRight[3,l,m]],Im[hretRight[3,l,m]],Re[dhretRight[3,l,m]],Im[dhretRight[3,l,m]],Re[ddhretRight[3,l,m]],Im[ddhretRight[3,l,m]],
Re[hretRight[5,l,m]],Im[hretRight[5,l,m]],Re[dhretRight[5,l,m]],Im[dhretRight[5,l,m]],Re[ddhretRight[5,l,m]],Im[ddhretRight[5,l,m]],
Re[hretRight[6,l,m]],Im[hretRight[6,l,m]],Re[dhretRight[6,l,m]],Im[dhretRight[6,l,m]],Re[ddhretRight[6,l,m]],Im[ddhretRight[6,l,m]],
Re[hretRight[7,l,m]],Im[hretRight[7,l,m]],Re[dhretRight[7,l,m]],Im[dhretRight[7,l,m]],Re[ddhretRight[7,l,m]],Im[ddhretRight[7,l,m]],
Re[hretRight[2,l,m]],Im[hretRight[2,l,m]],Re[dhretRight[2,l,m]],Im[dhretRight[2,l,m]],Re[ddhretRight[2,l,m]],Im[ddhretRight[2,l,m]],
Re[hretRight[4,l,m]],Im[hretRight[4,l,m]],Re[dhretRight[4,l,m]],Im[dhretRight[4,l,m]],Re[ddhretRight[4,l,m]],Im[ddhretRight[4,l,m]]}],gridout},
{"Datasets",{"inhom_left","inhom_right","grid"}}];,{l,2,lmaxret},{m,l,0,-2}];


Table[
Export[FileNameJoin[{h1dir,"h1ret","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hretLeft[9,l,m]],Im[hretLeft[9,l,m]],Re[dhretLeft[9,l,m]],Im[dhretLeft[9,l,m]],Re[ddhretLeft[9,l,m]],Im[ddhretLeft[9,l,m]],
Re[hretLeft[10,l,m]],Im[hretLeft[10,l,m]],Re[dhretLeft[10,l,m]],Im[dhretLeft[10,l,m]],Re[ddhretLeft[10,l,m]],Im[ddhretLeft[10,l,m]],
Re[hretLeft[8,l,m]],Im[hretLeft[8,l,m]],Re[dhretLeft[8,l,m]],Im[dhretLeft[8,l,m]],Re[ddhretLeft[8,l,m]],Im[ddhretLeft[8,l,m]]}],Transpose[{
Re[hretRight[9,l,m]],Im[hretRight[9,l,m]],Re[dhretRight[9,l,m]],Im[dhretRight[9,l,m]],Re[ddhretRight[9,l,m]],Im[ddhretRight[9,l,m]],
Re[hretRight[10,l,m]],Im[hretRight[10,l,m]],Re[dhretRight[10,l,m]],Im[dhretRight[10,l,m]],Re[ddhretRight[10,l,m]],Im[ddhretRight[10,l,m]],
Re[hretRight[8,l,m]],Im[hretRight[8,l,m]],Re[dhretRight[8,l,m]],Im[dhretRight[8,l,m]],Re[ddhretRight[8,l,m]],Im[ddhretRight[8,l,m]]}],gridout},
{"Datasets",{"inhom_left","inhom_right","grid"}}];,{l,2,lmaxret},{m,l-1,0,-2}];


(* ::Text:: *)
(*l=1*)


Table[
Export[FileNameJoin[{h1dir,"h1ret","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hretLeft[1,l,m]],Im[hretLeft[1,l,m]],Re[dhretLeft[1,l,m]],Im[dhretLeft[1,l,m]],Re[ddhretLeft[1,l,m]],Im[ddhretLeft[1,l,m]],
Re[hretLeft[3,l,m]],Im[hretLeft[3,l,m]],Re[dhretLeft[3,l,m]],Im[dhretLeft[3,l,m]],Re[ddhretLeft[3,l,m]],Im[ddhretLeft[3,l,m]],
Re[hretLeft[5,l,m]],Im[hretLeft[5,l,m]],Re[dhretLeft[5,l,m]],Im[dhretLeft[5,l,m]],Re[ddhretLeft[5,l,m]],Im[ddhretLeft[5,l,m]],
Re[hretLeft[6,l,m]],Im[hretLeft[6,l,m]],Re[dhretLeft[6,l,m]],Im[dhretLeft[6,l,m]],Re[ddhretLeft[6,l,m]],Im[ddhretLeft[6,l,m]],
Re[hretLeft[2,l,m]],Im[hretLeft[2,l,m]],Re[dhretLeft[2,l,m]],Im[dhretLeft[2,l,m]],Re[ddhretLeft[2,l,m]],Im[ddhretLeft[2,l,m]],
Re[hretLeft[4,l,m]],Im[hretLeft[4,l,m]],Re[dhretLeft[4,l,m]],Im[dhretLeft[4,l,m]],Re[ddhretLeft[4,l,m]],Im[ddhretLeft[4,l,m]]}],Transpose[{
Re[hretRight[1,l,m]],Im[hretRight[1,l,m]],Re[dhretRight[1,l,m]],Im[dhretRight[1,l,m]],Re[ddhretRight[1,l,m]],Im[ddhretRight[1,l,m]],
Re[hretRight[3,l,m]],Im[hretRight[3,l,m]],Re[dhretRight[3,l,m]],Im[dhretRight[3,l,m]],Re[ddhretRight[3,l,m]],Im[ddhretRight[3,l,m]],
Re[hretRight[5,l,m]],Im[hretRight[5,l,m]],Re[dhretRight[5,l,m]],Im[dhretRight[5,l,m]],Re[ddhretRight[5,l,m]],Im[ddhretRight[5,l,m]],
Re[hretRight[6,l,m]],Im[hretRight[6,l,m]],Re[dhretRight[6,l,m]],Im[dhretRight[6,l,m]],Re[ddhretRight[6,l,m]],Im[ddhretRight[6,l,m]],
Re[hretRight[2,l,m]],Im[hretRight[2,l,m]],Re[dhretRight[2,l,m]],Im[dhretRight[2,l,m]],Re[ddhretRight[2,l,m]],Im[ddhretRight[2,l,m]],
Re[hretRight[4,l,m]],Im[hretRight[4,l,m]],Re[dhretRight[4,l,m]],Im[dhretRight[4,l,m]],Re[ddhretRight[4,l,m]],Im[ddhretRight[4,l,m]]}],gridout},
{"Datasets",{"inhom_left","inhom_right","grid"}}];,{l,{1}},{m,l,0,-2}];


Table[
Export[FileNameJoin[{h1dir,"h1ret","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hretLeft[9,l,m]],Im[hretLeft[9,l,m]],Re[dhretLeft[9,l,m]],Im[dhretLeft[9,l,m]],Re[ddhretLeft[9,l,m]],Im[ddhretLeft[9,l,m]],
Re[hretLeft[8,l,m]],Im[hretLeft[8,l,m]],Re[dhretLeft[8,l,m]],Im[dhretLeft[8,l,m]],Re[ddhretLeft[8,l,m]],Im[ddhretLeft[8,l,m]]}],Transpose[{
Re[hretRight[9,l,m]],Im[hretRight[9,l,m]],Re[dhretRight[9,l,m]],Im[dhretRight[9,l,m]],Re[ddhretRight[9,l,m]],Im[ddhretRight[9,l,m]],
Re[hretRight[8,l,m]],Im[hretRight[8,l,m]],Re[dhretRight[8,l,m]],Im[dhretRight[8,l,m]],Re[ddhretRight[8,l,m]],Im[ddhretRight[8,l,m]]}],gridout},
{"Datasets",{"inhom_left","inhom_right","grid"}}];,{l,{1}},{m,l-1,0,-2}];


(* ::Text:: *)
(*l=0*)


Table[
Export[FileNameJoin[{h1dir,"h1ret","h1-l"<>ToString[l]<>"m"<>ToString[m]<>".h5"}],{Transpose[{
Re[hretLeft[1,l,m]],Im[hretLeft[1,l,m]],Re[dhretLeft[1,l,m]],Im[dhretLeft[1,l,m]],Re[ddhretLeft[1,l,m]],Im[ddhretLeft[1,l,m]],
Re[hretLeft[3,l,m]],Im[hretLeft[3,l,m]],Re[dhretLeft[3,l,m]],Im[dhretLeft[3,l,m]],Re[ddhretLeft[3,l,m]],Im[ddhretLeft[3,l,m]],
Re[hretLeft[6,l,m]],Im[hretLeft[6,l,m]],Re[dhretLeft[6,l,m]],Im[dhretLeft[6,l,m]],Re[ddhretLeft[6,l,m]],Im[ddhretLeft[6,l,m]],
Re[hretLeft[2,l,m]],Im[hretLeft[2,l,m]],Re[dhretLeft[2,l,m]],Im[dhretLeft[2,l,m]],Re[ddhretLeft[2,l,m]],Im[ddhretLeft[2,l,m]]}],Transpose[{
Re[hretRight[1,l,m]],Im[hretRight[1,l,m]],Re[dhretRight[1,l,m]],Im[dhretRight[1,l,m]],Re[ddhretRight[1,l,m]],Im[ddhretRight[1,l,m]],
Re[hretRight[3,l,m]],Im[hretRight[3,l,m]],Re[dhretRight[3,l,m]],Im[dhretRight[3,l,m]],Re[ddhretRight[3,l,m]],Im[ddhretRight[3,l,m]],
Re[hretRight[6,l,m]],Im[hretRight[6,l,m]],Re[dhretRight[6,l,m]],Im[dhretRight[6,l,m]],Re[ddhretRight[6,l,m]],Im[ddhretRight[6,l,m]],
Re[hretRight[2,l,m]],Im[hretRight[2,l,m]],Re[dhretRight[2,l,m]],Im[dhretRight[2,l,m]],Re[ddhretRight[2,l,m]],Im[ddhretRight[2,l,m]]}],gridout},
{"Datasets",{"inhom_left","inhom_right","grid"}}];
,{l,{0}},{m,l,0,-2}];
