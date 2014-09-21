
use "hw7.sml";

val e0 = eval_prog(Shift(1.0,1.0,Intersect(Line(1.0,1.0),Line(1.0,0.0))),[]);
val e1 = preprocess_prog(LineSegment(1.00000999,1.0,1.0,2.0));
val e2 = preprocess_prog( Shift(5.5,~1.2,LineSegment(~3.7,1.5,~3.7,1.5)) );
val e3 = preprocess_prog( Shift(1.0,1.0,LineSegment(1.0,1.0,0.0,0.0)) );





