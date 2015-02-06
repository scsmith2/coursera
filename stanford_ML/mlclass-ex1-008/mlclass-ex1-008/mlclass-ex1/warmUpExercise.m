function A = warmUpExercise()
%WARMUPEXERCISE Example function in octave
%   A = WARMUPEXERCISE() is an example function that returns the 5x5 identity matrix

A = zeros(5);
for i = 1:5
	A(i,i) = 1;
end

end
