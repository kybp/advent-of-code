program Day10Part1;

const
   width   = 57;
   height  = 57;
   newline = #10;

type
   cells    = array [1..width, 1..height] of integer;
   cell_set = array [1..width, 1..height] of boolean;

var
   grid           : cells;
   root_x, root_y : integer;
   total_score    : integer;


procedure read_grid;

var
   input : text;
   c     : char;
   x, y  : integer;

begin
   assign(input, '2024/input/day-10.txt');
   reset(input);

   for y := 1 to height do
   begin
      for x := 1 to width do
      begin
         read(input, c);
         grid[x, y] := ord(c) - ord('0');
      end;

      read(input, c); (* discard newline *)
   end;

   close(input);
end;


function empty_cell_set(): cell_set;

var
   x, y : integer;

begin
   for x := 1 to width do
      for y := 1 to height do
         empty_cell_set[x, y] := false;
end;


function set_length(s : cell_set): integer;

var
   x, y : integer;

begin
   set_length := 0;

   for x := 1 to width do
      for y := 1 to height do
         if s[x, y] then
            inc(set_length);
end;


function next_steps(sources : cell_set): cell_set;

var
   x, y, cell : integer;

begin
   next_steps := empty_cell_set();

   for x := 1 to width do
      for y := 1 to height do
      begin
         if not sources[x, y] then
            continue;

         cell := grid[x, y];

         if (x + 1 <= width) and (grid[x + 1, y] = cell + 1) then
            next_steps[x + 1, y] := true;
         if (x - 1 > 0) and (grid[x - 1, y] = cell + 1) then
            next_steps[x - 1, y] := true;
         if (y + 1 <= height) and (grid[x, y + 1] = cell + 1) then
            next_steps[x, y + 1] := true;
         if (y - 1 > 0) and (grid[x, y - 1] = cell + 1) then
            next_steps[x, y - 1] := true;
      end;
end;


function score(x, y : integer): integer;

var
   next_cell : integer;
   steps     : cell_set;

begin
   steps := empty_cell_set();
   steps[x, y] := true;

   for next_cell := 1 to 9 do
      begin
         steps := next_steps(steps);
         if set_length(steps) = 0 then
            break;
      end;

   score := set_length(steps);
end;


begin
   read_grid;

   total_score := 0;

   for root_x := 1 to width do
      for root_y := 1 to height do
         if grid[root_x, root_y] = 0 then
            inc(total_score, score(root_x, root_y));

   write(total_score);
   writeln;
end.
