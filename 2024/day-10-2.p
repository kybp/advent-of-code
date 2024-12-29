program Day10Part2;

const
   width   = 57;
   height  = 57;
   newline = #10;

type
   cells = array [1..width, 1..height] of integer;

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


function score(x, y, prev : integer): integer;

var
   cell : integer;

begin
   if (x < 1) or (y < 1) or (x > width) or (y > height) then
      score := 0
   else
   begin
      cell := grid[x, y];

      if cell <> prev + 1 then
         score := 0
      else if cell = 9 then
         score := 1
      else
         score := (
            score(x + 1, y, cell) +
            score(x - 1, y, cell) +
            score(x, y + 1, cell) +
            score(x, y - 1, cell)
         );
   end;
end;


begin
   read_grid;

   total_score := 0;

   for root_x := 1 to width do
      for root_y := 1 to height do
         if grid[root_x, root_y] = 0 then
            inc(total_score, score(root_x, root_y, -1));

   write(total_score);
   writeln;
end.
