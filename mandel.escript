#!/usr/bin/env escript
%%! -smp enable -sname mandelbrot

-define(WIDTH, 500).
-define(HEIGHT, 500). 
-define(XSTART, -2.0).
-define(XEND, 1.0).
-define(YSTART, -1.0).
-define(YEND, 1.0).
-define(ITER, 5000).

main([]) -> 
    io:format("Rendering mandelbrot to ~p * ~p\n", [?WIDTH, ?HEIGHT]),
    {ok, File} = file:open('mandel.bmp', [write]),

    Pixels = lines(),
    Size = (?WIDTH * ?HEIGHT * 3) + 26,
    Bmp = << "BM", Size:64/little, 26:32/little, 12:32/little, ?WIDTH:16/little, ?HEIGHT:16/little, 1:16/little, 24:16/little>>,
    file:write(File,Bmp),
    lists:foreach(
        fun(Line) -> 
            lists:foreach(
                fun({R,G,B}) ->
                    file:write(File,<<R:8,G:8,B:8>>)
                end, Line)
        end, Pixels),
    file:close(File). 
    


lines() ->
    Ystep = (?YEND - ?YSTART) / ?HEIGHT,
    [line(?YSTART + Y*Ystep) || Y <- lists:seq(0, ?HEIGHT-1)].

line(Y) ->
    io:format("\r ~p", [Y]),
    Xstep = (?XEND - ?XSTART) / ?WIDTH,
    [inset(?XSTART + X*Xstep,Y) || X <- lists:seq(0, ?WIDTH-1)].

inset(Cr, Ci) ->
    inset(0, 0, Cr, Ci, 0).

inset(Zr, Zi, Cr, Ci, Iter) ->
    Zr2 = Zr*Zr,
    Zi2 = Zi*Zi,

    case (Zr2+Zi2) > 4 of
        true -> {255,255,255};
        false when Iter < ?ITER -> 
            Zrn = Zr2 - Zi2 + Cr,
            Zin = (2 * Zr * Zi) + Ci,
            inset(Zrn, Zin, Cr, Ci, Iter+1);
        _ -> {0,0,0}
   end.
