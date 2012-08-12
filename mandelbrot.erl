-module('mandelbrot').

-export([render/0]).

-define(WIDTH, 4000).
-define(HEIGHT, 3000). 
-define(XSTART, -1.7).
-define(XEND, 0.7).
-define(YSTART, -1.0).
-define(YEND, 1.0).
-define(ITER, 255).
-define(ITER2, 65535).

-record(state, {xrange, yrange, xstep, ystep, x, y, cr, ci, file, itp}).

render() ->
    io:format("Rendering mandelbrot to ~p * ~p\n", [?WIDTH, ?HEIGHT]),
    File = openBmp(),
    render2(File),
    closeBmp(File).

%% =========================================================
openBmp() ->
    {ok, File} = file:open('mandel.bmp', [write,raw]),
    Size = (?WIDTH * ?HEIGHT * 3) + 26,
    Bmp = << "BM", Size:64/little, 26:32/little, 12:32/little, ?WIDTH:16/little, ?HEIGHT:16/little, 1:16/little, 24:16/little>>,
    file:write(File,Bmp),
    File.

closeBmp(File) ->
    file:close(File).



render2(File) ->
    Yrange = ?YEND - ?YSTART,
    Xrange = ?XEND - ?XSTART,
    Xstep = Xrange / ?WIDTH,
    Ystep = Yrange / ?HEIGHT,
     
    State = #state{    
        xrange = Yrange,
        yrange = Xrange,
        xstep = Xstep,
        ystep = Ystep,
        y=0,
        ci = ?YSTART,
        file = File,
        itp = [{X,Y} || X <- [-Xstep/2,0,-Xstep/2], Y <- [-Ystep/2,0,Ystep/2]]
    },


    render2lines(State).

render2lines(State = #state{y=Y,ystep=Ystep,ci=Ci} ) when State#state.y < ?HEIGHT ->
    io:format("\r~p", [Y]),
    render2pixels(State#state{x=0, cr=?XSTART}, <<>>),
    render2lines(State#state{y=Y+1, ci=Ci+Ystep});

render2lines(_State = #state{} ) ->
    ok.

render2pixels(State = #state{x=X, xstep=Xstep, cr=Cr, ci=Ci, itp=Itp}, Buf) when State#state.x < ?WIDTH ->
    R = erlang:round(lists:sum([inset(Cr+U, Ci+V) || {U,V} <- Itp])/9),
    Col = case R of
        0 -> <<0:24>>;
        K -> <<((K bsr 1)+127):8,K:8,K:8>> 
    end,

    render2pixels(State#state{x=X+1,cr=Cr+Xstep}, <<Buf/binary, Col/binary>>);
    
render2pixels(#state{file=File}, Buf ) ->
    file:write(File, Buf),
    ok.

inset(Cr, Ci) ->
    inset(0, 0, Cr, Ci, 0).

inset(Zr, Zi, Cr, Ci, Iter) ->
    Zr2 = Zr*Zr,
    Zi2 = Zi*Zi,

    case (Zr2+Zi2) > 36 of
       true -> Iter;
       false when Iter < ?ITER -> 
            Zrn = Zr2 - Zi2 + Cr,
            Zin = (2 * Zr * Zi) + Ci,
            inset(Zrn, Zin, Cr, Ci, Iter+1);
        _ -> 0
   end.
