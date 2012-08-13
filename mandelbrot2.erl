-module(mandelbrot2).

-export([render/0,render/7,renderLine/1]).

-record(state, {width,height,rstart,rend,istart,iend,x,y,rs,is,c,iter,file,itp,pid}).

-define(MAXCON, 8).

render() ->
    render(
        _Width=4000,
        _Height=3000,
        _Rstart=-2,
        _Rend=1,
        _Istart=-1,
        _Iend=1,
        _Iter=255).

render(Width,Height,Rstart,Rend,Istart,Iend,Iter) ->
    Rs=(Rend-Rstart)/Width,
    Is=(Iend-Istart)/Height,
    
    File=openBmp(Width,Height),
    State = #state{
        width=Width,
        height=Height,
        rstart=Rstart,
        rend=Rend,
        istart=Istart,
        iend=Iend,
        iter=Iter,
        x=0,
        y=0,
        rs=Rs,
        is=Is,
        c={Rstart,Istart},
        itp=[{X,Y} || X <- [-Rs/2,0,Rs/2], Y<-[-Is/2,0,Is/2]],
        file=File,
        pid=self()
    },
    ok = render(State,0),
    file:close(File),
    ok.

render(State = #state{is=Is, c={_Cr,Ci}, y=Y, file=File},Con) when Y < State#state.height ->
   io:format("~p ", [Y]),
   spawn(mandelbrot2, renderLine, [State]),
   Ccon = case Con+1 of 
     Ncon when Ncon >= ?MAXCON -> receiveAndWrite(Ncon, File, []), 0;
     Ncon -> Ncon
   end,
   render(State#state{c={0,Ci+Is},y=Y+1},Ccon);

render(State,Con) ->
    receiveAndWrite(Con, State#state.file, []),
    ok.

receiveAndWrite(Lines, File, Buf) when Lines > 0 ->
    receive
        {Y, Data} -> io:format("r~p ", [Y]), receiveAndWrite(Lines-1, File, [{Y,Data}|Buf]); 
        terminate -> io:format("!!!!!!!!!", []), ok
    end;
receiveAndWrite(Lines, File, Buf) ->
    lists:foreach(
        fun({_Y, Data}) -> file:write(File, Data) end,
        lists:keysort(1,Buf)
    ),
    io:format("\r", []).
 


b(State) ->
    io:format("\r~p", [State#state.y]).

%% ================================================================================================

renderLine(State = #state{rstart=Rstart, y=Y, c={_Cr,Ci}, pid=Parent}) ->
    Line = renderPixel(State#state{x=0, c={Rstart,Ci}}, <<>>),
    Parent ! {Y, Line},
    ok. 

renderPixel(State = #state{x=X, rs=Rs, c={Cr,Ci}, itp=Itp, iter=Iter}, Buf) when X < State#state.width ->
    P = case erlang:round(lists:sum([inset({0,0}, {Cr+U, Ci+V}, Iter) || {U,V} <- Itp])/length(Itp)) of
        0 -> <<0:24>>;
        K -> <<((K bsr 1)+127):8,K:8,K:8>> 
    end,
    renderPixel(State#state{x=X+1, c={Cr+Rs,Ci}}, <<Buf/binary, P/binary>>);

renderPixel(_State, Buf) ->
    Buf.

%% ================================================================================================

openBmp(W,H) ->
    {ok, File} = file:open('mandel2.bmp', [write,raw]),
    Size = (W * H * 3) + 26,
    Bmp = << "BM", Size:64/little, 26:32/little, 12:32/little, W:16/little, H:16/little, 
                1:16/little, 24:16/little>>,
    file:write(File,Bmp),
    File.

inset({Zr,Zi},{Cr,Ci},I) ->
    Zr2 = Zr*Zr,
    Zi2 = Zi*Zi,

    case (Zr2+Zi2) > 36 of
       true -> I;
       false when I > 0 -> 
            Zrn = Zr2 - Zi2 + Cr,
            Zin = (2 * Zr * Zi) + Ci,
            inset({Zrn, Zin}, {Cr, Ci}, I-1);
        _ -> 0
   end. 
