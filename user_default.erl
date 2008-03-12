%% @copyright 2008 partdavid at gmail.com
%% @author partdavid at gmail.com
%% @doc Erlang Command Shell
%%
%% The Erlang Command Shell is a module intended to be installed as
%% `user_default'. Doing so will enhance the commands available in
%% the shell, replacing some Unix-style command shell functionality.
%% The intention is not so much to replace a Unix shell in all circumstances
%% but to provide benefits where basic shell tasks can be carried out from
%% within the Erlang shell.
%%
%% Some functions are inspired by Perl, as well, as, for example, the
%% Perl `chomp' and `split' functions are superior to or have no equivalent
%% in shell languages.
%%
%% TODO:
%% <ul>
%%    <li>find</li>
%%    <li>grep options</li>
%%    <li>tar, gzip, gunzip</li>
%%    <li>rmdir</li>
%%    <li>ln</li>
%%    <li>umask</li>
%%    <li>head, tail</li>
%%    <li>test (file tests)</li>
%%    <li>survey short shell scripts to find holes</li>
%%    <li>cp, mv accept funs (copy, move to fun "destination")</li>
%%    <li>which</li>
%%    <li>ssh client</li> (Can use pty? Is werl terminal-like?)
%%    <li>command invocation</li>
%%    <li>ls -R, grep -R</li>
%%    <li>alias?</li>
%%    <li>global options or preferences</li>
%% </ul>
%%
%% This program is free software: you can redistribute it and/or modify it
%% under the terms of the GNU Lesser General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or (at your
%% option) any later version.
%%
%% This program is distributed in the hope that it will be useful, but WITHOUT
%% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
%% FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
%% License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public License
%% along with this program (see the COPYING file for details).
%% If not, see [http://www.gnu.org/licenses/].
%%
-module(user_default).
-description("Unix shell-like commands for erlang shell").

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([mv/2, mv/3, cp/2, cp/3, mkdir/1, mkdir/2,
         rm/1, rm/2, cat/1, cat/2,
         chomp/1, chomp/2, tee/2, tee/3, printf/2, printf/3,
         split/2, split/3, chop/1, join/2,
         clock/1,
         wc/1, wc/2, wcf/1, wcf/2,
         grep/2, grep/3, grepf/2, grepf/3,
         ls/0, ls/1, ls/2, lsl/0, lsl/1, lsl/2,
         find/1, find/2,
         system/1, system/2, 
         start_job_server/0, stop_job_server/0,
         output/1, jobs/0, kill/1, bg/1,
         r/0, glob/1, stat/1, stat/2]).

-define(fi2kl(FI),
        lists:zip(record_info(fields, file_info), tl(tuple_to_list(FI)))).

-define(ftype_abbr(T),
        begin
           D = [{d, directory},
                {dir, directory},
                {f, regular},
                {file, regular}] ++ [ {X, X} || X <- [directory, regular] ],
           case lists:keysearch(T, 1, D) of
              {value, {T, V}} -> V;
              false -> false
           end
        end).

%% @type filespec() = globpattern() + filename() + [filename()].
%% @type globpattern() = string() + atom().
%% @type filename() = string() + atom().
%% @type options() = [option()].
%% @type option() = atom() + {atom(), term()}.
%% @type fileinfo() = #file_info{} + [{atom(), term()}].
%% @type results() = [{string(), ok + {error, term()}}].
%% @type command() = string().
%% @type joberror() = {job_server_timeout, term()}.

%% @spec stat(F::filename()) -> fileinfo()
%% @doc Gets the file_info structure (see {@link file:read_file_info/1}) for
%% the file.
stat(F) ->
   stat(F, []).

%% @spec stat(filename(), options()) -> fileinfo()
%% @doc Gets the file_info structure (see {@link file:read_file_info/1}) for
%% the file. If the `filist' option is present, returns the value as a
%% keylist rather than a `file_info' record.
stat(F, Opt) ->
   {ok, S} = file:read_file_info(F),
   norm_fi(S, Opt).

norm_fi(S, Opt) ->
   case opt(filist, Opt) of
      true ->
         ?fi2kl(S);
      _ ->
         S
   end.

%% @spec r() -> {ok, user_default} + error
%% @doc Recompiles and reloads the user_default module. Convenient when
%% changing. Think `alias r=. ~/.bashrc'.
r() ->
   c:c(user_default).

%% @spec chomp(string()) -> string()
%% @doc Chomps the line ending from the string. Note that this is hardcoded
%% to Unix line-ending convention.
%% @todo Implement varying line-endings--however, it's not unusual to use a Unix
%% convention even on Windows when editing Erlang, this might have to wait
%% for global options.
chomp(X) ->
   chomp(X, []).

chomp(X, Opt) ->
   chomp(X, [], Opt).

chomp([], Acc, _Opt) ->
   lists:reverse(Acc);
chomp([L|Rest], Acc, Opt) when is_list(L) ->
   chomp(Rest, [chomp(L)|Acc], Opt);
chomp(L, _, _Opt) ->
   %% Unix line-ending, should probably in opts? prefs?
   string:strip(L, right, $\n).

%% @spec cat(filespec()) -> [string()]
%% @doc The cat function returns a list of lines in the specified file(s).
%% The line endings are stripped with the {@link chomp/1} function.
%% If this is not desired, use {@link chomp/2}.
%%
%% The files may be specified as an atom, a list of atoms, a string or
%% list of strings. Any wildcard globbing patterns in any of the specifications
%% are expanded (see {@link filelib:wildcard/1}).
cat(Files) ->
   cat(Files, []).

%% @spec cat(filespec(), options()) -> [string()]
%% @doc As {@link cat/1}, but you can specify the `nochomp' option to avoid
%% the line-endings being chomped.
cat(Files, Opts) ->
   catr(glob(Files), [], Opts).

catr([], Acc, Opt) ->
   case opt(nochomp, Opt) of
      true -> lists:reverse(Acc);
      _ -> chomp(lists:reverse(Acc))
   end;
catr([F|Files], Acc, Opt) ->
   case file:open(F, [read]) of
      {ok, IO} ->
         file:open(F, [read]),
         Lines = read_lines(IO, Acc, Opt),
         file:close(IO),
         catr(Files, Lines, Opt);
      {error, eisdir} ->
         catr(Files, Acc, Opt)
   end.

%% @spec printf(Format::string(), [term()]) -> string()
%% @doc Returns a formatted list (using the same formatting specification
%% as {@link io:format/2}).
printf(Format, Exprs) ->
   printf(Format, Exprs, []).

printf(Format, Exprs, Opt) when not is_list(Exprs) ->
   printf(Format, [Exprs], Opt);
printf(Format, Exprs, Opt) ->
   case is_string(Exprs) of
      true -> printf(Format, [Exprs], Opt);
      _ -> lists:flatten(io_lib:format(Format, Exprs))
   end.

%% @spec grep(Pattern::string(), Lines) -> Lines
%%    Lines = [string()] + bool()
%% @doc Matches the regular expression pattern against the list of strings
%% given, returning a list of matching lines (see {@link regexp}).
grep(Pattern, Lines) ->
   grep(Pattern, Lines, []).

%% @spec grep(Pattern::string(), ScanList, options()) -> Lines + FileLines
%%    ScanList = filespec() + [string()]
%%    Lines = [string()] + [{File, Lines}] + bool() + [File]
%%    File = string()
%% @doc By default, matches the regular expression against the list of
%% strings, returning a list of matching lines (see {@link regexp}). If
%% the `files' option is given, interprets the scan list to be a list
%% of file specifications (see {@link cat/1}). Matching lines from each
%% file are returned in a list, as the second element of a tuple whose
%% first element is the filename.
%%
%% If the `quiet' option is given, merely returns `true' if any of the
%% lines match (or, when a list of files is given, a list of the names
%% of the files with any matching line). This short-circuits unnecessary
%% scanning.
%% @todo Needs many options: `-i' for one
grep(Pattern, Lines, Opt) ->
   case opt(files, Opt) of
      true ->
         grepf(Pattern, Lines, Opt);
      _ ->
         grep(Pattern, opt(quiet, Opt), Lines, Opt, [])
   end.

grep(_, true, [], _, []) ->
   false;
grep(_, true, _, _, [_]) ->
   true;
grep(_, _, [], _, A) ->
   lists:reverse(A);
grep({compiled, RX}, Q, [L|Lines], Opt, A) ->
   case regexp:first_match(L, RX) of
      nomatch ->
         grep({compiled, RX}, Q, Lines, Opt, A);
      {match, _, _} ->
         grep({compiled, RX}, Q, Lines, Opt, [L|A])
   end;
grep(Pattern, Q, Lines, Opt, A) ->
   {ok, RX} = regexp:parse(Pattern),
   grep({compiled, RX}, Q, Lines, Opt, A).

%% @spec grepf(Pattern::string(), filespec()) -> [{File, Lines}] + [File]
%%    File = string()
%%    Lines = [string()]
%% @doc As {@link grep/3}, without having to specify the `files' option.
grepf(Pattern, Files) ->
   grepf(Pattern, Files, []).

%% @spec grepf(Pattern::string(), filespec(), options()) ->
%%    [{File, Lines}] + [File]
%%    File = string()
%%    Lines = [string()]
grepf(Pattern, Files, Opt) ->
   R = [ {F, grep(Pattern, cat(F, Opt), rmopt(files, Opt))} || F <- glob(Files) ],
   case opt(quiet, Opt) of
      true ->
         [ F || {F, true} <- R ];
      _ ->
         [ E || E = {_, [_|_]} <- R ]
   end.

%% @spec wc(Lines) -> Count
%%     Lines = [string()]
%%     Count = {Linecount, Wordcount, Bytecount}
%%     Linecount = integer()
%%     Wordcount = integer()
%%     Bytecount = integer()
%% @doc Equivalent to {@link wc/2} with no options.
wc(L) ->
   wc(L, []).

%% Opt - 'files' - input is filenames, not lines
%% Opt - nototal - when input is filenames, don't print total
%% @spec wc(ScanList, options()) -> Count
%%    ScanList = [string()] + filespec()
%%    Count = SimpleCount + [{File, SimpleCount}]
%%    SimpleCount = {integer(), integer(), integer()} + {integer(), integer()}
%%                  + integer()
%% @doc Count the lines, words and bytes in the input. The input can be
%% specified as a list of lines (the default) or as a file specification
%% (see {@link cat/1}) if the `files' option is given. When scanning files,
%% `wc/2' keeps a running total of the counted attributes, and includes
%% a "total" tuple in the result. If this is not desired, use the `nototal'
%% option.
%%
%% By default, lines, words (see {@link string:words/1}) and bytes are
%% counted, and returned as three-tuples. If two of `lines', `words' or
%% `characters' are given, two-tuples are returned instead, with the
%% applicable attributes. If only one of these options is given, results
%% are returned as integers (not 1-tuples).
wc(Lines, Opt) ->
   case opt(files, Opt) of
      true ->
         wcf(Lines, Opt);
      _ ->
         {LCt, WCt, CCt} = wc(Lines, Opt, {0, 0, 0}),
         case [ opt(O, Opt) || O <- [lines, words, characters] ] of
            [true, true, false] ->
               {LCt, WCt};
            [true, false, true] ->
               {LCt, CCt};
            [false, true, true] ->
               {WCt, CCt};
            [true, false, false] ->
               LCt;
            [false, true, false] ->
               WCt;
            [false, false, true] ->
               CCt;
            _ -> {LCt, WCt, CCt}
         end
   end.

%% @spec wcf(filespec()) -> Count
%%    Count = [{File, Simplecount}]
%%    File  = string()
%%    Simplecount = {integer(), integer(), integer()}
%% @doc Shorthand for {@link wc/2} with the `files' option.
wcf(Files) ->
   wcf(Files, []).

add({A, B, C}, {X, Y, Z}) ->
   {A + X, B + Y, C + Z};
add({A, B}, {X, Y}) ->
   {A + X, B + Y};
add(T, 0) when is_tuple(T) ->
   T;
add(A, X) ->
   A + X.

%% @spec wcf(filespec(), options()) -> Count
%%    Count = [{File, Simplecount}]
%%    File  = string()
%%    Simplecount = {integer(), integer(), integer()} + {integer(), integer()}
%%       + integer()
%% @doc Shorthand for {@link wc/2} with the `files' option.
wcf(Files, Opt) ->
   {R, T} = lists:mapfoldl(
              fun(F, A) ->
                    R = wc(cat(F, Opt), rmopt(files, Opt)),
                    {{F, R}, add(R, A)}
              end, 0, glob(Files)),
   case opt(nototal, Opt) of
      true -> R;
      _ -> [{total, T}|R]
   end.
                

wc([], _, A) ->
   A;
wc([L|Lines], Opt, {LCt, WCt, CCt}) ->
   wc(Lines, Opt, {LCt + 1,
                   WCt + string:words(L),
                   CCt + length(L)}).


%% Copied from hammer.data:is_string/1
is_string(T) when not(is_list(T)) ->
   false;
is_string([]) ->
   true;
is_string([C|Rest]) ->
   case is_graphic(C) of
      true ->
         is_string(Rest);
      false ->
         false
   end.

is_graphic(C) when not(is_integer(C)) ->
   false;
is_graphic(C) when C == 9;
                   C == 10;
                   C == 13 ->
   true;
is_graphic(C) when C > 31, C < 127 ->
   true;
is_graphic(C) when C > 159, C < 255 ->
   true;
is_graphic(_C) ->
   false.

%% Convenience function for looking up options. Proplists are actually
%% slightly awkward for this.
opt(Opt, SOpt) when not is_list(SOpt) ->
   opt(Opt, [SOpt]);
opt(Opt, OptList) ->
   case proplists:lookup(Opt, OptList) of
      {Opt, Val} ->
         Val;
      none -> false
   end.

%% Set a default option--note, this sucks, because of the way proplists
%% work.
defopt(O, Opt) when not is_list(Opt) ->
   defopt(O, [Opt]);
defopt(O, Opt) ->
   case opt(O, Opt) of
      false ->
         [O|Opt];
      _ -> Opt
   end.

%% Remove the option
rmopt(O, Opt) when not is_list(Opt) ->
   rmopt(O, [Opt]);
rmopt(O, Opt) ->
   proplists:delete(O, Opt).

%% @spec lsl() -> [File]
%%    File = string()
%% @doc Equivalent to {@link ls/1} with `long' option.
lsl() ->
   ls([long]).

%% @spec lsl(OptOrFiles) -> [{File, fileinfo()}]
%%    OptOrFiles = filespec() + options()
%%    File = string()
%% @doc Equivalent to {@link ls/2} with defaults of `long' option
%% and current directory (`"."').
lsl(Opt = [O|_]) when is_tuple(O);
                      is_atom(O) ->
   ls(".", defopt(long, Opt));
lsl(Paths) ->
   ls(Paths, [long]).

%% @spec lsl(filespec(), options()) -> [{File, fileinfo()}]
%%    File = string()
%% @doc Equivalent to {@link ls/2} with `long' option.
lsl(Paths, Opt) ->
   ls(Paths, defopt(long, Opt)).

%% @spec ls() -> [File]
%%    File = string()
%% @doc Eqivalent to {@link ls/2} with no options and a file specification
%% of `"."'.
ls() ->
   ls(".", []).

%% @spec ls(OptOrFiles) -> Listing
%%    OptOrFiles = filespec() + options()
%%    Listing = [File] + [{File, fileinfo()}]
%% @doc Equivalent to {@link ls/2} with no default options and a default
%% file specification of `"."'.
ls(Opt = [O|_]) when is_tuple(O); is_atom(O) ->
   ls(".", Opt);
ls(Paths) ->
   ls(Paths, []).

%% @spec ls(filespec(), options()) -> Listing
%%    Listing = [File] + [{File, fileinfo()}]
%% @doc List the specified file, or the files in the specified directory. If
%% the ``long'' option is given, produces a "long" listing including the file
%% info (you may also specify the ``filist'' option: see {@link stat/2}).
%% @todo Needs -d, -R, -t, -r options.
ls(Path, Opt) ->
   Result = case opt(long, Opt) of
      true ->
         gather_info(shortls(glob(Path), [], Opt), [], Opt);
      _ ->
         shortls(glob(Path), [], Opt)
   end,
   case Result of
      [{Dir, Subfiles}] when is_list(Dir) -> Subfiles;
      _ -> Result
   end.

shortls([], Result, _) ->
   lists:sort(Result);
shortls([P|Paths], Acc, Opt) ->
   case filelib:is_dir(P) of
      true ->
         {ok, Files} = file:list_dir(P),
         shortls(Paths, [{P, lists:sort(Files)}|Acc], Opt);
      _ -> case filelib:is_file(P) of
              true -> shortls(Paths, [P|Acc], Opt);
              _ -> shortls(Paths, [{no_such_file_or_directory, P}|Acc], Opt)
           end
   end.

gather_info([], Acc, _Opt) ->
   lists:sort(Acc);
gather_info([N = {no_such_file_or_directory, _P}|Paths], Acc, Opt) ->
   gather_info(Paths, [N|Acc], Opt);
gather_info([{Dir, Files}|Paths], Acc, Opt) ->
   gather_info(Paths,
               [{Dir, lists:map(fun(F) ->
                                      case file:read_file_info(
                                             filename:join(Dir, F)) of
                                         {ok, FI} -> {F, norm_fi(FI, Opt)};
                                         {error, Reason} ->
                                            {cannot_stat, F, Reason}
                                      end
                                end, Files)}|Acc], Opt);
gather_info([F|Paths], Acc, Opt) ->
   gather_info(Paths,
               [case file:read_file_info(F) of
                   {ok, FI} -> {F, norm_fi(FI, Opt)};
                   {error, Reason} -> {cannot_stat, F, Reason}
                end|Acc], Opt).

%% @spec tee(term(), filespec()) -> term()
%% @doc Writes the term to the file(s) specified, and returns it.
%% @todo Needs -a option.
tee(Expr, File) ->
   tee(Expr, File, []).

tee(Expr, File, Opt) ->
   do_tee(Expr, glob(File), Opt).

do_tee(Expr, [], _) ->
   Expr;
do_tee(Expr, [F|File], Opt) ->
   {ok, IO} = file:open(F, [write]),
   file:write(IO, Expr),
   file:close(IO),
   do_tee(Expr, File, Opt).
   

read_lines(IO, Acc, Opt) ->
   case io:get_line(IO, []) of
      eof -> Acc;
      Line -> read_lines(IO, [Line|Acc], Opt)
   end.
   

%% @spec rm(filespec()) -> Results
%% @doc Equivalent to {@link rm/2} with no options.
rm(Files) ->
   rm(Files, []).

%% @spec rm(filespec(), options()) -> results()
%% @doc Remove the specified files. If the `verbose' option is given, a result
%% is returned for every file removed. Otherwise, only files which encountered
%% an error have entries in the result list.
rm(Files, Opt) ->
   list_results([ {F, file:delete(F)} || F <- glob(Files) ], Opt).

list_results(Results, Opt) ->
   case proplists:lookup(verbose, Opt) of
      {verbose, true} ->
         Results;
      _ -> [ R || R = {_, {error, _}} <- Results ]
   end.

%% @spec mv(filespec(), Destination::filename()) -> results()
%% @doc Equivalent to {@link mv/3} with no options.
mv(From, To) ->
   mv(From, To, []).

%% @spec cp(filespec(), Destination::filename()) -> results()
%% @doc Equivalent to {@link cp/3} with no options.
cp(From, To) ->
   cp(From, To, []).

%% @spec mv(filespec(), Destination::filename(), options()) -> results()
%% @doc Moves the specified files to their destination. If the destination is
%% a new filename, only one file may be specified. The `verbose' option
%% may be given with the same effect as with {@link rm/2}.
mv(From, To, Opt) ->
   transfiles(glob(From), To, fun file:rename/2, Opt).

%% @spec cp(filespec(), Destination::filename(), options()) -> results()
%% @doc Copies the specified files to their destination. If the destination is
%% a new filename, only one file may be specified. The `verbose' option
%% may be given with the same effect as with {@link rm/2}.
cp(From, To, Opt) ->
   transfiles(glob(From), To, fun file:copy/2, Opt).

transfiles(Files, Dest, F, Opt) when is_atom(Dest) ->
   transfiles(Files, atom_to_list(Dest), F, Opt);
transfiles(Files, Dest, F, Opt) ->
   if
      length(Files) > 1 ->
         true = filelib:is_dir(filename:basename(Dest));
      true -> true
   end,
   list_results(lists:map(fun (File) ->
                   case filelib:is_dir(filename:basename(Dest)) of
                      true ->
                         {File,
                          F(File,
                            filename:join(Dest,
                                          filename:basename(File)))};
                      _ -> {File, F(File, Dest)}
                   end
             end, Files), Opt).

%% @spec glob(filespec()) -> [File]
%%    File = string()
%% @doc Given the atom, string, list of atoms, list of strings, expand any
%% wildcards found and return the result as a list of strings.
glob(Files) ->
   glob(Files, []).

glob([], Acc) ->
   Acc;
glob(File, Acc) when is_atom(File) ->
   glob([atom_to_list(File)], Acc);
glob(File = [X|_], Acc) when not is_atom(X), not is_list(X) ->
   glob([File], Acc);
glob([X|R], Acc) when is_atom(X) ->
   glob([atom_to_list(X)|R], Acc);
glob([File|R], Acc) ->
   List = case filelib:wildcard(File) of
             [] -> [File];
             X -> X
          end,
   glob(R, Acc ++ List).
%% If you've ever read the Unix Hater's Handbook you'll know about the
%% derision regarding this whole business of the shell doing its thing to
%% expand all filenames, and *then* handing this list off to the command. This
%% introduces all kinds of problems, some legitimate (like quoting
%% irritations) and others less so (like missing an opportunity to confirm rm
%% * as distinct from rm file1 file2 file3 ...). The suggestion is to make
%% this filename globbing a library function called by every command instead,
%% and I think it's kind of funny that that's more or less what I'm doing
%% here. -pd

%% @spec mkdir(Directory::filename()) -> ok + {error, term()}
%% @doc Equivalent to {@link mkdir/2} with no options.
mkdir(D) ->
   mkdir(D, []).

%% @spec mkdir(Directory::filename(), options()) -> ok + {error, term()}
%% @doc Makes the specified directory. If the `path' option is given, also
%% creates leading directory components.
mkdir(D, Opt) when is_atom(D) ->
   mkdir(atom_to_list(D), Opt);
mkdir(D, Opt) ->
   case opt(path, Opt) of
      true ->
         filelib:ensure_dir(D);
      _ -> file:make_dir(D)
   end.

job_server({start, Starter}) ->
   Starter ! {started, self()},
   job_server([]);
job_server(Jobs) ->
   receive
      {result, Job, Output} ->
         forward_output(lists:keysearch(Job, 1, Jobs), Output),
         job_server(lists:keydelete(Job, 1, Jobs));
      {'EXIT', Job, Reason} ->
         forward_output(lists:keysearch(Job, 1, Jobs), Reason),
         job_server(lists:keydelete(Job, 1, Jobs));
      {invoke, From, Cmd} ->
         Job = spawn_link(fun () ->
                                job_server ! {result, self(), os:cmd(Cmd)}
                          end),
         From ! {job, Job, Cmd},
         job_server(lists:keystore(Job, 1, Jobs, {Job, From, Cmd}));
      {jobs, From} ->
         From ! {jobs, [ J || J = {_, Sub, _} <- Jobs,
                              Sub =:= From ]},
         job_server(Jobs);
      {kill, From, Job} ->
         exit(Job, kill),
         From ! {killed, Job},
         job_server(lists:keydelete(Job, 1, Jobs));
      stop ->
         ok
   end.

%% @spec output(Command::string()) -> term()
%% @doc Return the saved output from the completed background command
%% (see {@link bg/1}).
output(Cmd) ->
   receive
      {output, Cmd, Output} when is_list(Output) -> chomp(Output);
      {output, Cmd, Output} -> Output
   after 0
        ->
         no_output
   end.

%% @spec start_job_server() -> ok + joberror()
%% @doc Starts the background job server, required for background command
%% invocation (see {@link bg/1}).
%% @todo This really needs to be in a separate module as a gen_server.
start_job_server() ->
   Starter = self(),
   Pid = spawn_link(fun() ->
                          process_flag(trap_exit, true),
                          job_server({start, Starter})
              end),
   register(job_server, Pid),
   receive
      {started, _} -> ok
   after
      1000 ->
         {job_server_timeout, starting}
   end.

%% @spec stop_job_server() -> stop
%% @doc Stops job server.
stop_job_server() ->
   job_server ! stop.

forward_output(false, _) ->
   ok;
forward_output({value, {_, Sub, Cmd}}, Output) ->
   Sub ! {output, Cmd, Output}.

%% @spec bg(command()) -> Job
%%    Job = {job, JobId, command()}
%%    JobId = term()
%% @doc Submit a background command for execution. The returned JobId can be
%% used to kill the job if necessary.
%% @todo Need a way to submit background job with options (output options
%% like nochomp).
bg(Cmd) ->
   job_server ! {invoke, self(), Cmd},
   receive
      {job, Job, Cmd} -> {job, Job, Cmd}
   after
      1000 -> {job_server_timeout, Cmd}
   end.

%% @spec jobs() -> [Job]
%%    Job = {job, JobId, command()}
%%    JobId = term()
%% @doc List currently executing background commands. Note: completed
%% jobs will not be reflected in this list. Once a command is complete its
%% output can be returned with the {@link output/1} command.
jobs() ->
   job_server ! {jobs, self()},
   receive
      {jobs, Jobs} ->
         {jobs, Jobs}
   after
      1000 -> {job_server_timeout, jobs}
   end.

%% @spec kill(JobId) -> {killed, Job} + joberror()
%%    JobId = term()
%%    Job = {job, term(), command()}
%% @doc Kill a currently executing background command.
kill({job, Job, _Command}) ->
   kill(Job);
kill(Job) when is_list(Job) ->
   kill(list_to_pid(Job));
kill(Job) ->
   job_server ! {kill, Job},
   receive
      {killed, Job} ->
         {killed, Job}
   after
      1000 ->
         {job_server_timeout, {kill, Job}}
   end.

%% @spec system(command()) -> Lines
%%    Lines = [string()]
%% @doc Equivalent to {@link system/2} with no options.
system(Cmd) ->
   system(Cmd, []).

%% @spec system(command(), options()) -> Lines
%%    Lines = [string()]
%% @doc Execute the given shell (real shell) command, returning the
%% output as a list of lines. The `nochomp' option can be specified
%% (see {@link cat/2}). If the `background' option is specified this
%% command is equivalent to {@link bg/1}.
system(Cmd, Opt) ->
   Output = case opt(background, Opt) of
               true -> bg(Cmd);
               _ -> os:cmd(Cmd)
            end,
   case opt(nochomp, Opt) of
      true -> chomp(Output);
      _ -> Output
   end.

%% special list functions
%% extended split (split into N chunks) -- let's call it a perl-style split

%% @spec split(Separator, string()) -> [string()]
%% @doc Equivalent to {@link split/3} with no limit on the number of strings
%% returned.
split(Pred, List) ->
   split(Pred, List, -1).

%% our fun split is the inverse of lists:splitwith/2 because I feel it's
%% more obvious to write: split(fun(C) -> C =:= $. end, List), given the
%% other options. If you provide a character "predicate" or a list "predicate"
%% then you are *splitting on* (not retaining) the character/sublist
%% that satisfies the condition. -pd
split2(Pred, List) when is_function(Pred) ->
   {First, Rest} = lists:splitwith(fun(C) -> not Pred(C) end, List),
   case Rest of
      [] -> [First, []];
      [_|Last] -> [First, Last]
   end;
%% Certain common cases given as special atoms
split2(Space, List) when Space == ' '; Space == space ->
   split2({rx, "\\s+"}, List);
split2(Comma, List) when Comma == ','; Comma == comma ->
   split2({rx, "\\s*,\\s*"}, List);
split2({RxSpec, RX}, List) when RxSpec == rx;
                                RxSpec == regex;
                                RxSpec == regexp ->
   tuple_to_list(splitrx(RX, List));
split2(Pred, List) when is_list(Pred) ->
   tuple_to_list(splitsub(Pred, List, []));
split2(Pred, List) ->
   split2(fun(X) -> X =:= Pred end, List).

%% We don't use regexp:split because we only want two limbs at a time
%% for counting up
splitrx(RX, List) ->
   case regexp:first_match(List, RX) of
      {match, 1, 0} ->
         throw({regex_error, zero_width_match_at_start_of_list});
      {match, Start, Len} ->
         {lists:sublist(List, Start - 1),
          lists:nthtail(Start + Len - 1, List)};
      nomatch ->
         {List, []}
   end.

%% Note--does not include substring in split, unlike lists:splitwith/2
splitsub(_, [], Acc) ->
   {lists:reverse(Acc), []};
splitsub(Str, List = [H|T], Acc) ->
   case lists:prefix(Str, List) of
      true -> {lists:reverse(Acc), lists:nthtail(length(Str), List)};
      _ -> splitsub(Str, T, [H|Acc])
   end.

%% @spec split(Separator, string(), Limit::integer()) -> [string()]
%%    Separator = char() + string() + {regex, string()} + '_'
%%                   + ',' + function()
%% @doc Splits the given string with the supplied separator, generating
%% at most the number of substrings indicated as the limit.
%%
%% The separator may be a single character (integer) value, a substring
%% (the string will be split on this substring), a tuple indicating
%% a string to be applied as a regular expression (the string will be
%% split on the matching sections), a function (the string will be
%% split on the single character (list element) for which the fun
%% returns `true'.
%%
%% The separator can also be a "magic" atom. The single-space atom
%% (<code>' '</code> or `space') splits the string on a run of
%% whitespace similar to %% {@link string:words/1}. The comma atom
%% (<code>','</code> or `comma') splits the string on a comma,
%% optionally surrounded by spaces.
split(Pred, List, N) ->
   split(Pred, List, N, []).

split(_, [], _, Chunks) ->
   lists:reverse(Chunks);
split(_, Rest, 1, Chunks) ->
   lists:reverse([Rest|Chunks]);
split(Pred, List, N, Chunks) ->
   [First, Rest] = split2(Pred, List),
   case First of
      [] ->
         split(Pred, Rest, N, Chunks);
      _ ->
         split(Pred, Rest, N - 1, [First|Chunks])
   end.

%% @spec chop(list()) -> list()
%% @doc Removes the list element of the list.
chop(L) ->
   lists:sublist(L, length(L) - 1).

%% @spec join(Separator, [string()]) -> string()
%%    Separator = char() + string()
%% @doc Joins the list of strings with the supplied separator. This is
%% different from {@link string:join/2} in that the separator can be
%% a character, and the order is reversed (and thereby aligned with
%% Perl `join()'.
join(S, L) when is_integer(S) ->
   join([S], L);
join(S, L) ->
   string:join(L, S).

%% @spec clock(function()) -> {Timing, term()}
%%    Timing = [{Attribute, term()}]
%%    Attribute = io + reductions + runtime + real
%% @doc This is the equivalent of the `time' command. It runs the specified
%% function, taking statistics, and returns a tuple containing timing
%% information such as the bytes of I/O (input and output), the approximate
%% number of reductions, the run time and the real (elapsed) time--both in
%% milliseconds.
clock(F) ->
   statistics(wall_clock),
   statistics(runtime),
   {{input, I1}, {output, O1}} = statistics(io),
   statistics(reductions),
   R = F(),
   {_, Reductions} = statistics(reductions),
   {{input, I2}, {output, O2}} = statistics(io),
   {_, Runtime} = statistics(runtime),
   {_, Real} = statistics(wall_clock),
   Io = {{input, I2 - I1}, {output, O2 - O1}},
   {[{io, Io}, {reductions, Reductions}, {runtime, Runtime},
     {real, Real}],
    R}.

%% @spec find(Dir::filespec()) -> [File]
%%    File = string()
%% @doc Equivalent to {@link find/2} with no find-arguments.
find(Dir) ->
   find(Dir, []).

%% @spec find(Dir::filespec(), FindArgs) -> [Result]
%%    FindArgs = [FindArg]
%%    FindArg  = {Predicate, Value::term()} + function() + {exec, function()}
%%    Predicate = mtime + type + access + atime + ctime + mode + links
%%                + major_device + minor_device + inode + uid + gid
%% @doc Descends the filesystem hierarchy rooted at the given directory/ies,
%% applying the predicates (if any) to identify matching files. The predicate
%% can be one of standard file attributes (interpreted in one or two cases
%% as the Unix command `find' does, notably `mtime') or it can be a fun,
%% in which case the file is included in the result if the fun (taking
%% the one argument of the filename) returns `true'. All of the predicates
%% must be true to include the file.
%%
%% Normally, the file is included in the result list. However, if one or more
%% argument of the form `{exec, Function}' is specified, the function is run
%% instead (with the one argument of the filename) and its return value
%% included. The filename is not automatically included if one of these exec
%% functions is run, so if this is desired the fun will need to arrange for
%% the filename to be indicated in the return value.
%%
%% @todo Need more find-like mode logic (right now operates on equality)
find(Dir, Args) when is_tuple(Args) ->
   find(Dir, [Args]);
find(Dir, Args) ->
   {Execs, Preds} = lists:partition(fun ({exec, _}) -> true;
                                        (_) -> false
                                    end, Args),
   find(Dir, [ fpred(P) || P <- Preds], [ F || {exec, F} <- Execs]).

find(Dir, Preds, Execs) ->
   find(glob(Dir), Preds, Execs, []).

find([], _, _, A) ->
   lists:reverse(A);
find([P|Paths], Preds, Execs, A) ->
   case filelib:is_dir(P) of
      true ->
         case file:list_dir(P) of
            {ok, Entries} ->
               Descend = [ filename:join(P, F) || F <- Entries ] ++ Paths,
               case eval_preds(Preds, P) of
                  true ->
                     find(Descend, Preds, Execs, [apply_execs(Execs, P)|A]);
                  _ ->
                     find(Descend, Preds, Execs, A)
               end;
            Err ->
               find(Paths, Preds, Execs, [{error_cannot_descend, P, Err}|A])
         end;
      _ ->
         case eval_preds(Preds, P) of
            true ->
               find(Paths, Preds, Execs, [apply_execs(Execs, P)|A]);
            _ ->
               find(Paths, Preds, Execs, A)
         end
   end.

eval_preds(Preds, P) ->
   eval_preds(true, Preds, P).

eval_preds(Last, [], _) -> Last;
eval_preds(false, _, _) -> false;
eval_preds(true, [F|Preds], Path) ->
   eval_preds(F(Path), Preds, Path).

apply_execs([], Path) ->
   Path;
apply_execs([F], Path) ->
   F(Path);
apply_execs(Execs, Path) ->
   [ F(Path) || F <- Execs ].

f_mtime(F, Int) ->
   S = stat(F),
   D = calendar:time_difference(S#file_info.mtime, {date(), time()}),
   Days = case D of
             {XDays, {0, 0, 0}} -> XDays;
             {PDays, _} -> PDays + 1
          end,
   Int < Days.

f_cmp(What, F, Val) ->
   S = stat(F, [filist]),
   case lists:keysearch(What, 1, S) of
      {value, {What, Val}} ->
         true;
      _ -> false
   end.

fpred(F) when is_function(F) ->
   F;
fpred({mtime, N}) ->
   fun(F) ->
         f_mtime(F, N)
   end;
fpred({type, T}) ->
   fun(F) ->
         f_cmp(type, F, ?ftype_abbr(T))
   end;
fpred({Thing, V}) ->
   fun(F) ->
         f_cmp(Thing, F, V)
   end.

cat_test_() ->
   {ok, Mustelids} = file:consult("ecs_test.mustelids"),
   {ok, Birds} = file:consult("ecs_test.birds"),
   [
    fun() -> c:cd("ecs_test") end,
    ?_assertMatch(Birds, cat(birds)),
    ?_assert([ X ++ "\n" || X <- Birds ] ==
              cat(birds, nochomp)),
    ?_assert([ X ++ "\n" || X <- Birds ] ==
             cat(birds, [nochomp])),
    ?_assert([ X ++ "\n" || X <- Birds ] ==
             cat(birds, [{nochomp, true}])),
    fun() -> c:cd("..") end,
    ?_assertMatch(Birds, cat("ecs_test/birds")),
    ?_assertMatch(Birds, cat("ecs_*/birds")),
    ?_assert(Birds ++ Mustelids == cat("ecs_test/*"))
   ].

ls_test_() ->
   {ok, Files} = file:consult("ecs_test.ls"),
   {ok, [Tops|Files2p]} = file:consult("ecs_test.lsd"),
   Files2 = lists:sort(Tops ++ Files2p),
   {ok, FilesL} = file:consult("ecs_test.lsl"),
   XMtime = fun({F, S}) ->
                  {value, {mtime, {Date, {H, M, _}}}} =
                     lists:keysearch(mtime, 1, S),
                  {F, {Date, {H, M, 0}}}
            end,
   FXParam = fun(P) ->
                   fun({F, S}) ->
                         {value, {P, Val}} = lists:keysearch(P, 1, S),
                         {F, Val}
                   end
             end,
   [
    fun() -> c:cd("ecs_test") end,
    ?_assertMatch(Files, ls()),
    ?_assertMatch(Files2, ls("*")),
    fun() -> c:cd("..") end,
    % Need to track down some kind of bug with DST processing. -pd
    % fun() ->
    %       Ls = ls(ecs_test, [long]),
    %       Mtimes = lists:map(fun({F, S}) ->
    %                                {Date, {H, M, _}} = S#file_info.mtime,
    %                                [UTC|_] =
    %                                   calendar:local_time_to_universal_time_dst(
    %                                     {Date, {H, M, 0}}),
    %                                {F, UTC}
    %                          end, Ls),
    %       Cmtimes = lists:map(XMtime, FilesL),
    %       ?assertMatch(Mtimes, Cmtimes)
    % end,
    fun() ->
          XSizes = FXParam(size),
          Ls = ls("ecs_test", [long, filist]),
          Sizes = lists:map(XSizes, Ls),
          Csizes = lists:map(XSizes, FilesL),
          ?assertMatch(Sizes, Csizes)
    end,
    ?_assertMatch(1, 1)
   ].

find_test_() ->
   [Files, FilesM, FilesF] =
      lists:map(fun(F) ->
                      {ok, L} = file:consult(F),
                      lists:sort(L)
                end, ["ecs_test.find", "ecs_test.findm", "ecs_test.findf"]),
   [
    ?_assertMatch(Files, lists:sort(find("ecs_test"))),
    ?_assertMatch(FilesM, lists:sort(find(ecs_test, {mtime, 1}))),
    ?_assertMatch(FilesF, lists:sort(find(ecs_test, [{type, f}])))
   ].

%% TODO
%% glob
%% grep
%% split
%% cp, mv, mkdir
