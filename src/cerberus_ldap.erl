-module(cerberus_ldap).

-behaviour(cerberus_directory).

%% Include files
-include_lib("eldap/include/eldap.hrl").

%% Exported functions

-export([
    authenticate/3,
    groups/1,
    users/1
]).

-export_type([
    config_data/0
]).

-type config_data() :: [{atom(), term()}].

%% API

-spec authenticate(config_data(), string(), string()) -> cerberus:authentication_result().

authenticate(Props, User, Password) ->
    Host = require_prop(host, Props),
    Options = maybe_options([port, timeout, ssl], Props, []),
    Base = require_prop(base, Props),
    Uid = proplists:get_value(uid, Props, "uid"),
    BindDn = proplists:get_value(bind_dn, Props, undefined),
    BindPassword = proplists:get_value(bind_password, Props, undefined),
    ShouldAuthConnection = BindDn =/= undefined andalso BindPassword =/= undefined,
    case eldap:open([Host], Options) of
        {ok, Handle} ->
            PreAuthResult = case ShouldAuthConnection of
                true -> eldap:simple_bind(Handle, BindDn, BindPassword);
                false -> ok
            end,
            case PreAuthResult of
                ok ->
                    SearchOptions0 = [{base, Base}, {filter, eldap:equalityMatch(Uid, User)}],
                    SearchOptions = maybe_options([timeout], Props, SearchOptions0),
                    Result = do_auth(Handle, SearchOptions, Password),
                    eldap:close(Handle),
                    Result;
                {error, AuthReason} ->
                    eldap:close(Handle),
                    {error, AuthReason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec groups(config_data()) -> cerberus:group_result().

groups(Props) ->
    Host = require_prop(host, Props),
    Options = maybe_options([port, timeout, ssl], Props, []),
    BindDn = proplists:get_value(bind_dn, Props, undefined),
    BindPassword = proplists:get_value(bind_password, Props, undefined),
    ShouldAuthConnection = BindDn =/= undefined andalso BindPassword =/= undefined,
    case eldap:open([Host], Options) of
        {ok, Handle} ->
            PreAuthResult = case ShouldAuthConnection of
                true -> eldap:simple_bind(Handle, BindDn, BindPassword);
                false -> ok
            end,
            case PreAuthResult of
                ok ->
                    Result = case get_users(Handle, Props) of
                        {ok, Users} ->
                            case get_groups(Handle, Props) of
                                {ok, Groups} ->
                                    Groups1 = maps:map(fun(_Group, Members) ->
                                        Members1 = [maps:get(Member, Users, undefined) || Member <- Members],
                                        [M || M <- Members1, M =/= undefined]
                                    end, Groups),
                                    {ok, Groups1};
                                {error, Reason} -> {error, Reason}
                            end;
                        {error, Reason} -> {error, Reason}
                    end,
                    eldap:close(Handle),
                    Result;
                {error, AuthReason} ->
                    eldap:close(Handle),
                    {error, AuthReason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec users(config_data()) -> cerberus:user_result().

users(Props) ->
    Host = require_prop(host, Props),
    Options = maybe_options([port, timeout, ssl], Props, []),
    BindDn = proplists:get_value(bind_dn, Props, undefined),
    BindPassword = proplists:get_value(bind_password, Props, undefined),
    ShouldAuthConnection = BindDn =/= undefined andalso BindPassword =/= undefined,
    case eldap:open([Host], Options) of
        {ok, Handle} ->
            PreAuthResult = case ShouldAuthConnection of
                true -> eldap:simple_bind(Handle, BindDn, BindPassword);
                false -> ok
            end,
            case PreAuthResult of
                ok ->
                    Result = case get_users(Handle, Props) of
                        {ok, Users} ->
                            UserMap = maps:fold(fun(CN, [Uid], AccMap) -> AccMap#{Uid => extract_name(CN)} end, #{}, Users),
                            {ok, UserMap};
                        {error, Reason} -> {error, Reason}
                    end,
                    eldap:close(Handle),
                    Result;
                {error, AuthReason} ->
                    eldap:close(Handle),
                    {error, AuthReason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Local functions

require_prop(Key, Props) ->
    case proplists:get_value(Key, Props) of
        undefined -> erlang:error({unset, Key});
        Value -> Value
    end.

maybe_option(Key, Props, Options) ->
    case proplists:get_value(Key, Props) of
        undefined -> Options;
        Value -> [{Key,Value} | Options]
    end.

maybe_options(Keys, Props, Opts) ->
    lists:foldl(fun(K,Acc) -> maybe_option(K,Props,Acc) end, Opts, Keys).

do_auth(Handle, SearchOptions, Password) ->
    case eldap:search(Handle, SearchOptions) of
        {ok, #eldap_search_result{entries = [E]}} when entries =/= [] ->
            case eldap:simple_bind(Handle, E#eldap_entry.object_name, Password) of
                ok -> ok;
                {error, Error} -> {reject, Error}
            end;
        {ok, #eldap_search_result{}} ->
            {reject, unknown_user};
        {error, Error} ->
            {error, Error}
    end.

get_users(Handle, Props) ->
    Base = require_prop(base, Props),
    Uid = proplists:get_value(uid, Props, "uid"),
    SearchOptions0 = [{base, Base}, {filter, eldap:present(Uid)}],
    SearchOptions = maybe_options([timeout], Props, SearchOptions0),
    case eldap:search(Handle, SearchOptions) of
        {ok, #eldap_search_result{entries = UserEntries}} ->
            Users = lists:foldl(fun(#eldap_entry{object_name = CN, attributes = Attrs}, Acc) ->
                Acc#{CN => proplists:get_value(Uid, Attrs)}
            end, #{}, UserEntries),
            {ok, Users};
        {error, Reason} ->
            {error, Reason}
    end.

get_groups(Handle, Props) ->
    BaseGroups = require_prop(base_groups, Props),
    CN = proplists:get_value(cn, Props, "cn"),
    Member = proplists:get_value(unique_member, Props, "member"),
    UniqueMember = proplists:get_value(unique_member, Props, "uniqueMember"),
    SearchOptions0 = [{base, BaseGroups}, {filter, eldap:'or'([eldap:present(Member), eldap:present(UniqueMember)])}],
    SearchOptions = maybe_options([timeout], Props, SearchOptions0),
    case eldap:search(Handle, SearchOptions) of
        {ok, #eldap_search_result{entries = UserEntries}} ->
            Users = lists:foldl(fun(#eldap_entry{attributes = Attrs}, Acc) ->
                Key = proplists:get_value(CN, Attrs),
                Acc#{Key => member(Member, UniqueMember, Attrs)}
            end, #{}, UserEntries),
            {ok, Users};
        {error, Reason} ->
            {error, Reason}
    end.

member(Member, UniqueMember, Attrs) ->
    case proplists:get_value(UniqueMember, Attrs) of
        undefined -> proplists:get_value(Member, Attrs);
        Value -> Value
    end.

extract_name(CanonicalName) ->
    case string:split(CanonicalName, ",") of
        ["cn=" ++ CN |_] -> CN;
        _ -> undefined
    end.
