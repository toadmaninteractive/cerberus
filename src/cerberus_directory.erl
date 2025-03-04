-module(cerberus_directory).

%% Include files

%% Exported functions

%% API

-callback authenticate(term(), string(), string()) -> cerberus:authentication_result().
-callback groups(term()) -> cerberus:group_result().
-callback users(term()) -> cerberus:user_result().

%% Local functions
