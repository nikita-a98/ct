%%% ==================================================================
%%% @author Oleksandr Boiko <erlydev@gmail.com>
%%% @doc
%%% ...
%%% @end
%%% ==================================================================

-ifndef(DROTR_MNESIA_HRL).
-define(DROTR_MNESIA_HRL, 1).

%%% ==================================================================
%%% Macro
%%% ==================================================================

-define(MNESIA_NODES, nodes()).

%%% ==================================================================
%%% Records
%%% ==================================================================

-record(drotr_config, {
    k :: term(),
    v :: term()
}).

-define(TAB_DROTR_CONFIG, drotr_config).
-define(TAB_DROTR_CONFIG_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, set},
        {attributes, record_info(fields, ?TAB_DROTR_CONFIG)},
        {majority, true},
        {load_order, 1},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]).

%% -------------------------------------------------------------------

-record(drotr_counter, {
    c :: term(),
    v :: term()
}).

-define(TAB_DROTR_COUNTERS, drotr_counter).
-define(TAB_DROTR_COUNTERS_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, set},
        {attributes, record_info(fields, ?TAB_DROTR_COUNTERS)},
        {majority, true},
        {load_order, 1},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]).

%% -------------------------------------------------------------------

-record(drotr_counter_temp, {
    c :: term(),
    v :: term()
}).

-define(TAB_DROTR_COUNTERS_TEMP, drotr_counter_temp).
-define(TAB_DROTR_COUNTERS_TEMP_DEF,
    [
        {access_mode, read_write},
        {ram_copies, nodes()},
        {type, set},
        {attributes, record_info(fields, ?TAB_DROTR_COUNTERS_TEMP)},
        {majority, true},
        {load_order, 1},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]).

%% -------------------------------------------------------------------

-record(drotr_cache, {
    k :: term(),
    v :: term(),
    expire_at :: pos_integer() | infinity
}).

-define(TAB_DROTR_CACHE, drotr_cache).
-define(TAB_DROTR_CACHE_DEF,
    [
        {access_mode, read_write},
        {ram_copies, nodes()},
        {type, set},
        {attributes, record_info(fields, ?TAB_DROTR_CACHE)},
        {majority, true},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]},
        {index, [expire_at]}
    ]).

%% -------------------------------------------------------------------

-type profile() :: #{
gender => binary(),
birthdate => pos_integer(),
country => binary(),
lang => binary(),
xlangs => [binary(), ...],
about => binary()
}.

-record(drotr_user, {
    user_id     = null :: term(),
    profile     = #{} :: profile(),
    created_at  = null :: pos_integer(),
    blocked_at  = null :: pos_integer(),
    deleted_at  = null :: pos_integer()
}).

-define(TAB_DROTR_USERS, drotr_user).
-define(TAB_DROTR_USERS_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, ordered_set},
        {attributes, record_info(fields, ?TAB_DROTR_USERS)},
        {majority, true},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]},
        {index, []}
    ]).

%% -------------------------------------------------------------------

-record(drotr_user_id_seq, {
    n :: term(),
    v :: term()
}).

-define(TAB_DROTR_USERS_ID_SEQ, drotr_user_id_seq).
-define(TAB_DROTR_USERS_ID_SEQ_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, set},
        {attributes, record_info(fields, ?TAB_DROTR_USERS_ID_SEQ)},
        {majority, true},
        {load_order, 1},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]).

%% -------------------------------------------------------------------

-record(drotr_user_phone, {
    phone :: binary(),
    user_id :: term()
    %is_main :: boolean()
}).

-define(TAB_DROTR_USER_PHONES, drotr_user_phone).
-define(TAB_DROTR_USER_PHONES_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, set},
        {attributes, record_info(fields, ?TAB_DROTR_USER_PHONES)},
        {majority, true},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]},
        {index, [user_id]}
    ]).

%% -------------------------------------------------------------------

-record(drotr_user_email, {
    email :: binary(),
    user_id :: term()
    %is_main :: boolean()
}).

-define(TAB_DROTR_USER_EMAILS, drotr_user_email).
-define(TAB_DROTR_USER_EMAILS_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, set},
        {attributes, record_info(fields, ?TAB_DROTR_USER_EMAILS)},
        {majority, true},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]},
        {index, [user_id]}
    ]).

%% -------------------------------------------------------------------

-record(drotr_user_name, {
    name :: binary(),
    user_id :: term()
}).

-define(TAB_DROTR_USER_NAMES, drotr_user_name).
-define(TAB_DROTR_USER_NAMES_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, set},
        {attributes, record_info(fields, ?TAB_DROTR_USER_NAMES)},
        {majority, true},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]},
        {index, [user_id]}
    ]).

%% -------------------------------------------------------------------

-record(drotr_user_password, {
    user_id :: term(),
    hash :: binary(),
    updated_at :: pos_integer()
}).

-define(TAB_DROTR_USER_PASSWORDS, drotr_user_password).
-define(TAB_DROTR_USER_PASSWORDS_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, set},
        {attributes, record_info(fields, ?TAB_DROTR_USER_PASSWORDS)},
        {majority, true},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]},
        {index, []}
    ]).

%% -------------------------------------------------------------------

-record(drotr_users_online, {
    user_id :: term(),
    node_id :: term(),
    conn_pid :: pid()
}).

-define(TAB_DROTR_USERS_ONLINE, drotr_users_online).
-define(TAB_DROTR_USERS_ONLINE_DEF,
    [
        {access_mode, read_write},
        {ram_copies, nodes()},
        {type, set},
        {attributes, record_info(fields, ?TAB_DROTR_USERS_ONLINE)},
        {majority, true},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]},
        {index, []}
    ]).

%% -------------------------------------------------------------------

-record(drotr_onetime_code, {
    global_user_id :: term(),
    code :: binary(),
    expired_at :: pos_integer()
}).

-define(TAB_DROTR_ONETIME_CODES, drotr_onetime_code).
-define(TAB_DROTR_ONETIME_CODES_DEF,
    [
        {access_mode, read_write},
        {ram_copies, ?MNESIA_NODES},
        {type, set},
        {attributes, record_info(fields, ?TAB_DROTR_ONETIME_CODES)},
        {majority, true},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]},
        {index, []}
    ]).

%% -------------------------------------------------------------------

-record(drotr_jwt, {
    jti :: binary(),
    user_id :: term(),
    jwt_refresh :: binary(),
    exp :: pos_integer()
}).

-define(TAB_DROTR_JWT, drotr_jwt).
-define(TAB_DROTR_JWT_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, set},
        {attributes, record_info(fields, ?TAB_DROTR_JWT)},
        {majority, true},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]},
        {index, [user_id, exp]}
    ]).

%% -------------------------------------------------------------------

-record(drotr_channel, {
    channel_id  = null :: term(),
    owner_id    = null :: term(),
    name        = null :: binary(),
    topic       = null :: binary(),
    image       = null :: binary(),
    settings    = null :: term(),
    created_at  = null :: pos_integer(),
    deleted_at  = null :: pos_integer(),
    blocked_at  = null :: pos_integer()
}).

-define(TAB_DROTR_CHANNELS, drotr_channel).
-define(TAB_DROTR_CHANNELS_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, ordered_set},
        {attributes, record_info(fields, ?TAB_DROTR_CHANNELS)},
        {majority, true},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]},
        {index, [owner_id, name]}
    ]).

%% -------------------------------------------------------------------

-record(drotr_channel_id_seq, {
    n :: term(),
    v :: term()
}).

-define(TAB_DROTR_CHANNELS_ID_SEQ, drotr_channel_id_seq).
-define(TAB_DROTR_CHANNELS_ID_SEQ_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, set},
        {attributes, record_info(fields, ?TAB_DROTR_CHANNELS_ID_SEQ)},
        {majority, true},
        {load_order, 1},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]).

%% -------------------------------------------------------------------

-record(drotr_channel_subscriber, {
    k             = null :: {ChannelId :: term(), SubscriberId :: term()},
    approved      = false :: boolean(),
    created_at    = null :: pos_integer()
}).

-define(TAB_DROTR_CHANNEL_SUBSCRIBERS, drotr_channel_subscriber).
-define(TAB_DROTR_CHANNEL_SUBSCRIBERS_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, ordered_set},
        {attributes, record_info(fields, ?TAB_DROTR_CHANNEL_SUBSCRIBERS)},
        {majority, true},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]).

%% -------------------------------------------------------------------

-record(drotr_groupchat, {
    chat_id     = null :: term(),
    topic       = null :: binary(),
    image       = null :: binary(),
    settings    = null :: maps:map(), % TODO: access = public | protected; access_key
    created_at  = null :: pos_integer()
}).

-define(TAB_DROTR_GROUPCHATS, drotr_groupchat).
-define(TAB_DROTR_GROUPCHATS_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, ordered_set},
        {attributes, record_info(fields, ?TAB_DROTR_GROUPCHATS)},
        {majority, true},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]).

%% -------------------------------------------------------------------

-record(drotr_groupchat_id_seq, {
    n :: term(),
    v :: term()
}).

-define(TAB_DROTR_GROUPCHATS_ID_SEQ, drotr_groupchat_id_seq).
-define(TAB_DROTR_GROUPCHATS_ID_SEQ_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, set},
        {attributes, record_info(fields, ?TAB_DROTR_GROUPCHATS_ID_SEQ)},
        {majority, true},
        {load_order, 1},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]).

%% -------------------------------------------------------------------

-record(drotr_groupchat_member, {
    k             = null :: {ChatId :: term(), MemberId :: term()},
    role          = member :: member | admin,
    perms         = [r, w] :: [r | w | x, ...],
    created_at    = null :: pos_integer()
}).

-define(TAB_DROTR_GROUPCHAT_MEMBERS, drotr_groupchat_member).
-define(TAB_DROTR_GROUPCHAT_MEMBERS_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, ordered_set},
        {attributes, record_info(fields, ?TAB_DROTR_GROUPCHAT_MEMBERS)},
        {majority, true},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]).

%% -------------------------------------------------------------------

-record(drotr_groupchat_log, {
    k :: {binary(), binary()},  % {ChatId, MessageId}
    packet :: maps:map()
}).

-define(TAB_DROTR_GROUPCHAT_LOG, drotr_groupchat_log).
-define(TAB_DROTR_GROUPCHAT_LOG_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, ordered_set},
        {attributes, record_info(fields, ?TAB_DROTR_GROUPCHAT_LOG)},
        {majority, false},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]).

%% -------------------------------------------------------------------

-record(drotr_privchat, {
    chat_id     = null :: tuple(),
    extra       = null :: maps:map(),
    created_at  = null :: pos_integer()
}).

-define(TAB_DROTR_PRIVCHATS, drotr_privchat).
-define(TAB_DROTR_PRIVCHATS_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, ordered_set},
        {attributes, record_info(fields, ?TAB_DROTR_PRIVCHATS)},
        {majority, true},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]).

%% -------------------------------------------------------------------

-record(drotr_contact, {
    k           = null :: {ContactOwnerId :: term(), ContactId :: term()},
    name        = null :: binary(),
    tags        = [] :: [binary(), ...],
    note        = null :: binary()
}).

-define(TAB_DROTR_CONTACTS, drotr_contact).
-define(TAB_DROTR_CONTACTS_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, ordered_set},
        {attributes, record_info(fields, ?TAB_DROTR_CONTACTS)},
        {majority, true},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]).

%% -------------------------------------------------------------------

-record(drotr_muted_user, {
    k         = null :: {MutedByUserId :: term(), MutedUserId :: term()},
    muted_at  = null :: pos_integer()
}).

-define(TAB_DROTR_MUTED_USERS, drotr_muted_user).
-define(TAB_DROTR_MUTED_USERS_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, set},
        {attributes, record_info(fields, ?TAB_DROTR_MUTED_USERS)},
        {majority, true},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]).

%% -------------------------------------------------------------------

-record(drotr_blocked_user, {
    k         = null :: {BlockedByUserId :: term(), BlockedUserId :: term()},
    blocked_at  = null :: pos_integer()
}).

-define(TAB_DROTR_BLOCKED_USERS, drotr_blocked_user).
-define(TAB_DROTR_BLOCKED_USERS_DEF,
    [
        {access_mode, read_write},
        {disc_copies, [node() | ?MNESIA_NODES]},
        {type, set},
        {attributes, record_info(fields, ?TAB_DROTR_BLOCKED_USERS)},
        {majority, true},
        {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
    ]).

%% -------------------------------------------------------------------

-define(MNESIA_TABS_DEFS,
    [
        {?TAB_DROTR_CONFIG, ?TAB_DROTR_CONFIG_DEF},
        {?TAB_DROTR_COUNTERS, ?TAB_DROTR_COUNTERS_DEF},
        {?TAB_DROTR_COUNTERS_TEMP, ?TAB_DROTR_COUNTERS_TEMP_DEF},
        {?TAB_DROTR_CACHE, ?TAB_DROTR_CACHE_DEF},
        {?TAB_DROTR_USERS, ?TAB_DROTR_USERS_DEF},
        {?TAB_DROTR_USERS_ID_SEQ, ?TAB_DROTR_USERS_ID_SEQ_DEF},
        {?TAB_DROTR_USER_PHONES, ?TAB_DROTR_USER_PHONES_DEF},
        {?TAB_DROTR_USER_EMAILS, ?TAB_DROTR_USER_EMAILS_DEF},
        {?TAB_DROTR_USER_NAMES, ?TAB_DROTR_USER_NAMES_DEF},
        {?TAB_DROTR_USER_PASSWORDS, ?TAB_DROTR_USER_PASSWORDS_DEF},
        {?TAB_DROTR_USERS_ONLINE, ?TAB_DROTR_USERS_ONLINE_DEF},
        {?TAB_DROTR_ONETIME_CODES, ?TAB_DROTR_ONETIME_CODES_DEF},
        {?TAB_DROTR_JWT, ?TAB_DROTR_JWT_DEF},
        {?TAB_DROTR_CHANNELS, ?TAB_DROTR_CHANNELS_DEF},
        {?TAB_DROTR_CHANNELS_ID_SEQ, ?TAB_DROTR_CHANNELS_ID_SEQ_DEF},
        {?TAB_DROTR_CHANNEL_SUBSCRIBERS, ?TAB_DROTR_CHANNEL_SUBSCRIBERS_DEF},
        {?TAB_DROTR_GROUPCHATS, ?TAB_DROTR_GROUPCHATS_DEF},
        {?TAB_DROTR_GROUPCHATS_ID_SEQ, ?TAB_DROTR_GROUPCHATS_ID_SEQ_DEF},
        {?TAB_DROTR_GROUPCHAT_MEMBERS, ?TAB_DROTR_GROUPCHAT_MEMBERS_DEF},
        {?TAB_DROTR_GROUPCHAT_LOG, ?TAB_DROTR_GROUPCHAT_LOG_DEF},
        {?TAB_DROTR_PRIVCHATS, ?TAB_DROTR_PRIVCHATS_DEF},
        {?TAB_DROTR_CONTACTS, ?TAB_DROTR_CONTACTS_DEF},
        {?TAB_DROTR_MUTED_USERS, ?TAB_DROTR_MUTED_USERS_DEF},
        {?TAB_DROTR_BLOCKED_USERS, ?TAB_DROTR_BLOCKED_USERS_DEF}
    ]).

-define(MNESIA_TABS_LIST, [TabName || {TabName, _} <- ?MNESIA_TABS_DEFS]).

-endif.