#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <glib.h>
#include <purple.h>
#include <erl_interface.h>

#define UI_NAME "prplerl"
#define PURPLE_GLIB_READ_COND  (G_IO_IN | G_IO_HUP | G_IO_ERR)
#define PURPLE_GLIB_WRITE_COND (G_IO_OUT | G_IO_HUP | G_IO_ERR | G_IO_NVAL)

typedef unsigned char byte;

static int
read_exact(byte *buf, int len)
{
  int i, got=0;

  do {
    if ((i = read(fileno(stdin), buf+got, len-got)) <= 0)
      return(i);
    got += i;
  } while (got<len);

  return(len);
}

static int
write_exact(byte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(fileno(stdout), buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);

  return (len);
}

static int
read_cmd(byte *buf)
{
  int len;

  if (read_exact(buf, 2) != 2)
    return(-1);
  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, len);
}

static int
write_cmd(byte *buf, int len)
{
  byte li;

  li = (len >> 8) & 0xff;
  write_exact(&li, 1);

  li = len & 0xff;
  write_exact(&li, 1);

  return write_exact(buf, len);
}

int write_term(ETERM *term) {
    int len, retval;
    byte *buf;
    len = erl_term_len(term);
    buf = erl_malloc(len);

    retval = erl_encode(term, buf);
    write_cmd(buf, len);

    erl_free(buf);

    return retval;
}


// PURPLE

GHashTable* accounts = NULL;


typedef struct _PurpleGLibIOClosure {
    PurpleInputFunction function;
    guint result;
    gpointer data;
} PurpleGLibIOClosure;

static void purple_glib_io_destroy(gpointer data)
{
    g_free(data);
}

static gboolean purple_glib_io_invoke(GIOChannel *source, GIOCondition condition, gpointer data)
{
    PurpleGLibIOClosure *closure = data;
    PurpleInputCondition purple_cond = 0;

    if (condition & PURPLE_GLIB_READ_COND)
        purple_cond |= PURPLE_INPUT_READ;
    if (condition & PURPLE_GLIB_WRITE_COND)
        purple_cond |= PURPLE_INPUT_WRITE;

    closure->function(closure->data, g_io_channel_unix_get_fd(source),
                      purple_cond);

    return TRUE;
}

static guint glib_input_add(gint fd, PurpleInputCondition condition, PurpleInputFunction function,
                               gpointer data)
{
    PurpleGLibIOClosure *closure = g_new0(PurpleGLibIOClosure, 1);
    GIOChannel *channel;
    GIOCondition cond = 0;

    closure->function = function;
    closure->data = data;

    if (condition & PURPLE_INPUT_READ)
        cond |= PURPLE_GLIB_READ_COND;
    if (condition & PURPLE_INPUT_WRITE)
        cond |= PURPLE_GLIB_WRITE_COND;

    channel = g_io_channel_unix_new(fd);
    closure->result = g_io_add_watch_full(channel, G_PRIORITY_DEFAULT, cond,
                          purple_glib_io_invoke, closure, purple_glib_io_destroy);

    g_io_channel_unref(channel);
    return closure->result;
}

static PurpleEventLoopUiOps glib_eventloops =
{
    g_timeout_add,
    g_source_remove,
    glib_input_add,
    g_source_remove,
    NULL,
#if GLIB_CHECK_VERSION(2,14,0)
    g_timeout_add_seconds,
#else
    NULL,
#endif

    NULL,
    NULL,
    NULL
};

static void
prplerl_write_conv_im(PurpleConversation *conv, const char *who,
              const char *message, PurpleMessageFlags flags, time_t mtime)
{
    int *acc_id;
    ETERM *result;
    PurpleBuddy *buddy;

    // this message is sent by us.. ignore it
    if(flags & PURPLE_MESSAGE_SEND) return;

    acc_id = (int *) conv->account->ui_data;
    buddy = purple_find_buddy(conv->account, who);
    if(buddy != NULL)
        who = purple_buddy_get_name(buddy);

    result = erl_format("{~a, ~i, ~i, ~s, ~s}", "message", acc_id,
                        (int) mktime(localtime(&mtime)), who, message);
    write_term(result);
    erl_free_term(result);
}

static void
prplerl_write_conv_chat(PurpleConversation *conv, const char *who,
              const char *message, PurpleMessageFlags flags, time_t mtime)
{
    int *acc_id;
    ETERM *result;
    PurpleBuddy *buddy;

    // this message is sent by us.. ignore it
    if(flags & PURPLE_MESSAGE_SEND) return;

    acc_id = (int *) conv->account->ui_data;
    buddy = purple_find_buddy(conv->account, who);
    if(buddy != NULL)
        who = purple_buddy_get_name(buddy);

    result = erl_format("{~a, ~i, ~i, ~s, ~s, ~s}", "chat_message", acc_id,
                        (int) mktime(localtime(&mtime)), who, message);
    write_term(result);
    erl_free_term(result);
}

void
prplerl_create_conversation(PurpleConversation *conv)
{
}

static PurpleConversationUiOps prplerl_conv_uiops =
{
    prplerl_create_conversation, /* create_conversation  */
    NULL,                        /* destroy_conversation */
    prplerl_write_conv_chat,     /* write_chat           */
    prplerl_write_conv_im,       /* write_im             */
    NULL,                        /* write_conv           */
    NULL,                        /* chat_add_users       */
    NULL,                        /* chat_rename_user     */
    NULL,                        /* chat_remove_users    */
    NULL,                        /* chat_update_user     */
    NULL,                        /* present              */
    NULL,                        /* has_focus            */
    NULL,                        /* custom_smiley_add    */
    NULL,                        /* custom_smiley_write  */
    NULL,                        /* custom_smiley_close  */
    NULL,                        /* send_confirm         */
    NULL,
    NULL,
    NULL,
    NULL
};

static void connection_connected(PurpleConnection *gc)
{
    PurpleAccount *account = purple_connection_get_account(gc);
    int *acc_id = (int *) account->ui_data;
    ETERM *result;

    result = erl_format("{~a, ~i, ~s, ~s}", "connected", acc_id,
        account->username, account->protocol_id);
    write_term(result);
    erl_free_term(result);
}

static void connection_disconnected(PurpleConnection *gc)
{
    PurpleAccount *account = purple_connection_get_account(gc);
    int *acc_id = (int *) account->ui_data;
    ETERM *result;

    result = erl_format("{~a, ~i, ~s, ~s}", "disconnected", acc_id,
        account->username, account->protocol_id);
    write_term(result);
    erl_free_term(result);
}

static void connection_notice(PurpleConnection *gc, const char *text)
{
    PurpleAccount *account = purple_connection_get_account(gc);
    int *acc_id = (int *) account->ui_data;
    ETERM *result;

    result = erl_format("{~a, ~i, ~s, ~s, ~s}", "notice", acc_id,
        account->username, account->protocol_id, text);
    write_term(result);
    erl_free_term(result);
}

static void connection_report_disconnect_reason(PurpleConnection *gc, PurpleConnectionError reason, const char *text)
{
    PurpleAccount *account = purple_connection_get_account(gc);
    int *acc_id = (int *) account->ui_data;
    ETERM *result;

    if (purple_connection_error_is_fatal(reason)) {
        result = erl_format("{~a, ~a, ~i, ~s, ~s, ~s}", "disconnected", "fatal", acc_id,
                    account->username, account->protocol_id, text);
    } else {
        result = erl_format("{~a, ~i, ~s, ~s, ~s}", "disconnected", acc_id,
                    account->username, account->protocol_id, text);
    }
    write_term(result);
    erl_free_term(result);
}

static PurpleConnectionUiOps conn_ui_ops =
{
    NULL, /* connect_progress */
    connection_connected,
    connection_disconnected,
    connection_notice,
    NULL, /* report_disconnect */
    NULL, /* network_connected */
    NULL, /* network disconnected */
    connection_report_disconnect_reason
};

static void
prplerl_ui_init(void)
{
    purple_conversations_set_ui_ops(&prplerl_conv_uiops);
    purple_connections_set_ui_ops(&conn_ui_ops);
}

static PurpleCoreUiOps prplerl_core_uiops =
{
    NULL, /* ui_prefs_init */
    NULL, /* debug_ui_init */
    prplerl_ui_init,
    NULL, /* quit */
    NULL, /* get_ui_info */
    NULL,
    NULL,
    NULL
};

// /PURPLE

void
prplerl_login(ETERM *data)
{
    int acc_id;

    ETERM *id;
    ETERM *username;
    ETERM *password;
    ETERM *protocol;
    ETERM *pattern;

    PurpleAccount *account;

    pattern = erl_format("{Id, User, Password, Protocol}");
    if (erl_match(pattern, data)) {
        id = erl_var_content(pattern, "Id");
        acc_id = ERL_INT_VALUE(id);
        username = erl_var_content(pattern, "User");
        password = erl_var_content(pattern, "Password");
        protocol = erl_var_content(pattern, "Protocol");

        account = purple_account_new(erl_iolist_to_string(username),
                                     erl_iolist_to_string(protocol));
        account->ui_data = GINT_TO_POINTER(acc_id);
        purple_account_set_password(account, erl_iolist_to_string(password));
        purple_account_set_enabled(account, UI_NAME, TRUE);

        g_hash_table_insert(accounts, GINT_TO_POINTER(acc_id), account);
    } else {
        fprintf(stderr, "Pattern not match {Id, User, Password, Protocol}");
    }

    erl_free_term(id);
    erl_free_term(username);
    erl_free_term(password);
    erl_free_term(protocol);
    erl_free_term(pattern);
}

void
prplerl_sendmsg(ETERM *data)
{
    int acc_id;

    ETERM *id;
    ETERM *to;
    ETERM *msg;
    ETERM *pattern;
    PurpleAccount *account;

    pattern = erl_format("{Id, To, Msg}");
    if (erl_match(pattern, data)) {
        id = erl_var_content(pattern, "Id");
        acc_id = ERL_INT_VALUE(id);
        account = g_hash_table_lookup(accounts, GINT_TO_POINTER(acc_id));
        if(account != NULL) {
            to = erl_var_content(pattern, "To");
            msg = erl_var_content(pattern, "Msg");

            PurpleConversation *conv;
            PurpleConvIm       *im;

            conv = purple_find_conversation_with_account(PURPLE_CONV_TYPE_IM,
                                 erl_iolist_to_string(to),
                                 account);
            // If not, create a new one
            if (conv == NULL)
                conv = purple_conversation_new(PURPLE_CONV_TYPE_IM, account,
                                               erl_iolist_to_string(to));

            // Get the IM specific data
            im = purple_conversation_get_im_data(conv);
            purple_conv_im_send(im, erl_iolist_to_string(msg));
        } else {
            fprintf(stderr, "Invalid Account Id");
        }

    } else {
        fprintf(stderr, "Pattern not match {To, Msg}");
    }

    erl_free_term(to);
    erl_free_term(msg);
    erl_free_term(pattern);
}


gboolean chan_data(GIOChannel *source, GIOCondition condition, gpointer gdata)
{
    // if the stdin is dead, harakiri
    if (condition == G_IO_ERR || condition == G_IO_HUP) {
        abort();
    }

    ETERM *data;
    ETERM *ecmd;
    ETERM *tuple;
    ETERM *pattern;

    char *cmd;

    byte buf[1000];

    read_cmd(buf);
    tuple = erl_decode(buf);

    pattern = erl_format("{Cmd, Data}");
    if (erl_match(pattern, tuple)) {
        ecmd = erl_var_content(pattern, "Cmd");
        cmd = ERL_ATOM_PTR(ecmd);
        data = erl_var_content(pattern, "Data");

        if(strcmp(cmd, "login") == 0) {
            prplerl_login(data);
        } else if(strcmp(cmd, "sendmsg") == 0) {
            prplerl_sendmsg(data);
        } else {
            fprintf(stderr, "Unknown Command: %s\n", cmd);
        }
    } else {
        fprintf(stderr, "Pattern not match {Cmd, Data} invalid command");
    }

    erl_free_term(ecmd);
    erl_free_term(data);
    erl_free_term(tuple);
    erl_free_term(pattern);

    return TRUE;
}

static void prplerl_blist_update(PurpleBuddyList *list, PurpleBlistNode *node)
{
    ETERM *result;
    int *acc_id;
    int logged_in;
    int is_available;

    if(node->type == PURPLE_BLIST_BUDDY_NODE) {
        PurpleBuddy *buddy = (PurpleBuddy*) node;
        PurpleStatus *status;
        const char *status_name;
        const char *status_msg;

        acc_id = (int *) buddy->account->ui_data;

        logged_in = purple_presence_is_online(buddy->presence);
        is_available = purple_presence_is_available(buddy->presence);
        status = purple_presence_get_active_status(buddy->presence);
        status_name = purple_status_get_name(status);
        status_msg = purple_status_get_attr_string(status, "message");

        result = erl_format("{~a, ~i, ~s, {~a, ~a}, {~s, ~s}}", "buddy_update", acc_id,
                            buddy->name, logged_in ? "online" : "offline",
                            is_available ? "available" : "away",
                            status_name ? status_name : "",
                            status_msg ? status_msg : "no_message"
        );

        write_term(result);
        erl_free_term(result);
    }
}

static void prplerl_blist_new(PurpleBlistNode *node)
{
    if(node->type == PURPLE_BLIST_BUDDY_NODE) {
        PurpleBuddy *buddy = (PurpleBuddy*) node;
        // fprintf(stderr, "NEW BUDDY %s\n", buddy->name);
    }
}

static void prplerl_blist_remove(PurpleBuddyList *list, PurpleBlistNode *node)
{
    if(node->type == PURPLE_BLIST_BUDDY_NODE) {
        PurpleBuddy *buddy = (PurpleBuddy*) node;
        fprintf(stderr, "REMOVE BUDDY\n");
    }
}


static PurpleBlistUiOps prplerl_blist_uiops =
{
    NULL,
    prplerl_blist_new,
    NULL,
    prplerl_blist_update,
    prplerl_blist_remove
};


static void
init_libpurple(void)
{
    char tmpdir[100] = "/tmp/.prplerl-XXXXXXX";

    if (mkdtemp(tmpdir) == NULL) {
        fprintf(stderr, "Can't create tmp\n");
        abort();
        return;
    }

    purple_util_set_user_dir(tmpdir);

    purple_debug_set_enabled(FALSE);
    purple_core_set_ui_ops(&prplerl_core_uiops);
    purple_blist_set_ui_ops(&prplerl_blist_uiops);
    purple_eventloop_set_ui_ops(&glib_eventloops);

    if (!purple_core_init(UI_NAME)) {
        abort();
    }

    purple_set_blist(purple_blist_new());
    purple_blist_load();
}

static void
loop(void)
{
    accounts = g_hash_table_new(g_direct_hash, g_direct_equal);
    // stdin channel
    GIOChannel *chan = g_io_channel_unix_new(fileno(stdin));

    GMainLoop *mainloop = g_main_loop_new (NULL, FALSE);
    init_libpurple();
    g_io_add_watch(chan, G_IO_IN | G_IO_HUP | G_IO_ERR, chan_data, mainloop);
    g_main_loop_run(mainloop);

    g_hash_table_destroy(accounts);
}

int main(int argc, char *argv[])
{
    signal(SIGCHLD, SIG_IGN);
    erl_init(NULL, 0);
    loop();
    return 0;
}
