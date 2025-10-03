// my_tui_nif.c
#include <erl_nif.h>
#include <termios.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

// Terminal handling functions
static struct termios original_term;

static void set_raw_mode();
static void restore_term();
static int read_key();

/*
void set_raw_mode() {
    struct termios term;
    tcgetattr(STDIN_FILENO, &original_term); // Save original settings
    term = original_term;
    term.c_lflag &= ~(ICANON | ECHO | ISIG); // Disable canonical mode, echo, signals
    term.c_iflag &= ~(IXON | ICRNL); // Disable flow control, CR-to-NL mapping
    term.c_oflag &= ~(OPOST); // Disable output processing
    term.c_cc[VMIN] = 1; // Read at least 1 byte
    term.c_cc[VTIME] = 0; // No timeout
    tcsetattr(STDIN_FILENO, TCSANOW, &term);
}
*/

// Set up PTY line discipline with termios
static void set_raw_mode() {
    struct termios term; // man 4 termios
    tcgetattr(STDIN_FILENO, &original_term); // Save original settings
    term = original_term;
    cfmakeraw(&term); // Set full raw mode (disables ICANON, ECHO, ISIG, IEXTEN, etc.)
    term.c_cc[VMIN] = 0; // don't wait infinitely for the first byte
    term.c_cc[VTIME] = 0; // time out immediately
    if (tcsetattr(STDIN_FILENO, TCSANOW, &term) == -1) {
        // Log error if needed
        fprintf(stderr, "tcsetattr failed: %s\n", strerror(errno));
    }
}

static void restore_term() {
    tcsetattr(STDIN_FILENO, TCSANOW, &original_term); // Restore original settings
}

static int read_key() {
    unsigned char buf = 0;
    ssize_t n = read(STDIN_FILENO, &buf, 1);
    if (n == 1) return buf;
    return -1; // Error or no character read
}

// NIF functions
static ERL_NIF_TERM set_raw_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    set_raw_mode();
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM read_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int key = read_key();
    if (key >= 0) return enif_make_int(env, key);
    return enif_make_atom(env, "no_data");
}

static ERL_NIF_TERM restore_term_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    restore_term();
    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
    {"set_raw_mode", 0, set_raw_nif},
    {"read_key", 0, read_key_nif},
    {"restore_term", 0, restore_term_nif}
};

ERL_NIF_INIT(cs_io_nif, nif_funcs, NULL, NULL, NULL, NULL)
