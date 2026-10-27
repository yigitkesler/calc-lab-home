// Yigit Kesler 251ADB030
// https://github.com/yigitkesler/calc-lab-home
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources used chat gpt :https://chatgpt.com/share/6ae115a3-8718-83eb-8b38-b1f8f639c90f
 #define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>

#define FIRST_NAME "yigit"
#define FULL_NAME "yigit_kesler"
#define STUDENT_ID "251ADB030"

typedef enum {
    TOK_EOF, TOK_NUM, TOK_PLUS, TOK_MINUS, TOK_MUL,
    TOK_DIV, TOK_POW, TOK_LPAREN, TOK_RPAREN, TOK_ERR
} TokenType;

typedef struct {
    TokenType type;
    long long ival;
    double fval;
    int is_float;
    long long pos;
} Token;

typedef struct {
    const char *src;
    long long len;
    long long idx;
} Lexer;

typedef struct {
    Lexer lexer;
    Token curr;
} Parser;

typedef struct {
    long long ival;
    double fval;
    int is_float;
} ResultVal;

static int global_error = 0;
static long long global_err_pos = 0;

void set_global_error(long long pos)
{
    if (!global_error) {
        global_error = 1;
        global_err_pos = pos;
    }
}

char lex_peek(Lexer *l)
{
    return l->idx < l->len ? l->src[l->idx] : '\0';
}

char lex_get(Lexer *l)
{
    return l->idx < l->len ? l->src[l->idx++] : '\0';
}
// The lexer keeps indices into the original file, so idx + 1 includes
// characters in previous lines and comments in every error position.
Token get_next_token(Lexer *l)
{
    while (l->idx < l->len) {
        long long pos = l->idx + 1;
        char c = lex_peek(l);

        if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
            lex_get(l);
            continue;
        }

        if (isdigit((unsigned char)c)) {
            long long start_idx = l->idx;
            int has_dot = 0;

            while (l->idx < l->len) {
                char curr = l->src[l->idx];

                if (isdigit((unsigned char)curr)) {
                    lex_get(l);
                } else if (curr == '.') {
                    if (has_dot ||
                        l->idx + 1 >= l->len ||
                        !isdigit((unsigned char)l->src[l->idx + 1])) {
                        long long dot_pos = l->idx + 1;
                        set_global_error(dot_pos);
                        return (Token){TOK_ERR, 0, 0.0, 0, dot_pos};
                    }

                    has_dot = 1;
                    lex_get(l);
                } else if (curr == 'e' || curr == 'E') {
                    set_global_error(l->idx + 1);
                    return (Token){TOK_ERR, 0, 0.0, 0, l->idx + 1};
                } else {
                    break;
                }
            }

            long long token_len = l->idx - start_idx;
            char *buf = malloc((size_t)token_len + 1);

            if (!buf) {
                fprintf(stderr, "Error: Memory allocation failed\n");
                exit(1);
            }

            memcpy(buf, l->src + start_idx, (size_t)token_len);
            buf[token_len] = '\0';

            if (has_dot) {
                double val = strtod(buf, NULL);
                free(buf);
                return (Token){TOK_NUM, 0, val, 1, start_idx + 1};
            }

            long long val = strtoll(buf, NULL, 10);
            free(buf);
            return (Token){TOK_NUM, val, (double)val, 0, start_idx + 1};
        }

        lex_get(l);

        switch (c) {
        case '+':
            return (Token){TOK_PLUS, 0, 0.0, 0, pos};
        case '-':
            return (Token){TOK_MINUS, 0, 0.0, 0, pos};
        case '*':
            if (lex_peek(l) == '*') {
                lex_get(l);
                return (Token){TOK_POW, 0, 0.0, 0, pos};
            }
            return (Token){TOK_MUL, 0, 0.0, 0, pos};
        case '/':
            return (Token){TOK_DIV, 0, 0.0, 0, pos};
        case '(':
            return (Token){TOK_LPAREN, 0, 0.0, 0, pos};
        case ')':
            return (Token){TOK_RPAREN, 0, 0.0, 0, pos};
        default:
            set_global_error(pos);
            return (Token){TOK_ERR, 0, 0.0, 0, pos};
        }
    }

    return (Token){TOK_EOF, 0, 0.0, 0, l->len + 1};
}

void parser_next(Parser *p)
{
    if (!global_error) {
        p->curr = get_next_token(&p->lexer);
    }
}

ResultVal parse_expr(Parser *p);
ResultVal parse_term(Parser *p);
ResultVal parse_power(Parser *p);
ResultVal parse_primary(Parser *p);

ResultVal parse_primary(Parser *p)
{
    if (global_error) {
        return (ResultVal){0, 0.0, 0};
    }

    if (p->curr.type == TOK_NUM) {
        ResultVal r = {
            p->curr.ival, p->curr.fval, p->curr.is_float
        };
        parser_next(p);
        return r;
    }

    if (p->curr.type == TOK_LPAREN) {
        parser_next(p);
        ResultVal r = parse_expr(p);

        if (p->curr.type == TOK_RPAREN) {
            parser_next(p);
        } else {
            set_global_error(p->curr.pos);
        }

        return r;
    }

    set_global_error(p->curr.pos);
    return (ResultVal){0, 0.0, 0};
}

long long ipow(long long base, long long exp)
{
    long long res = 1;

    while (exp > 0) {
        if (exp & 1) {
            res *= base;
        }

        exp >>= 1;

        if (exp > 0) {
            base *= base;
        }
    }

    return res;
}
// Parsing the exponent with parse_power makes ** right-associative:
// 2 ** 3 ** 2 is evaluated as 2 ** (3 ** 2).
// Integer operands keep integer arithmetic; either float promotes to double.
// Negative integer exponents are reported at the exponent's first token;
// non-finite floating results are reported at the operator.
ResultVal parse_power(Parser *p)
{
    if (global_error) {
        return (ResultVal){0, 0.0, 0};
    }

    ResultVal left = parse_primary(p);

    if (p->curr.type == TOK_POW) {
        long long op_pos = p->curr.pos;
        parser_next(p);
        long long right_token_pos = p->curr.pos;
        ResultVal right = parse_power(p);

        if (global_error) {
            return (ResultVal){0, 0.0, 0};
        }

        if (!left.is_float && !right.is_float) {
            if (right.ival < 0) {
                set_global_error(right_token_pos);
                return (ResultVal){0, 0.0, 0};
            }

            long long res = ipow(left.ival, right.ival);
            return (ResultVal){res, (double)res, 0};
        }

        double l_val = left.is_float ? left.fval : (double)left.ival;
        double r_val = right.is_float ? right.fval : (double)right.ival;
        double res = pow(l_val, r_val);

        if (!isfinite(res)) {
            set_global_error(op_pos);
            return (ResultVal){0, 0.0, 0};
        }

        return (ResultVal){0, res, 1};
    }

    return left;
}
// Terms consume powers before multiplication and division; expressions
// consume terms before addition and subtraction, preserving precedence.
// Save the right operand's first token before parsing it so division by
// zero points to that operand, including its opening parenthesis.
ResultVal parse_term(Parser *p)
{
    if (global_error) {
        return (ResultVal){0, 0.0, 0};
    }

    ResultVal left = parse_power(p);

    while (p->curr.type == TOK_MUL || p->curr.type == TOK_DIV) {
        TokenType op = p->curr.type;
        long long op_pos = p->curr.pos;
        parser_next(p);
        long long right_token_pos = p->curr.pos;
        ResultVal right = parse_power(p);

        if (global_error) {
            return (ResultVal){0, 0.0, 0};
        }

        if (op == TOK_DIV &&
            (right.is_float ? right.fval == 0.0 : right.ival == 0)) {
            set_global_error(right_token_pos);
            return (ResultVal){0, 0.0, 0};
        }

        if (left.is_float || right.is_float) {
            double l_val = left.is_float ? left.fval : (double)left.ival;
            double r_val = right.is_float ? right.fval : (double)right.ival;
            double res = op == TOK_MUL ? l_val * r_val : l_val / r_val;

            if (!isfinite(res)) {
                set_global_error(op_pos);
                return (ResultVal){0, 0.0, 0};
            }

            left = (ResultVal){0, res, 1};
        } else {
            long long res = op == TOK_MUL
                ? left.ival * right.ival
                : left.ival / right.ival;
            left = (ResultVal){res, (double)res, 0};
        }
    }

    return left;
}

ResultVal parse_expr(Parser *p)
{
    if (global_error) {
        return (ResultVal){0, 0.0, 0};
    }

    ResultVal left = parse_term(p);

    while (p->curr.type == TOK_PLUS || p->curr.type == TOK_MINUS) {
        TokenType op = p->curr.type;
        long long op_pos = p->curr.pos;
        parser_next(p);
        ResultVal right = parse_term(p);

        if (global_error) {
            return (ResultVal){0, 0.0, 0};
        }

        if (left.is_float || right.is_float) {
            double l_val = left.is_float ? left.fval : (double)left.ival;
            double r_val = right.is_float ? right.fval : (double)right.ival;
            double res = op == TOK_PLUS ? l_val + r_val : l_val - r_val;

            if (!isfinite(res)) {
                set_global_error(op_pos);
                return (ResultVal){0, 0.0, 0};
            }

            left = (ResultVal){0, res, 1};
        } else {
            long long res = op == TOK_PLUS
                ? left.ival + right.ival
                : left.ival - right.ival;
            left = (ResultVal){res, (double)res, 0};
        }
    }

    return left;
}

void create_directory_recursive(const char *path)
{
    char tmp[1024];
    snprintf(tmp, sizeof(tmp), "%s", path);
    size_t len = strlen(tmp);

    if (len == 0) {
        return;
    }

    if (tmp[len - 1] == '/') {
        tmp[len - 1] = '\0';
    }

    for (char *p = tmp + 1; *p; p++) {
        if (*p == '/') {
            *p = '\0';

            if (mkdir(tmp, 0777) != 0 && errno != EEXIST) {
                fprintf(stderr, "Error: Failed to create directory %s\n", tmp);
                exit(1);
            }

            *p = '/';
        }
    }

    if (mkdir(tmp, 0777) != 0 && errno != EEXIST) {
        fprintf(stderr, "Error: Failed to create directory %s\n", tmp);
        exit(1);
    }
}
// Each line is parsed through indices into the original buffer without
// copying the line. The line boundary also gives the error position for
// a missing operand or closing parenthesis.
// Results grow geometrically and are kept until the entire file passes,
// so a later error produces only ERROR:<pos>, without partial results.
int evaluate_file_content(
    const char *content,
    long long len,
    ResultVal **out_results,
    int *out_count,
    long long *out_err_pos)
{
    global_error = 0;
    global_err_pos = 0;

    int capacity = 100;
    int count = 0;
    ResultVal *results = malloc((size_t)capacity * sizeof(*results));

    if (!results) {
        fprintf(stderr, "Error: Memory allocation failed\n");
        exit(1);
    }

    long long idx = 0;

    while (idx < len) {
        long long line_start = idx;
        long long line_end = idx;

        while (line_end < len && content[line_end] != '\n') {
            line_end++;
        }

        long long p = line_start;

        while (p < line_end &&
               (content[p] == ' ' ||
                content[p] == '\t' ||
                content[p] == '\r')) {
            p++;
        }

        if (p == line_end || content[p] == '#') {
            idx = line_end + 1;
            continue;
        }

        Parser parser = {
            .lexer = {content, line_end, line_start},
            .curr = {TOK_EOF, 0, 0.0, 0, 0}
        };

        parser_next(&parser);
        ResultVal res = parse_expr(&parser);

        if (!global_error && parser.curr.type != TOK_EOF) {
            set_global_error(parser.curr.pos);
        }

        if (global_error) {
            *out_err_pos = global_err_pos;
            free(results);
            return 0;
        }

        if (count >= capacity) {
            capacity *= 2;
            ResultVal *temp = realloc(
                results, (size_t)capacity * sizeof(*results));

            if (!temp) {
                fprintf(stderr, "Error: Memory reallocation failed\n");
                free(results);
                exit(1);
            }

            results = temp;
        }

        results[count++] = res;
        idx = line_end + 1;
    }

    *out_results = results;
    *out_count = count;
    return 1;
}

int process_file(const char *input_path, const char *outdir)
{
    FILE *f = fopen(input_path, "rb");

    if (!f) {
        fprintf(stderr, "Error opening input file: %s\n", input_path);
        return 1;
    }

    if (fseek(f, 0, SEEK_END) != 0) {
        fprintf(stderr, "Error seeking input file\n");
        fclose(f);
        return 1;
    }

    long long fsize = ftell(f);

    if (fsize < 0 || fseek(f, 0, SEEK_SET) != 0) {
        fprintf(stderr, "Error reading input file size\n");
        fclose(f);
        return 1;
    }

    char *buf = malloc((size_t)fsize + 1);

    if (!buf) {
        fprintf(stderr, "Error: Memory allocation failed\n");
        fclose(f);
        return 1;
    }

    if (fsize > 0 &&
        (long long)fread(buf, 1, (size_t)fsize, f) != fsize) {
        fprintf(stderr, "Error reading input file\n");
        free(buf);
        fclose(f);
        return 1;
    }

    buf[fsize] = '\0';

    if (fclose(f) != 0) {
        fprintf(stderr, "Error closing input file\n");
        free(buf);
        return 1;
    }

    ResultVal *results = NULL;
    int count = 0;
    long long err_pos = 0;
    int success = evaluate_file_content(
        buf, fsize, &results, &count, &err_pos);

    const char *slash = strrchr(input_path, '/');
    const char *filename = slash ? slash + 1 : input_path;
    char base_name[256];

    if (snprintf(base_name, sizeof(base_name), "%s", filename) >=
        (int)sizeof(base_name)) {
        fprintf(stderr, "Error: File name too long\n");
        free(results);
        free(buf);
        return 1;
    }

    char *dot = strrchr(base_name, '.');

    if (dot) {
        *dot = '\0';
    }

    char outpath[1024];

    if (snprintf(outpath, sizeof(outpath), "%s/%s_%s_%s.txt",
                 outdir, base_name, FULL_NAME, STUDENT_ID) >=
        (int)sizeof(outpath)) {
        fprintf(stderr, "Error: Output file path too long\n");
        free(results);
        free(buf);
        return 1;
    }

    FILE *fo = fopen(outpath, "w");

    if (!fo) {
        fprintf(stderr, "Error opening output file: %s\n", outpath);
        free(results);
        free(buf);
        return 1;
    }

    int write_error = 0;

    if (!success) {
        write_error = fprintf(fo, "ERROR:%lld\n", err_pos) < 0;
    } else {
        for (int i = 0; i < count; i++) {
            int written;

            if (results[i].is_float) {
                written = fprintf(fo, "%.15g\n", results[i].fval);
            } else {
                written = fprintf(fo, "%lld\n", results[i].ival);
            }

            if (written < 0) {
                write_error = 1;
                break;
            }
        }
    }

    if (fclose(fo) != 0) {
        write_error = 1;
    }

    free(results);
    free(buf);

    if (write_error) {
        fprintf(stderr, "Error writing output file: %s\n", outpath);
        return 1;
    }

    return 0;
}

int main(int argc, char *argv[])
{
    const char *dir_arg = NULL;
    const char *out_arg = NULL;
    const char *input_file = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-d") == 0 ||
            strcmp(argv[i], "--dir") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "Error: Missing directory argument\n");
                return 1;
            }
            dir_arg = argv[++i];
        } else if (strcmp(argv[i], "-o") == 0 ||
                   strcmp(argv[i], "--output-dir") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "Error: Missing output directory argument\n");
                return 1;
            }
            out_arg = argv[++i];
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Error: Unknown option %s\n", argv[i]);
            return 1;
        } else {
            if (input_file) {
                fprintf(stderr, "Error: Multiple input files\n");
                return 1;
            }
            input_file = argv[i];
        }
    }

    if ((!dir_arg && !input_file) || (dir_arg && input_file)) {
        fprintf(stderr,
                "Usage: calc [-d DIR | --dir DIR] "
                "[-o OUTDIR | --output-dir OUTDIR] [input.txt]\n");
        return 1;
    }

    char base[512];
    const char *source = dir_arg ? dir_arg : input_file;

    if (snprintf(base, sizeof(base), "%s", source) >= (int)sizeof(base)) {
        fprintf(stderr, "Error: Input path too long\n");
        return 1;
    }

    size_t len = strlen(base);

    while (len > 1 && base[len - 1] == '/') {
        base[--len] = '\0';
    }

    char *slash = strrchr(base, '/');
    char *base_name = slash ? slash + 1 : base;

    if (!dir_arg) {
        char *dot = strrchr(base_name, '.');

        if (dot) {
            *dot = '\0';
        }
    }

    char outdir[1024];
    int n;

    if (out_arg) {
        n = snprintf(outdir, sizeof(outdir), "%s", out_arg);
    } else {
        n = snprintf(outdir, sizeof(outdir), "%s_%s_%s",
                     base_name, FIRST_NAME, STUDENT_ID);
    }

    if (n < 0 || n >= (int)sizeof(outdir)) {
        fprintf(stderr, "Error: Output directory path too long\n");
        return 1;
    }

    if (!dir_arg) {
        create_directory_recursive(outdir);
        return process_file(input_file, outdir);
    }

    DIR *d = opendir(dir_arg);

    if (!d) {
        fprintf(stderr, "Error opening directory: %s\n", dir_arg);
        return 1;
    }

    create_directory_recursive(outdir);

    struct dirent *entry;
    int status = 0;

    while ((entry = readdir(d)) != NULL) {
        const char *ext = strrchr(entry->d_name, '.');

        if (!ext || strcmp(ext, ".txt") != 0) {
            continue;
        }

        char inpath[1024];

        if (snprintf(inpath, sizeof(inpath), "%s/%s",
                     dir_arg, entry->d_name) >= (int)sizeof(inpath)) {
            fprintf(stderr, "Error: Input file path too long\n");
            status = 1;
            break;
        }

        struct stat st;

        if (stat(inpath, &st) != 0) {
            fprintf(stderr, "Error inspecting input file: %s\n", inpath);
            status = 1;
            break;
        }

        if (!S_ISREG(st.st_mode)) {
            continue;
        }

        if (process_file(inpath, outdir) != 0) {
            status = 1;
            break;
        }
    }

    if (closedir(d) != 0) {
        fprintf(stderr, "Error closing directory\n");
        status = 1;
    }

    return status;
}